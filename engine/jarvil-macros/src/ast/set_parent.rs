extern crate proc_macro;
use proc_macro::*;
use quote::quote;
use syn::{
    token::Comma, FnArg, GenericArgument, PathArguments, PathSegment, Stmt, Type,
};
use crate::get_macro_expr_stmt;

fn has_node_suffix(word: &str) -> bool {
    let str_len = word.len();
    if str_len < 4 {
        return false;
    }
    if word[(str_len - 4)..str_len].eq("Node") {
        return true;
    } else {
        return false;
    }
}

enum NodeTypeKind {
    PURE,   // BlockNode
    OPTION, // Option<BlockNode>
    NONE,   // BinaryOperatorKind
}

fn path_segment_from_type(type_arg: &Type) -> Option<&PathSegment> {
    let s = match type_arg {
        Type::Path(path_type) => {
            let mut path = path_type.path.segments.iter();
            match path.next() {
                Some(path) => Some(path),
                None => None,
            }
        }
        _ => None,
    };
    s
}

fn is_node_type(type_arg: &Type) -> bool {
    match type_arg {
        Type::Reference(ref_type) => {
            let ref_type = &ref_type.elem;
            match path_segment_from_type(ref_type) {
                Some(path) => {
                    let ident = &path.ident;
                    if has_node_suffix(&ident.to_string()) {
                        true
                    } else {
                        false
                    }
                }
                None => false,
            }
        }
        _ => false,
    }
}

fn is_option_node_type(type_arg: &Type) -> bool {
    match path_segment_from_type(type_arg) {
        Some(path) => {
            let ident = &path.ident;
            let arguments = &path.arguments;
            if ident.to_string().eq("Option") {
                match arguments {
                    PathArguments::AngleBracketed(args) => {
                        match args.args.iter().next() {
                            Some(arg) => match arg {
                                GenericArgument::Type(sub_type) => return is_node_type(sub_type),
                                _ => return false,
                            },
                            None => unreachable!("Option<...> always have a subtype"),
                        };
                    }
                    _ => false,
                }
            } else {
                false
            }
        }
        None => false,
    }
}

fn is_node_or_optional_type(type_arg: &Type) -> NodeTypeKind {
    if is_node_type(type_arg) {
        return NodeTypeKind::PURE;
    } else if is_option_node_type(type_arg) {
        return NodeTypeKind::OPTION;
    } else {
        return NodeTypeKind::NONE;
    }
}

fn get_node_args(
    args: &syn::punctuated::Punctuated<FnArg, Comma>,
) -> (Vec<proc_macro2::Ident>, Vec<proc_macro2::Ident>) {
    let mut args_iter = args.iter();
    let mut node_args = vec![];
    let mut optional_node_args = vec![];
    while let Some(arg) = args_iter.next() {
        match arg {
            FnArg::Receiver(_) => continue,
            FnArg::Typed(pat_type) => {
                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        match is_node_or_optional_type(&pat_type.ty) {
                            // TODO - directly make the stmt here with just the ref of pat without cloning it
                            NodeTypeKind::PURE => node_args.push(pat_ident.ident.clone()),
                            NodeTypeKind::OPTION => {
                                optional_node_args.push(pat_ident.ident.clone())
                            }
                            _ => continue,
                        }
                    }
                    _ => continue,
                }
            }
        }
    }
    (node_args, optional_node_args)
}

fn get_set_parents_macro_expr(
    macro_name: &str,
    idents: &Vec<proc_macro2::Ident>,
    parent_node: &syn::Ident,
    weak_node: &syn::Ident,
) -> Stmt {
    let mut arg_str = "(".to_string();
    let mut flag = false;
    for ident in idents {
        if flag {
            arg_str.push_str(", ");
        }
        arg_str.push_str(&ident.to_string());
        flag = true;
    }
    arg_str.push(')');
    arg_str.push_str(&format!(", {}, node, {}", parent_node.to_string(), weak_node.to_string()));
    get_macro_expr_stmt(macro_name, &arg_str)
}

pub fn impl_set_parent_macro(parent_node: &syn::Ident, weak_node: &syn::Ident, ast: &syn::ItemFn) -> TokenStream {
    // ast nodes for the original function
    let attrs = &ast.attrs;
    let vis = &ast.vis;
    let sig = &ast.sig;
    let block = &ast.block;
    let stmts = &block.stmts;

    let (node_args, optional_node_args) = get_node_args(&sig.inputs);
    let set_parents_macro_stmt = get_set_parents_macro_expr("impl_set_parents", &node_args, parent_node, weak_node);
    let set_parents_optional_macro_stmt =
        get_set_parents_macro_expr("impl_set_parents_optional", &optional_node_args, parent_node, weak_node);
    let first_stmt = &stmts[0]; // TODO - check this first statement is let node = ...
    let remaining_stmt = &stmts[1..];
    let gen = quote! {
        #(#attrs)* #vis #sig {
            #first_stmt

            #set_parents_macro_stmt;
            #set_parents_optional_macro_stmt;

            #(#remaining_stmt)*
        }
    };
    gen.into()
}
