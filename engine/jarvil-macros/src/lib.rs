extern crate proc_macro;
use std::{str::FromStr};
use proc_macro::*;
use quote::{quote};
use syn::{FnArg, Type, PathArguments, PathSegment, Stmt, Expr, ExprMacro, punctuated::Punctuated, token::Colon2, Token};

fn has_node_suffix(word: &str) -> bool {
    let str_len = word.len();
    if str_len < 4 {
        return false
    }
    if word[(str_len - 4)..str_len].eq("Node") {
        return true
    } else {
        return false
    }
}

// This method is taken from Tokio-macros
fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}

enum NodeTypeKind {
    PURE,   // BlockNode
    OPTION, // Option<BlockNode>
    NONE    // BinaryOperatorKind
}

fn path_segment_from_type(type_arg: &Box<Type>) -> Option<&PathSegment> {
    let s = match &*type_arg.as_ref() {
        Type::Path(path_type) => {
            let mut path = path_type.path.segments.iter();
            match path.next() {
                Some(path) => {
                    Some(path)
                }
                None => None,
            }
        },
        _ => None
    };
    s
}

fn is_node_type(type_arg: &Box<Type>) -> bool {
    match path_segment_from_type(type_arg) {
        Some(path) => {
            let ident = &path.ident;
            if has_node_suffix(&ident.to_string()) {
                true
            } else {
                false
            }
        },
        None => false
    }
}

fn is_option_node_type(type_arg: &Box<Type>) -> bool {
    match path_segment_from_type(type_arg) {
        Some(path) => {
            let ident = &path.ident;
            if ident.to_string().eq("Option") {
                // TODO - check path.arguments for the subtype inside Option<...> and check whether it's a node (use is_node_type)
                true
            } else {
                false
            }
        },
        None => false
    }
}

fn is_node_or_optional_type(type_arg: &Box<Type>) -> NodeTypeKind {
    if is_node_type(type_arg) {
        return NodeTypeKind::PURE
    } else if is_option_node_type(type_arg) {
        return NodeTypeKind::OPTION
    } else {
        return NodeTypeKind::NONE
    }
}

fn get_macro_expr(macro_name: &str, macro_expr_str: &str) -> Stmt {
    let mut punc: Punctuated<PathSegment, Colon2> = Punctuated::new();
    punc.push(syn::PathSegment{
        ident: syn::Ident::new(macro_name, quote::__private::Span::call_site()),
        arguments: PathArguments::None,
    });
    let token_stream =  match proc_macro2::TokenStream::from_str(macro_expr_str) {
        Ok(token_stream) => token_stream,
        Err(e) => unreachable!("string should be lexically correct: {:?}", e),
    };
    let expr_macro = ExprMacro{
        attrs: vec![],
        mac: syn::Macro{
            path: syn::Path{
                leading_colon: None,
                segments: punc,
            },
            bang_token: Token![!](quote::__private::Span::call_site()),
            delimiter: syn::MacroDelimiter::Paren(syn::token::Paren{
                span: quote::__private::Span::call_site(),
            }),
            tokens: token_stream,
        },
    };
    let set_parents_macro_call_stmt = Stmt::Expr(Expr::Macro(expr_macro));
    set_parents_macro_call_stmt
}

fn get_set_parents_macro_expr(idents: &Vec<proc_macro2::Ident>) -> Stmt {
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
    // TODO - add for ASTNode and node arguments
    get_macro_expr("print_args", &arg_str)
}

fn get_set_parents_optional_macro_expr(idents: &Vec<proc_macro2::Ident>) -> Stmt {
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
    // TODO - add for ASTNode and node arguments
    get_macro_expr("print_args_optional", &arg_str)
}

fn impl_set_parent_macro(args_ast: &syn::Ident, ast: &syn::ItemFn) -> TokenStream {
    // ast nodes for the original function
    let attrs = &ast.attrs;
    let vis = &ast.vis;
    let sig = &ast.sig;
    let block = &ast.block;
    let stmts = &block.stmts;

    let mut args_iter = sig.inputs.iter();
    let mut node_args = vec![];
    let mut optional_node_args = vec![];
    while let Some(arg) = args_iter.next() {
        match arg {
            FnArg::Receiver(_) => panic!("macro should only be used for classmethods"),
            FnArg::Typed(pat_type) => {
                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        match is_node_or_optional_type(&pat_type.ty) {
                            // directly make the stmt here with just the ref of pat without cloning it
                            NodeTypeKind::PURE => node_args.push(pat_ident.ident.clone()),
                            NodeTypeKind::OPTION => optional_node_args.push(pat_ident.ident.clone()),
                            _ => continue
                        }
                    },
                    _ => continue
                }
            }
        }
    }
    let set_parents_macro_stmt = get_set_parents_macro_expr(&node_args);
    let set_parents_optiona_macro_stmt = get_set_parents_optional_macro_expr(&optional_node_args);
    let first_stmt = &stmts[0];
    let remaining_stmt = &stmts[1..];
    let gen = quote! {
        #(#attrs)* #vis #sig {
            // print_args!((#arg_1_name, #arg_2_name));
            // print_optional_args!((#arg_2));
            // println!("bool is: {}-{}", #n, #m);
            // println!("yo baby: {}", stringify!(#arg_1));
            // println!("yo baby: {}", stringify!(#arg_2));
            // print_args!((#args_ast));
            // #(#stmts)*
            #first_stmt
            // println!("{}", node);
            // #set_parents_macro_call_stmt;
            #set_parents_macro_stmt;
            #set_parents_optiona_macro_stmt;
            #(#remaining_stmt)*
        }
    };
    gen.into()
}

#[proc_macro_attribute]
pub fn set_parent(args: TokenStream, input: TokenStream) -> TokenStream {
    let input_ast: syn::ItemFn = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };
    let args_ast: syn::Ident = match syn::parse(args.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(args, e),
    };
    impl_set_parent_macro(&args_ast, &input_ast)
}