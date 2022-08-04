extern crate proc_macro;
use proc_macro::*;
use quote::{quote};
use syn::{FnArg, Type, PathArguments, PathSegment};

// This method is taken from Tokio-macros
fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}

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

enum NodeTypeKind {
    PURE,   // BlockNode
    OPTION  // Option<BlockNode>
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
                // TODO - check path.arguments for the subtype inside Option<...> and check whether it's a node (use has_node_suffix)
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
                // TODO - check path.arguments for the subtype inside Option<...> and check whether it's a node (use has_node_suffix)
                println!("yayasydfhgasdkhsdhsdahsaksa");
                true
            } else {
                false
            }
        },
        None => false
    }
}

fn is_node_or_optional_type(type_arg: &Box<Type>) -> bool {
    todo!()
}

fn impl_set_parent_macro(args_ast: &syn::Ident, ast: &syn::ItemFn) -> TokenStream {
    let attrs = &ast.attrs;
    let vis = &ast.vis;
    let sig = &ast.sig;
    let block = &ast.block;
    let stmts = &block.stmts;

    let args = &sig.inputs;
    let mut args_iter = args.iter();
    let mut args_vec = vec![];
    while let Some(arg) = args_iter.next() {
        let pat_type = match arg {
            FnArg::Receiver(_) => panic!("macro should only be used for classmethods"),
            FnArg::Typed(pat_type) => {
                args_vec.push((pat_type.pat.clone(), pat_type.ty.clone()));
            }
        };
    }
    let arg_1_name = &args_vec[0].0;
    let arg_2_name = &args_vec[1].0;
    let arg_1_type = &args_vec[0].1;
    let arg_2_type = &args_vec[1].1;
    let m = is_option_node_type(arg_2_type);
    let n = is_node_type(arg_1_type);
    // syn::Ident::new("NotOption", quote::__private::Span::call_site())
    let first_stmt = &stmts[0];
    let remaining_stmt = &stmts[1..];
    let gen = quote! {
        #(#attrs)* #vis #sig {
            // print_args!((#arg_1_name, #arg_2_name));
            // print_optional!(#arg_2_name);
            println!("bool is: {}-{}", #n, #m);
            println!("{}", stringify!(#args_ast));
            // #(#stmts)*
            #first_stmt
            println!("{}", node);
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