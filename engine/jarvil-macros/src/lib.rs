extern crate proc_macro;
use proc_macro::*;
use quote::{quote};
use syn::{FnArg, Type, PathArguments};

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

fn is_option_on_node(arg: &Box<Type>) -> Option<syn::Ident> {
    let s = match &*arg.as_ref() {
        Type::Path(path_type) => {
            let mut path = path_type.path.segments.iter();
            match path.next() {
                Some(path) => {
                    // TODO - check path.arguments for the subtype inside Option<...> and check whether it's a node
                    let ident = &path.ident;
                    if ident.to_string().eq("Option") {
                        println!("yayasydfhgasdkhsdhsdahsaksa");
                        Some(path.ident.clone())
                    } else {
                        None
                    }
                }
                None => None,
            }
        },
        _ => None
    };
    s
}

fn impl_set_parent_macro(args_ast: &syn::Ident, ast: &syn::ItemFn) -> TokenStream {
    /*
    let arg1 = &ast.sig.ident;
    let args = &ast.sig.inputs;
    let mut args_iter = args.iter();
    let mut args_vec = vec![];
    while let Some(arg) = args_iter.next() {
        let pat_type = match arg {
            FnArg::Receiver(_) => panic!("macro should only be used for classmethods"),
            FnArg::Typed(pat_type) => {
                args_vec.push(pat_type.pat.clone());
            }
        };
    }
    let arg_1 = &args_vec[0];
    let arg_2 = &args_vec[1];
    let gen = quote! {
        fn #arg1(#args) {
            // print_args!(#arg_1, #arg_2);
            print_args!((#arg_1, #arg_2));
            println!("args of the function `{}`: {}", stringify!(#arg1), stringify!(#args));
        }
    };
     */
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
    let s = match &*arg_2_type.as_ref() {
        Type::Path(path_type) => {
            let mut path = path_type.path.segments.iter();
            match path.next() {
                Some(path) => {
                    // Some(path.arguments.clone())
                    let ident = &path.ident;
                    if ident.to_string().eq("Option") {
                        println!("yayasydfhgasdkhsdhsdahsaksa");
                        Some(path.ident.clone())
                    } else {
                        None
                    }
                }
                None => None,
            }
        },
        _ => None
    };
    // syn::Ident::new("NotOption", quote::__private::Span::call_site())
    let first_stmt = &stmts[0];
    let remaining_stmt = &stmts[1..];
    let gen = quote! {
        #(#attrs)* #vis #sig {
            // print_args!((#arg_1_name, #arg_2_name));
            // print_optional!(#arg_2_name);
            println!("{}", stringify!(#args_ast));
            println!("{}", stringify!(#s));
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