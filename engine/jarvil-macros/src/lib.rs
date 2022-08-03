extern crate proc_macro;
use proc_macro::*;
use quote::{quote};
use syn::{FnArg, Type};

// This method is taken from Tokio-macros
fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}

fn impl_set_parent_macro(ast: &syn::ItemFn) -> TokenStream {
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
    let s = match &*arg_1_type.as_ref() {
        Type::Path(path_type) => {
            let mut path = path_type.path.segments.iter();
            match path.next() {
                Some(path) => {
                    // Some(path.arguments.clone())
                    let ident = &path.ident;
                    if ident.to_string().eq("Option") {
                        println!("yayasydfhgasdkhsdhsdahsaksa");
                        path.ident.clone()
                    } else {
                        syn::Ident::new("NotOption", quote::__private::Span::call_site())
                    }
                }
                None => syn::Ident::new("NotOption", quote::__private::Span::call_site()),
            }
        },
        _ => syn::Ident::new("NotOption", quote::__private::Span::call_site())
    };

    let gen = quote! {
        #(#attrs)* #vis #sig {
            // print_args!((#arg_1_name, #arg_2_name));
            // print_optional!(#arg_2_name);
            println!("{}", stringify!(#s));
            #(#stmts)*
            println!("{}", node);
        }
    };
    gen.into()
}

#[proc_macro_attribute]
pub fn set_parent(args: TokenStream, input: TokenStream) -> TokenStream {
    let ast: syn::ItemFn = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };
    impl_set_parent_macro(&ast)
}