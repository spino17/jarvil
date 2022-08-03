extern crate proc_macro;
use proc_macro::*;
use quote::quote;
use syn::{FnArg};

// This method is taken from Tokio-macros
fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}

fn impl_set_parent_macro(ast: &syn::ItemFn) -> TokenStream {
    let arg1 = &ast.sig.ident;
    let args = &ast.sig.inputs;
    let mut args_iter = args.iter();
    let mut args_vec = vec![];
    while let Some(arg) = args_iter.next() {
        let pat_type = match arg {
            FnArg::Receiver(_) => panic!("macro should be used for only classmethods"),
            FnArg::Typed(pat_type) => {
                args_vec.push(pat_type.pat.clone());
            }
        };
    }
    let arg_1 = &args_vec[0];
    let arg_2 = &args_vec[1];
    let gen = quote! {
        macro_rules! print_args {
            (($($t: ident),*)) => {
                $(
                    println!("{}", stringify!($t));
                )*
            };
        }

        fn dummy() {
            // print_args!(#arg_1, #arg_2);
            print_args!((#arg_1, #arg_2));
            println!("args of the function `{}`: {}", stringify!(#arg1), stringify!(#args));
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