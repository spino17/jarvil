extern crate proc_macro;
mod ast;
use crate::ast::impl_set_parent_macro;
use proc_macro::*;

// This method is taken from Tokio-macros
fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
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
