extern crate proc_macro;
mod helper;
mod node;
mod token;
use crate::helper::token_stream_with_error;
use crate::node::impl_node_macro;
use crate::node::impl_nodify_macro;
use crate::token::impl_tokenify_macro;
use proc_macro::*;

#[proc_macro_derive(Nodify)]
pub fn nodify_macro_derive(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };
    impl_nodify_macro(&input_ast)
}

#[proc_macro_derive(Tokenify)]
pub fn tokenify_macro_derive(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };
    impl_tokenify_macro(&input_ast)
}

#[proc_macro_derive(Node)]
pub fn node_macro_derive(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };
    impl_node_macro(&input_ast)
}
