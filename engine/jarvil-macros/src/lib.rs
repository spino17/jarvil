extern crate proc_macro;
mod ast;
mod token;
mod opcode;
use crate::ast::{node::impl_weak_nodes_macro};
use crate::token::impl_tokenify_macro;
use crate::ast::node::impl_node_trait;
use opcode::impl_opcode_util_macro;
use proc_macro::*;
use std::str::FromStr;
use syn::{
    punctuated::Punctuated,
    token::Colon2,
    Expr, ExprMacro, PathArguments, PathSegment, Stmt, Token,
};

// This method is taken from Tokio-macros
fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}

fn get_macro_expr_stmt(macro_name: &str, macro_expr_str: &str) -> Stmt {
    let mut punc: Punctuated<PathSegment, Colon2> = Punctuated::new();
    punc.push(syn::PathSegment {
        ident: syn::Ident::new(macro_name, quote::__private::Span::call_site()),
        arguments: PathArguments::None,
    });
    let token_stream = match proc_macro2::TokenStream::from_str(macro_expr_str) {
        Ok(token_stream) => token_stream,
        Err(e) => unreachable!("string should be lexically correct: {:?}", e),
    };
    let expr_macro = ExprMacro {
        attrs: vec![],
        mac: syn::Macro {
            path: syn::Path {
                leading_colon: None,
                segments: punc,
            },
            bang_token: Token![!](quote::__private::Span::call_site()),
            delimiter: syn::MacroDelimiter::Paren(syn::token::Paren {
                span: quote::__private::Span::call_site(),
            }),
            tokens: token_stream,
        },
    };
    let set_parents_macro_call_stmt = Stmt::Expr(Expr::Macro(expr_macro));
    set_parents_macro_call_stmt
}

#[proc_macro_derive(Nodify)]
pub fn weak_nodes_macro_derive(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };
    impl_weak_nodes_macro(&input_ast)
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
pub fn node(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };
    impl_node_trait(&input_ast)
}

#[proc_macro_derive(OpCodeUtil)]
pub fn opcode_util_macro_derive(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };
    impl_opcode_util_macro(&input_ast)
}