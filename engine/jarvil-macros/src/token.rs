extern crate proc_macro;
use proc_macro::*;
use quote::quote;
use syn::{
    punctuated::Punctuated,
    token::{Colon2, Comma},
    Expr, ExprMacro, FnArg, GenericArgument, PathArguments, PathSegment, Stmt, Token, Type,
};

pub fn impl_tokenify_macro(ast: &syn::DeriveInput) -> TokenStream {
    let enum_data = match &ast.data {
        syn::Data::Enum(enum_data) => enum_data,
        _ => panic!("tokenify macro should only be used for `CoreToken` enum")
    };
    todo!()
}