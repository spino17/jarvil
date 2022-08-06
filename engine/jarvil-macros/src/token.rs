extern crate proc_macro;
use proc_macro::*;
use quote::quote;
use std::str::FromStr;
use syn::{
    punctuated::Punctuated,
    token::{Colon2, Comma},
    Expr, ExprMacro, FnArg, GenericArgument, PathArguments, PathSegment, Stmt, Token, Type,
};

pub fn impl_tokenify_macro(ast: &syn::DeriveInput) -> TokenStream {
    todo!()
}