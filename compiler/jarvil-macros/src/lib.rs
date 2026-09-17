//! Derive macros that generate the Jarvil syntax tree's boilerplate.
//!
//! The AST has around a hundred node types, each needing the same handful of
//! impls. Writing those by hand would be thousands of lines that drift out of
//! sync the first time a variant is added, so they are derived from the enums
//! that already list every node and token.
//!
//! | Macro | Applies to | Generates |
//! |-------|-----------|-----------|
//! | [`Nodify`] | `ASTNode` | a constructor per variant, `new_with_<Variant>` |
//! | [`Node`] | node enums | `core_ref` accessors over the shared `Arc` |
//! | [`Tokenify`] | `CoreToken` | a predicate per token kind, plus `is_eq` and `to_string` |
//!
//! Each macro is deliberately specific to the type it is written for and will
//! panic if applied elsewhere: they encode assumptions about those enums rather
//! than being general-purpose.

extern crate proc_macro;
mod helper;
mod node;
mod token;
use crate::helper::token_stream_with_error;
use crate::node::impl_node_macro;
use crate::node::impl_nodify_macro;
use crate::token::impl_tokenify_macro;
use proc_macro::*;

/// Generates a `new_with_<Variant>` constructor for each `ASTNode` variant.
///
/// # Panics
///
/// At expansion time if applied to anything other than the `ASTNode` enum.
#[proc_macro_derive(Nodify)]
pub fn nodify_macro_derive(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };

    impl_nodify_macro(&input_ast)
}

/// Generates a predicate per `CoreToken` variant, plus `is_eq` and `to_string`.
///
/// The predicates are named after their variants and so are SCREAMING_CASE,
/// which is why `lexer::token` disables the naming lints.
///
/// # Panics
///
/// At expansion time if applied to anything other than the `CoreToken` enum.
#[proc_macro_derive(Tokenify)]
pub fn tokenify_macro_derive(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };

    impl_tokenify_macro(&input_ast)
}

/// Generates `core_ref` accessors for a node enum's variants.
///
/// # Panics
///
/// At expansion time if applied to a type that is not shaped like a node enum.
#[proc_macro_derive(Node)]
pub fn node_macro_derive(input: TokenStream) -> TokenStream {
    let input_ast: syn::DeriveInput = match syn::parse(input.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(input, e),
    };

    impl_node_macro(&input_ast)
}
