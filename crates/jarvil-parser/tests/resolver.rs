// Name-resolution corpus.
//
// Runs lexing, parsing and resolution but stops short of type checking, so a
// failure here points at the resolver rather than at whatever the type checker
// made of an already-broken tree.

mod common;

use common::resolve_to_diagnostics;
use std::fs;

#[test]
fn reports_resolution_errors() {
    insta::glob!("corpus/resolver/err/*.jv", |path| {
        let source = fs::read_to_string(path).unwrap();

        insta::assert_snapshot!(resolve_to_diagnostics(&source));
    });
}
