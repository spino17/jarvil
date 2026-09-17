// Parser corpus.
//
// `ok/` holds programs that must parse cleanly; the snapshot pins the shape of
// the tree. `err/` holds malformed programs, where the snapshot pins how panic
// mode recovers -- which tokens get skipped, where `MissingToken` placeholders
// land, and how much of the tree survives. Those recovery paths are the ones an
// editor hits constantly against half-typed code, so they are worth as much
// coverage as the happy path.

mod common;

use common::parse_to_ast;
use std::fs;

#[test]
fn parses_valid_programs() {
    insta::glob!("corpus/parser/ok/*.jv", |path| {
        let source = fs::read_to_string(path).unwrap();

        insta::assert_snapshot!(parse_to_ast(&source));
    });
}

#[test]
fn recovers_from_malformed_programs() {
    insta::glob!("corpus/parser/err/*.jv", |path| {
        let source = fs::read_to_string(path).unwrap();

        insta::assert_snapshot!(parse_to_ast(&source));
    });
}
