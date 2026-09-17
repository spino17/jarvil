// Type-checker corpus.
//
// The full analysis pipeline, stopping before code generation. This is where
// the language's most intricate machinery lives -- generic inference, interface
// bounds, enum exhaustiveness -- so it deserves the densest coverage.

mod common;

use common::type_check_to_diagnostics;
use std::fs;

#[test]
fn reports_type_errors() {
    insta::glob!("corpus/type_checker/err/*.jv", |path| {
        let source = fs::read_to_string(path).unwrap();

        insta::assert_snapshot!(type_check_to_diagnostics(&source));
    });
}
