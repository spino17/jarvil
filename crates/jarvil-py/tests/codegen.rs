// Transpiler corpus.
//
// Two assertions per program, and they do different jobs:
//
//   - the snapshot pins the emitted Python, so a diff shows exactly *what*
//     changed about code generation
//   - executing that Python and checking its output proves the program still
//     *means* the same thing, which a snapshot alone cannot tell you
//
// The execution half needs an interpreter, so it is skipped rather than failed
// when `python3` is absent -- a missing Python is not a compiler regression.

mod common;

use common::{python3_available, run_python, transpile};
use std::fs;

#[test]
fn transpiles_to_expected_python() {
    insta::glob!("corpus/codegen/*.jv", |path| {
        let source = fs::read_to_string(path).unwrap();

        match transpile(&source) {
            Ok(py_code) => insta::assert_snapshot!(py_code),
            Err(diagnostics) => panic!(
                "expected {} to compile cleanly, but analysis reported:\n{}",
                path.display(),
                diagnostics
            ),
        }
    });
}

#[test]
fn generated_python_produces_expected_output() {
    if !python3_available() {
        eprintln!("skipping: `python3` not found on PATH");

        return;
    }

    insta::glob!("corpus/codegen/*.jv", |path| {
        let source = fs::read_to_string(path).unwrap();

        let py_code = match transpile(&source) {
            Ok(py_code) => py_code,
            Err(diagnostics) => panic!(
                "expected {} to compile cleanly, but analysis reported:\n{}",
                path.display(),
                diagnostics
            ),
        };

        insta::assert_snapshot!(run_python(&py_code));
    });
}
