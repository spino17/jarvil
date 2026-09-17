//! The Python backend: turns a checked Jarvil program into Python source.
//!
//! Analysis lives in [`jarvil_parser`]; this crate is only the last pass, plus
//! the orchestration that runs the two together. The split is what lets a tool
//! that only needs to *understand* Jarvil -- the language server, say -- depend
//! on the front end without pulling in a code generator it will never call.
//!
//! # Emitting Python
//!
//! [`build_code`] is the whole pipeline: source in, Python out.
//!
//! ```no_run
//! use jarvil_parser::code::JarvilCode;
//! use jarvil_py::build_code;
//!
//! let (result, _ast_json) = build_code(JarvilCode::new("def main():\n    print(1)\n"));
//!
//! match result {
//!     Ok(python) => println!("{python}"),
//!     Err(report) => eprintln!("{report:?}"),
//! }
//! ```
//!
//! [`PythonCodeGenerator`] is the pass itself, for a caller that has already
//! run analysis and wants only the final step.
//!
//! # What the translation preserves
//!
//! Output is a direct translation rather than a lowering: statements map to
//! statements, and the generated file has the same shape as the source.
//! Constructs with no Python equivalent -- enums, `match` -- become the obvious
//! encoding, and generics are erased, since one Python function body serves
//! every instantiation.
//!
//! Comments do **not** survive. They reach code generation as trivia, but each
//! is replaced by a newline, so the generated file keeps the source's vertical
//! spacing while losing the prose. Carrying them across would be a worthwhile
//! improvement, since the output is meant to be read.
//!
//! [`PythonCodeGenerator`]: python::PythonCodeGenerator

#![forbid(unsafe_code)]

pub mod helper;
pub mod python;

use jarvil_parser::ast::print::serialize_ast;
use jarvil_parser::build_ast;
use jarvil_parser::code::JarvilCode;
use jarvil_parser::error::error::JarvilProgramAnalysisErrors;
use jarvil_parser::parser::resolver::JarvilResolver;
use jarvil_parser::parser::type_checker::JarvilTypeChecker;
use miette::Report;
use python::PythonCodeGenerator;

/// Attaches source text to a report so miette can render the offending span.
///
/// Diagnostics carry byte offsets, not the text they point into, so the source
/// has to be supplied before a report can be printed.
fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);

    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => err,
        None => unreachable!(),
    }
}

/// Compiles Jarvil source to Python, running the whole pipeline.
///
/// Returns the generated Python alongside a JSON rendering of the syntax tree.
/// The JSON is produced whether or not compilation succeeded, which is why it
/// sits outside the `Result`: `jarvil` writes it next to the source as a
/// debugging aid even for a program that failed to compile.
///
/// # Errors
///
/// Returns the **first** diagnostic any pass reported, with the source attached
/// so it renders with a code frame. Later diagnostics are discarded; a caller
/// that wants all of them -- an editor, say -- should use
/// [`jarvil_parser::analysis::with_analysis`] instead.
///
/// # Panics
///
/// Panics if the syntax tree cannot be serialised to JSON, which would mean a
/// bug in the AST's `Serialize` implementations rather than anything about the
/// program being compiled.
pub fn build_code(code: JarvilCode) -> (Result<String, Report>, String) {
    let errors = JarvilProgramAnalysisErrors::default();
    let (ast, code_handler) = build_ast(&code, &errors);

    // name resolution
    let resolver = JarvilResolver::new(&code_handler, &errors);
    let semantic_db = resolver.resolve_ast(&ast);

    // type checking
    let type_checker = JarvilTypeChecker::new(&code_handler, &errors, semantic_db);
    let modified_semantic_db = type_checker.check_ast(&ast);

    // ast json serialization
    let ast_str = serialize_ast(&ast, &code_handler).unwrap();

    if let Some(report) = errors.first_error_report() {
        return (Err(attach_source_code(report, code.to_string())), ast_str);
    }

    // Python code-generation
    let py_generator = PythonCodeGenerator::new(&code_handler, modified_semantic_db);
    let py_code = py_generator.generate_python_code(&ast);

    (Ok(py_code), ast_str)
}
