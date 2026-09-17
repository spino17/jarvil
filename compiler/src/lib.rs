//! The Jarvil compiler: a statically typed language that transpiles to Python.
//!
//! The point of the language is to catch at compile time the class of bug that
//! Python only reports at runtime, while still producing ordinary Python that
//! runs anywhere Python does.
//!
//! # The pipeline
//!
//! Compilation is five passes over one file, each depending on the last:
//!
//! | Pass | Module | Produces |
//! |------|--------|----------|
//! | Lexing | [`lexer`] | a token stream, with comments and whitespace kept as trivia |
//! | Parsing | [`parser::parser`] | a [`BlockNode`] syntax tree, recovering from errors rather than stopping |
//! | Name resolution | [`parser::resolver`] | a [`SemanticStateDatabase`] binding every identifier to a symbol |
//! | Type checking | [`parser::type_checker`] | the same database, with types inferred and checked |
//! | Code generation | [`codegen::python`] | Python source |
//!
//! Diagnostics from every pass accumulate in one
//! [`JarvilProgramAnalysisErrors`], so a failed pass does not stop the next one
//! from running -- which is what lets a single compile report several errors,
//! and lets an editor answer questions about code that does not compile.
//!
//! # Two entry points
//!
//! [`build_code`] is the batch compiler, shaped for the CLI: source in, Python
//! out, first error on failure.
//!
//! ```no_run
//! use compiler::{build_code, code::JarvilCode};
//!
//! let (result, _ast_json) = build_code(JarvilCode::new("def main():\n    print(1)\n"));
//!
//! match result {
//!     Ok(python) => println!("{python}"),
//!     Err(report) => eprintln!("{report:?}"),
//! }
//! ```
//!
//! [`analysis::with_analysis`] is the query interface, shaped for editors. It
//! stops before code generation and hands back the syntax tree, the semantic
//! database and *every* diagnostic, so tools can ask questions of a program
//! that does not compile. [`queries`] builds go-to-definition and hover on top
//! of it.
//!
//! ```
//! use compiler::analysis::with_analysis;
//!
//! let found = with_analysis("def main():\n    let x: int = \"oops\"\n", |ctx| {
//!     ctx.diagnostics.len()
//! });
//!
//! assert_eq!(found, 1);
//! ```
//!
//! # Scope
//!
//! There is no module system yet, so a compilation unit is a single file: no
//! imports, no cross-file resolution, and the standard library is built into
//! the compiler rather than written in Jarvil (see [`builtin`] and `std/`).
//!
//! [`BlockNode`]: ast::ast::BlockNode
//! [`SemanticStateDatabase`]: scope::semantic_db::SemanticStateDatabase
//! [`JarvilProgramAnalysisErrors`]: error::error::JarvilProgramAnalysisErrors

// The last `unsafe` block went when the diagnostic collector stopped being an
// `UnsafeCell`. Nothing in a compiler of this shape needs it, and forbidding it
// outright is what keeps a future "just for performance" hand-written safety
// argument from reappearing.
#![forbid(unsafe_code)]
// `foo/foo.rs` holding the substance of module `foo`, with `foo/mod.rs` as the
// re-export surface, is the layout this crate is built around: `ast::ast`,
// `lexer::lexer`, `parser::parser`, `scope::scope`, `error::error`. Renaming
// them would touch essentially every import in the crate for no benefit.
#![allow(clippy::module_inception)]
// The AST and scope constructors genuinely carry that many independent pieces
// of syntax. Grouping them into parameter structs purely to satisfy a count
// would add indirection without making any call site clearer.
#![allow(clippy::too_many_arguments)]
// A handful of internal predicates signal failure with `Result<_, ()>`. Giving
// them real error types is worth doing, but it belongs with the diagnostics
// rework rather than a lint sweep, since the useful error payloads are exactly
// what that work decides.
#![allow(clippy::result_unit_err)]

use crate::lexer::lexer::JarvilLexer;
use ast::ast::BlockNode;
use ast::print::serialize_ast;
use code::{JarvilCode, JarvilCodeHandler};
use codegen::python::PythonCodeGenerator;
use error::error::JarvilProgramAnalysisErrors;
use miette::Report;
use parser::parser::JarvilParser;
use parser::resolver::JarvilResolver;
use parser::type_checker::JarvilTypeChecker;

pub mod analysis;
pub mod ast;
pub mod builtin;
pub mod code;
pub mod codegen;
pub mod constants;
pub mod context;
pub mod core;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod queries;
pub mod scope;
#[cfg(test)]
pub mod tests;
pub mod types;

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

/// Lexes and parses `code`, stopping before name resolution.
///
/// Always returns a tree. The parser recovers from syntax errors rather than
/// bailing, so a malformed program still yields a [`BlockNode`] -- with
/// `MissingToken` and `SkippedTokens` nodes marking the damage -- and any
/// diagnostics are pushed onto `errors`. Check `errors` to find out whether
/// parsing actually succeeded.
///
/// The returned [`JarvilCodeHandler`] borrows `code`, which is why both are
/// passed in by reference rather than owned.
pub fn build_ast<'ctx>(
    code: &'ctx JarvilCode,
    errors: &'ctx JarvilProgramAnalysisErrors,
) -> (BlockNode, JarvilCodeHandler<'ctx>) {
    // lexing
    let core_lexer = JarvilLexer::new(errors);
    let (token_vec, code_lines) = core_lexer.tokenize(code);
    let code_handler = JarvilCodeHandler::new(code, code_lines);

    // parsing
    let parser = JarvilParser::new(&code_handler, errors);
    let ast = parser.parse(token_vec);
    (ast, code_handler)
}

/// Compiles Jarvil source to Python, running the whole pipeline.
///
/// Returns the generated Python alongside a JSON rendering of the syntax tree.
/// The JSON is produced whether or not compilation succeeded, which is why it
/// sits outside the `Result`: `anyon` writes it next to the source as a
/// debugging aid even for a program that failed to compile.
///
/// # Errors
///
/// Returns the **first** diagnostic any pass reported, with the source attached
/// so it renders with a code frame. Later diagnostics are discarded; a caller
/// that wants all of them -- an editor, say -- should use
/// [`analysis::with_analysis`] instead.
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
