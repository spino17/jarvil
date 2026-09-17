//! The Jarvil front end: everything from source text to a checked program.
//!
//! Jarvil is a statically typed language that transpiles to Python. The point
//! is to catch at compile time the class of bug Python only reports at runtime,
//! while still producing ordinary Python that runs anywhere Python does.
//!
//! This crate stops once a program has been understood. Emitting Python is
//! `jarvil-py`'s job, and it depends on this crate rather than the other way
//! round -- so a tool that only needs to *understand* Jarvil, such as the
//! language server, never pulls in a code generator it will not call.
//!
//! # The pipeline
//!
//! Four passes over one file, each depending on the last:
//!
//! | Pass | Module | Produces |
//! |------|--------|----------|
//! | Lexing | [`lexer`] | a token stream, with comments and whitespace kept as trivia |
//! | Parsing | [`parser::parser`] | a [`BlockNode`] syntax tree, recovering from errors rather than stopping |
//! | Name resolution | [`parser::resolver`] | a [`SemanticStateDatabase`] binding every identifier to a symbol |
//! | Type checking | [`parser::type_checker`] | the same database, with types inferred and checked |
//!
//! Diagnostics from every pass accumulate in one
//! [`JarvilProgramAnalysisErrors`], so a failed pass does not stop the next one
//! from running -- which is what lets a single compile report several errors,
//! and lets an editor answer questions about code that does not compile.
//!
//! # Entry points
//!
//! [`analysis::with_analysis`] runs the whole front end and hands back the
//! syntax tree, the semantic database and *every* diagnostic. [`queries`]
//! builds go-to-definition and hover on top of it.
//!
//! ```
//! use jarvil_parser::analysis::with_analysis;
//!
//! let found = with_analysis("def main():\n    let x: int = \"oops\"\n", |ctx| {
//!     ctx.diagnostics.len()
//! });
//!
//! assert_eq!(found, 1);
//! ```
//!
//! [`build_ast`] is the lower-level half, for a caller that wants only lexing
//! and parsing.
//!
//! To compile all the way to Python, use `jarvil_py::build_code`.
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
use code::{JarvilCode, JarvilCodeHandler};
use error::error::JarvilProgramAnalysisErrors;
use parser::parser::JarvilParser;

pub mod analysis;
pub mod ast;
pub mod builtin;
pub mod code;
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
