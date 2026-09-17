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

fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);
    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => err,
        None => unreachable!(),
    }
}

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
