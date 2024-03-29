use crate::lexer::lexer::Lexer;
use ast::ast::BlockNode;
use ast::print::serialize_ast;
use code::{JarvilCode, JarvilCodeHandler};
use codegen::python::PythonCodeGenerator;
use error::diagnostics::Diagnostics;
use lexer::lexer::CoreLexer;
use miette::Report;
use parser::parser::JarvilParser;
use parser::resolver::Resolver;
use parser::type_checker::TypeChecker;

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
pub mod scope;
#[cfg(test)]
pub mod tests;
pub mod types;

fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);
    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => err,
        None => unreachable!("the result should always unwrap to an error"),
    }
}

pub fn build_ast(code: &mut JarvilCode) -> (BlockNode, Vec<Diagnostics>, JarvilCodeHandler) {
    let core_lexer = CoreLexer::new();
    let (token_vec, mut errors, code_lines) = core_lexer.tokenize(code);
    let code_handler = JarvilCodeHandler::new(code, code_lines);
    let parser = JarvilParser::new(&code_handler);
    let (ast, mut parse_errors) = parser.parse(token_vec);
    errors.append(&mut parse_errors);
    (ast, errors, code_handler)
}

pub fn build_code(mut code: JarvilCode) -> (Result<String, Report>, String) {
    let (ast, mut errors, code_handler) = build_ast(&mut code);

    // name resolution
    let resolver = Resolver::new(&code_handler);
    let (semantic_db, mut semantic_errors) = resolver.resolve_ast(&ast);
    errors.append(&mut semantic_errors);

    // type checking
    let type_checker = TypeChecker::new(&code_handler, semantic_db);
    let semantic_db = type_checker.check_ast(&ast, &mut errors);

    // ast json serialization
    let ast_str = serialize_ast(&ast, &code_handler, semantic_db.interner()).unwrap();

    if !errors.is_empty() {
        let err = &errors[0];
        return (
            Err(attach_source_code(err.report(), code.to_string())),
            ast_str,
        );
    }

    // Python code-generation
    let py_generator = PythonCodeGenerator::new(&code_handler, semantic_db);
    let py_code = py_generator.generate_python_code(&ast);
    (Ok(py_code), ast_str)
}
