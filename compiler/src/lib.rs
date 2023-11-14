use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use ast::ast::BlockNode;
use code::JarvilCode;
use codegen::python::PythonCodeGenerator;
use error::diagnostics::Diagnostics;
use lexer::lexer::CoreLexer;
use miette::Report;
use parser::parser::JarvilParser;
use parser::resolver::Resolver;
use parser::type_checker::TypeChecker;

pub mod ast;
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
use std::env;
use std::rc::Rc;

pub fn curr_dir_path() -> Rc<String> {
    let curr_dir = env::current_dir().expect("failed to get current directory");
    let curr_dir_str = curr_dir.to_string_lossy();
    Rc::new(curr_dir_str.to_string())
}

fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);
    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => err,
        None => unreachable!("the result should always unwrap to an error"),
    }
}

pub fn build_ast(mut code: JarvilCode) -> (BlockNode, Vec<Diagnostics>, JarvilCode) {
    let core_lexer = CoreLexer::new();
    let (token_vec, mut errors) = core_lexer.tokenize(&mut code);
    let parser = JarvilParser::new(code);
    let (ast, mut parse_errors, code) = parser.parse(token_vec);
    errors.append(&mut parse_errors);
    (ast, errors, code)
}

pub fn build_code(code: JarvilCode, code_str: String) -> Result<String, Report> {
    let (ast, mut errors, code) = build_ast(code);

    // name-resolver
    let resolver = Resolver::new(code);
    let (semantic_state_db, mut semantic_errors, code) = resolver.resolve_ast(&ast);
    errors.append(&mut semantic_errors);

    // type-checker
    let type_checker = TypeChecker::new(code, semantic_state_db);
    let (semantic_state_db, code) = type_checker.check_ast(&ast, &mut errors);

    if !errors.is_empty() {
        let err = &errors[0];
        return Err(attach_source_code(err.report(), code_str));
    }

    // Python code-generation
    let py_generator = PythonCodeGenerator::new(code, semantic_state_db);
    let py_code = py_generator.generate_python_code(&ast);
    Ok(py_code)
}
