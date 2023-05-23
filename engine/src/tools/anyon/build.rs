use super::core::AbstractCommand;
use super::error::AnyonError;
use crate::ast::ast::BlockNode;
use crate::code::JarvilCode;
use crate::codegen::python::PythonCodeGenerator;
use crate::error::diagnostics::Diagnostics;
use crate::lexer::lexer::{CoreLexer, Lexer};
use crate::parser::parser::{JarvilParser, Parser};
use crate::parser::resolver::Resolver;
use crate::parser::type_checker::TypeChecker;

#[derive(Debug)]
pub struct BuildDriver {
    command_line_args: Vec<String>,
}

impl BuildDriver {
    pub fn new(command_line_args: Vec<String>) -> Self {
        BuildDriver { command_line_args }
    }

    pub fn build_ast(&self, code: &mut JarvilCode) -> (BlockNode, Vec<Diagnostics>) {
        let core_lexer = CoreLexer::new();
        let (token_vec, mut errors) = core_lexer.tokenize(code);
        let parser = JarvilParser::new(&*code);
        let (ast, mut parse_errors) = parser.parse(token_vec);
        errors.append(&mut parse_errors);
        (ast, errors)
    }

    pub fn build_code(&self, mut code: JarvilCode) -> Result<String, Diagnostics> {
        let (ast, mut errors) = self.build_ast(&mut code);

        // name-resolver
        let resolver = Resolver::new(&code);
        let (scope_table, mut semantic_errors) = resolver.resolve_ast(&ast);
        ast.set_scope(&scope_table);
        errors.append(&mut semantic_errors);

        // type-checker
        let type_checker = TypeChecker::new(&code);
        let mut type_errors = type_checker.check_ast(&ast);

        errors.append(&mut type_errors);
        if errors.len() > 0 {
            return Err(errors[0].clone());
        }

        // python code-generation
        let py_generator = PythonCodeGenerator::new(&code);
        let py_code = py_generator.generate_python_code(&ast);
        Ok(py_code)
    }
}

impl AbstractCommand for BuildDriver {
    fn check_cmd(&self) -> Result<(), AnyonError> {
        todo!()
    }

    fn execute_cmd(&self) {
        todo!()
    }
}

// TODO - remove this in favour of new driver design pattern
pub fn build_ast(code: &mut JarvilCode) -> (BlockNode, Vec<Diagnostics>) {
    let core_lexer = CoreLexer::new();
    let (token_vec, mut errors) = core_lexer.tokenize(code);
    let parser = JarvilParser::new(&*code);
    let (ast, mut parse_errors) = parser.parse(token_vec);
    errors.append(&mut parse_errors);
    (ast, errors)
}

pub fn build(mut code: JarvilCode) -> Result<String, Diagnostics> {
    let (ast, mut errors) = build_ast(&mut code);

    let resolver = Resolver::new(&code);
    let (scope_table, mut semantic_errors) = resolver.resolve_ast(&ast);
    ast.set_scope(&scope_table);
    errors.append(&mut semantic_errors);

    let type_checker = TypeChecker::new(&code);
    let mut type_errors = type_checker.check_ast(&ast);

    errors.append(&mut type_errors);
    if errors.len() > 0 {
        return Err(errors[0].clone());
    }

    let py_generator = PythonCodeGenerator::new(&code);
    let py_code = py_generator.generate_python_code(&ast);
    Ok(py_code)
}
