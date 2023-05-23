use super::core::AbstractCommand;
use super::error::AnyonError;
use crate::ast::ast::BlockNode;
use crate::code::JarvilCode;
use crate::codegen::python::PythonCodeGenerator;
use crate::context;
use crate::error::diagnostics::Diagnostics;
use crate::lexer::lexer::{CoreLexer, Lexer};
use crate::parser::parser::{JarvilParser, Parser};
use crate::parser::resolver::Resolver;
use crate::parser::type_checker::TypeChecker;
use crate::reader::read_file;
use miette::Report;
use std::fs;
use std::process::Command;
use std::str;

fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);
    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => return err,
        None => unreachable!("the result should always unwrap to an error"),
    }
}

#[derive(Debug)]
pub enum BuildMode {
    BUILD,
    RUN,
}

#[derive(Debug)]
pub struct BuildDriver {
    command_line_args: Vec<String>,
    mode: BuildMode,
}

impl BuildDriver {
    pub fn new(command_line_args: Vec<String>, mode: BuildMode) -> Self {
        BuildDriver {
            command_line_args,
            mode,
        }
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
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        Ok(())
    }

    fn execute_cmd(&self) {
        let curr_dir_path = context::curr_dir_path();
        // TODO - handle err case when main.jv does not exist
        let jarvil_code_file_path = format!("{}/main.jv", curr_dir_path);
        let transpiled_py_code_file_path = format!("{}/__transpiled_py_code__.py", curr_dir_path);
        let (code_vec, code_str) = read_file(&jarvil_code_file_path).unwrap();
        let code = JarvilCode::new(code_vec);
        match self.build_code(code) {
            Ok(py_code) => {
                fs::write(&transpiled_py_code_file_path, py_code).expect("file write failed");
                match self.mode {
                    BuildMode::RUN => {
                        let output = Command::new("python3")
                            .arg(transpiled_py_code_file_path)
                            .output();
                        match output {
                            Ok(output) => {
                                let len = output.stdout.len();
                                if len > 0 {
                                    match str::from_utf8(&output.stdout[..len - 1]) {
                                        Ok(v) => println!("{}", v),
                                        Err(e) => panic!("Invalid UTF-8 sequence: {}", e),
                                    }
                                }
                            }
                            Err(err) => {
                                // TODO - have a wrapper denoting that this error is Python generated runtime error
                                println!("{:?}", err)
                            }
                        }
                    }
                    BuildMode::BUILD => {}
                }
            }
            Err(err) => {
                let err = attach_source_code(err.report(), code_str);
                println!("{:?}", err);
            }
        }
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
