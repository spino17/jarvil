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

    pub fn build_code(&self, mut code: JarvilCode, code_str: String) -> Result<String, Report> {
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
            let err = errors[0].clone();
            return Err(attach_source_code(err.report(), code_str));
        }

        // Python code-generation
        let py_generator = PythonCodeGenerator::new(&code);
        let py_code = py_generator.generate_python_code(&ast);
        Ok(py_code)
    }
}

impl AbstractCommand for BuildDriver {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        // TODO - add logic to check the command
        Ok(())
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        let curr_dir_path = context::curr_dir_path();
        let jarvil_code_file_path = format!("{}/main.jv", curr_dir_path);
        let transpiled_py_code_file_path = format!("{}/__transpiled_py_code__.py", curr_dir_path);
        let (code_vec, code_str) = read_file(&jarvil_code_file_path)?;
        let code = JarvilCode::new(code_vec);
        let py_code = self.build_code(code, code_str)?;
        fs::write(&transpiled_py_code_file_path, py_code)?;
        match self.mode {
            BuildMode::RUN => {
                let output = Command::new("python3")
                    .arg(transpiled_py_code_file_path)
                    .output()?;
                let len = output.stdout.len();
                if len > 0 {
                    let std_output_str = str::from_utf8(&output.stdout[..len - 1])?;
                    println!("{}", std_output_str)
                }
            }
            BuildMode::BUILD => {}
        }
        Ok(())
    }

    fn help_str(&self) -> String {
        todo!()
    }
}
