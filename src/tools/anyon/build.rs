use super::core::AbstractCommand;
use super::error::AnyonError;
use super::helper::check_jarvil_code_file_extension;
use crate::ast::ast::BlockNode;
use crate::code::JarvilCode;
use crate::codegen::python::PythonCodeGenerator;
use crate::context;
use crate::error::constants::TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG;
use crate::error::diagnostics::Diagnostics;
use crate::lexer::lexer::{CoreLexer, Lexer};
use crate::parser::parser::{JarvilParser, Parser};
use crate::parser::resolver::Resolver;
use crate::parser::type_checker::TypeChecker;
use crate::reader::read_file;
use miette::Report;
use std::fs;
use std::process::Command;
use std::rc::Rc;
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
    alternate_code_file_name: Option<String>,
}

impl BuildDriver {
    pub fn new(command_line_args: Vec<String>, mode: BuildMode) -> Self {
        BuildDriver {
            command_line_args,
            mode,
            alternate_code_file_name: None,
        }
    }

    pub fn get_code_file_name(&self) -> String {
        match &self.alternate_code_file_name {
            Some(file_name) => file_name.to_string(),
            None => "main".to_string(),
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
        let (namespace, mut semantic_errors, identifier_binding_table, self_binding_table) =
            resolver.resolve_ast(&ast);
        errors.append(&mut semantic_errors);

        // type-checker
        let type_checker = TypeChecker::new(
            &code,
            namespace,
            identifier_binding_table,
            self_binding_table,
        );
        let (mut type_errors, namespace, identifier_binding_table, self_binding_table) =
            type_checker.check_ast(&ast);

        errors.append(&mut type_errors);
        if errors.len() > 0 {
            let err = errors[0].clone();
            return Err(attach_source_code(err.report(), code_str));
        }

        // Python code-generation
        let py_generator = PythonCodeGenerator::new(
            &code,
            namespace,
            identifier_binding_table,
            self_binding_table,
        );
        let py_code = py_generator.generate_python_code(&ast);
        Ok(py_code)
    }
}

impl AbstractCommand for BuildDriver {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        let len = self.command_line_args.len();
        if len == 2 {
            return Ok(());
        } else if len == 3 {
            let file_name = check_jarvil_code_file_extension(&self.command_line_args[2])?;
            self.alternate_code_file_name = Some(file_name.to_string());
            return Ok(());
        } else {
            return Err(AnyonError::new_with_command(
                TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG.to_string(),
            ));
        }
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        let curr_dir_path = context::curr_dir_path();
        let code_file_name = self.get_code_file_name();
        let jarvil_code_file_path = format!("{}/{}.jv", curr_dir_path, code_file_name);
        let transpiled_py_code_file_path = format!(
            "{}/__transpiled_{}_py_code__.py",
            curr_dir_path, code_file_name
        );
        let (code_vec, code_str) = read_file(&jarvil_code_file_path)?;
        let code = JarvilCode::new(code_vec);
        let py_code = self.build_code(code, code_str)?;
        fs::write(&transpiled_py_code_file_path, py_code)?;
        // format the Python code using `black` if available
        let _ = Command::new("python3")
            .arg("-m")
            .arg("black")
            .arg(&transpiled_py_code_file_path)
            .output()?;
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
