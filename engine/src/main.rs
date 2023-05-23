#[macro_use]
extern crate jarvil_macros;
mod ast;
mod code;
mod codegen;
mod constants;
mod context;
mod error;
mod lexer;
mod parser;
mod reader;
mod scope;
mod server;
mod tools;
mod types;

use crate::reader::read_file;
use crate::tools::anyon::build::build;
use code::JarvilCode;
use miette::{GraphicalReportHandler, GraphicalTheme, Report};
use owo_colors::Style;
use std::str;
use std::{env::args, fs, process::Command};
use tools::anyon::core::{get_cmd_from_command_line_args, AbstractCommand};

fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);
    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => return err,
        None => unreachable!("the result should always unwrap to an error"),
    }
}

fn compile(args: Vec<String>) {
    let (code_vec, code_str) = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let code = JarvilCode::new(code_vec);
    let result = build(code);
    match result {
        Ok(py_code) => {
            fs::write(
                "/Users/bhavyabhatt/Desktop/__transpiled_python_code__.py",
                py_code,
            )
            .expect("file write failed");
            let output = Command::new("python3")
                .arg("/Users/bhavyabhatt/Desktop/__transpiled_python_code__.py")
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
        Err(err) => {
            let err = attach_source_code(err.report(), code_str);
            // TODO - later give option to the user to display all errors
            println!("{:?}", err);
        }
    }
}

fn main() {
    // hook for styling of the error messages
    miette::set_hook(Box::new(|err| {
        let mut my_theme = GraphicalTheme::default();
        my_theme.styles.linum = Style::new().bright_blue();
        my_theme.styles.error = Style::new().red();
        my_theme.styles.warning = Style::new().yellow();
        my_theme.styles.advice = Style::new().yellow();
        my_theme.styles.help = Style::new().white();
        Box::new(GraphicalReportHandler::new_themed(my_theme))
    }));

    let args: Vec<String> = args().collect();
    let anyon_obj = get_cmd_from_command_line_args(args);
    match anyon_obj {
        Ok(mut ok_anyon_obj) => match ok_anyon_obj.check_cmd() {
            Ok(_) => ok_anyon_obj.execute_cmd(),
            Err(err) => println!("{}", err),
        },
        Err(err) => {
            println!("{}", err)
        }
    }
    // compile(args);
}
