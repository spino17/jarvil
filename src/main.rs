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
mod tools;
mod types;

use miette::{GraphicalReportHandler, GraphicalTheme};
use owo_colors::Style;
use std::{env::args, fs};
use tools::anyon::{
    core::{get_cmd_from_command_line_args, AbstractCommand},
    error::AnyonError,
};
use std::fmt::Write;

fn check_and_execute_cmd(args: Vec<String>) -> Result<(), AnyonError> {
    let mut anyon_obj = get_cmd_from_command_line_args(args)?;
    anyon_obj.check_cmd()?;
    anyon_obj.execute_cmd()?;
    Ok(())
}

fn main() {
    // hook for styling of the error messages
    miette::set_hook(Box::new(|_err| {
        let mut my_theme = GraphicalTheme::default();
        my_theme.styles.linum = Style::new().bright_blue();
        my_theme.styles.error = Style::new().red();
        my_theme.styles.warning = Style::new().yellow();
        my_theme.styles.advice = Style::new().yellow();
        my_theme.styles.help = Style::new().white();
        Box::new(GraphicalReportHandler::new_themed(my_theme))
    }));

    // Create a buffer to capture the output
    let mut buffer = String::new();
    let args: Vec<String> = args().collect();
    let result = check_and_execute_cmd(args);
    if let Err(err) = result {
        write!(&mut buffer, "{:?}", err).expect("Failed to write to buffer");
        println!("{:?}", err);
    }
}
