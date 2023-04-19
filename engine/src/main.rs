#[macro_use]
extern crate jarvil_macros;
mod ast;
mod backend;
mod cmd;
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
mod types;

use crate::cmd::compile::build::build;
use crate::reader::read_file;
use miette::{GraphicalReportHandler, GraphicalTheme, Report};
use owo_colors::Style;
use std::env::args;

fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);
    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => return err,
        None => unreachable!("the result should always unwrap to an error"),
    }
}

fn compile(args: Vec<String>) {
    let (code_vec, code_str) = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let result = build(code_vec);
    if let Err(err) = result {
        let err = attach_source_code(err.report(), code_str);
        // TODO - later give option to the user to display all errors
        println!("{:?}", err)
    }
}

fn main() {
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
    compile(args);
}
