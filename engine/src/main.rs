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

use miette::{GraphicalReportHandler, GraphicalTheme};
use owo_colors::Style;
use std::env::args;
use tools::anyon::core::{get_cmd_from_command_line_args, AbstractCommand};

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
}
