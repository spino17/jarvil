#[macro_use]
extern crate jarvil_macros;

mod ast;
mod cmd;
mod code;
mod constants;
mod context;
mod errors;
mod lexer;
mod parser;
mod reader;
mod scope;
mod server;
mod types;
mod utils;

use crate::cmd::compile::build::build;
use crate::reader::read_file;
use std::env::args;
use crate::ast::ast::ASTNode::STATEMENT;

fn start_compiler(args: Vec<String>) {
    let code_vec = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let result = build(code_vec);
    match result {
        Err(err) => println!("{}", err),
        _ => {}
    }
}

macro_rules! print_args {
    (($($t: ident),*)) => {
        $(
            println!("{}", stringify!($t));
        )*
    };
}

macro_rules! print_optional {
    ($t: ident) => {
        match $t {
            Some(val) => println!("{}", val),
            None => println!("I am None bloody"),
        }
    };
}

#[set_parent(STATEMENT)]
fn this_will_be_destroyed(name: usize, dude: Option<String>) {
    let node = 11;
    println!("I am already existing");
}


fn main() {
    let args: Vec<String> = args().collect();
    start_compiler(args);
    this_will_be_destroyed(10, Some(String::from("Bhavya is best")));
}
