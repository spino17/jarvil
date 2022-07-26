mod errors;
mod lexer;
mod parser;
mod scope;
mod context;
mod constants;
mod reader;
mod types;
mod ast;
mod utils;
mod code;
mod cmd;
mod server;

use crate::cmd::compile::build::build;
use crate::reader::read_file;
use std::env::args;

fn start_compiler(args: Vec<String>) {
    let code_vec = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let result = build(code_vec);
    match result {
        Err(err) => println!("{}", err),
        _ => {}
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    start_compiler(args);
}