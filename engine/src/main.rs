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

use jarvil::constants::common::token_for_identifier;

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
    // start_compiler(args);
    let ve = vec!['i', 'f'];
    let s_iter: std::slice::Iter<char> = ve.iter();
    println!("{:?}", token_for_identifier(s_iter));
}
