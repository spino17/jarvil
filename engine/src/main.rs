#[macro_use]
extern crate jarvil_macros;
mod ast;
mod cmd;
mod code;
mod constants;
mod context;
mod error;
mod lexer;
mod parser;
mod reader;
mod scope;
mod server;
mod types;
mod utils;

use crate::cmd::compile::build::build;
use crate::reader::read_file;
use std::convert::TryInto;
use std::env::args;

fn start_compiler(args: Vec<String>) {
    let code_vec = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let result = build(code_vec);
    if let Err(err) = result {
        println!("{}", err)
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    start_compiler(args);
    let x: usize = 10;
    let v = x.to_be_bytes().to_vec();
    let v_array = v[..].try_into().unwrap();
    let y = usize::from_be_bytes(v_array);
    assert!(x == y);
}
