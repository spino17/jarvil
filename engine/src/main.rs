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
use convert_case::{Case, Casing};
use crate::ast::ast::NODES_ARRAY;

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
    println!("{}", "StatementIndentWrapperNode".to_case(Case::Snake));
    println!("{:?}", NODES_ARRAY);
}
