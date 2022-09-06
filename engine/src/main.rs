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
mod utils;

use crate::cmd::compile::build::build;
use crate::reader::read_file;
use jarvil::backend::chunk::{Chunk, OpCode};
use jarvil::backend::object::core::Data;
use jarvil::backend::vm::VM;
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
    let mut chunk = Chunk::default();
    chunk.write_constant(Data::INT(13), 1);
    chunk.write_constant(Data::INT(12), 2);
    // chunk.write_byte(OpCode::OP_FALSE.to_byte(), 10);
    // chunk.write_byte(OpCode::OP_FALSE.to_byte(), 10);
    // chunk.write_byte(OpCode::OP_DIVIDE.to_byte(), 4);
    // chunk.write_byte(OpCode::OP_ADD.to_byte(), 5);
    // chunk.write_byte(OpCode::OP_EQUAL.to_byte(), 6);
    chunk.write_byte(OpCode::OP_GREATER_EQUAL.to_byte(), 8);
    chunk.write_byte(OpCode::OP_RETURN.to_byte(), 7);
    let mut vm = VM::new(chunk);
    vm.run();
}
