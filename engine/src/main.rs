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
use jarvil::backend::object::core::{Data, Object};
use jarvil::backend::object::string::StringObject;
use jarvil::backend::vm::VM;
use miette::{GraphicalReportHandler, GraphicalTheme, Report};
use owo_colors::{OwoColorize, Style};
use std::alloc::{alloc, dealloc, Layout};
use std::env::args;
use std::mem::ManuallyDrop;
use std::panic;
use std::{fs, path::Path};
use thiserror::Error;

fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);
    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => return err,
        None => unreachable!("the result should always unwrap to an error"),
    }
}

fn start_compiler(args: Vec<String>) {
    let (code_vec, code_str) = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let result = build(code_vec);
    if let Err(err) = result {
        let err = attach_source_code(err.report(), code_str);
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
    start_compiler(args);
    let mut chunk = Chunk::default();
    let mut s = StringObject::new_with_bytes("bro ");
    let mut v = StringObject::new_with_bytes("varima");
    let mut u = StringObject::new_with_bytes("bro varima");
    chunk.write_constant(Data::INT(13), 1);
    chunk.write_constant(Data::INT(12), 2);
    chunk.write_constant(Data::OBJ(Object::STRING(s.clone())), 5);
    chunk.write_constant(Data::OBJ(Object::STRING(v.clone())), 5);
    chunk.write_byte(OpCode::OP_ADD.to_byte(), 8);
    chunk.write_constant(Data::OBJ(Object::STRING(u.clone())), 5);
    chunk.write_byte(OpCode::OP_EQUAL.to_byte(), 8);
    chunk.write_byte(OpCode::OP_RETURN.to_byte(), 7);
    let mut vm = VM::new(chunk);
    vm.run();
    /*
    unsafe {
        std::mem::ManuallyDrop::drop(&mut s.0);
        std::mem::ManuallyDrop::drop(&mut v.0);
    }
     */
}
