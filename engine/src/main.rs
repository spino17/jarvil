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
use jarvil::backend::object::string::StringObject;
use jarvil::backend::vm::VM;
use miette::{GraphicalReportHandler, GraphicalTheme, Report};
use owo_colors::{OwoColorize, Style};
use std::alloc::{alloc, dealloc, Layout};
use std::env::args;
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
    /*
    panic::set_hook(Box::new(|_info| {
        // do nothing
    }));

    let result = panic::catch_unwind(|| {
        panic!("test panic");
    });

    match result {
        Ok(res) => res,
        Err(_) => println!("caught panic!"),
    }

    let size = "bhavya_bhatt";
    let len = size.len();
    let bytes_str = size.as_bytes();

    unsafe {
        let layout = Layout::array::<u8>(len).unwrap();
        let ptr = alloc(layout);
        for i in 0..len {
            *ptr.offset(i as isize) = bytes_str[i];
        }
        for i in 0..len {
            println!("{}-{}", *ptr.offset(i as isize), bytes_str[i]);
        }
        dealloc(ptr, layout);
    }
     */
    let s = StringObject::new_with_bytes("Bhavya".to_string());
    println!("{}", s);
    let v = StringObject::new_with_bytes("!and she is varima".to_string());
    println!("{}", v);
    println!("{}", StringObject::add(&s, &v));
    println!("{}", s);
}
