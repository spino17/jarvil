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

fn start_compiler(args: Vec<String>) {
    let code_vec = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let result = build(code_vec);
    match result {
        Err(err) => println!("{}", err),
        _ => {}
    }
}

macro_rules! impl_enum {
    ($t: tt, $v: tt) => {
        pub fn is_eq(symbol: &str) -> bool {
            match symbol {
                $t => {
                    println!("I am bro");
                    true
                }
                $v => {
                    println!("I am sis");
                    true
                },
                _ => {
                    println!("I am something else");
                    false
                }
            }
        }
    };
}

impl_enum!("name", "class");


fn main() {
    let args: Vec<String> = args().collect();
    println!("{}", is_eq("dsdsds"));
    start_compiler(args);
}
