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
            println!("inside print_args! : `{}`", stringify!($t));
        )*
    };
}

macro_rules! print_args_optional {
    (($($t: ident),*)) => {
        $(
            match $t {
                Some(val) => println!("inside optional print_args! : `{:?}`", val),
                None => println!("inside optional print_args! : `None`"),
            }
        )*
    };
}

#[derive(Debug)]
struct Node {
    name: String,
}

#[set_parent(STATEMENT)]
fn this_will_be_destroyed(n: Node, dude: Option<Node>, boss: usize) {
    let node = 11;
    println!("I am already existing");
}


fn main() {
    let args: Vec<String> = args().collect();
    start_compiler(args);
    let n = Node{
        name: "bhavya is best".to_string(),
    };
    let m = Node{
        name: "varima is best".to_string(),
    };
    this_will_be_destroyed(m, Some(n), 10);
}
