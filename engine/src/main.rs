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

use crate::utils::common::build_ast;
use crate::reader::read_file;
use std::env::args;

fn start_compiler() {
    let code_vec = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    /*
    let mut core_lexer = CoreLexer::new();
    let token_vec = core_lexer.tokenize(&mut code);
    // let lexical_errors = core_lexer.get_lexical_data_useful_for_parser();
    // TODO - dump all other errors in some log file, let users choose how many errors to show
    match context::first_error() {
        Some(error) => {
            print!("{}", error);
            return;
        },
        None => {}
    }
    let mut parser = PackratParser::new(&code);
    let ast = parser.parse(token_vec);
     */
    // let mut code = Code::new(code_vec);
    let ast_result = build_ast(code_vec);
    let ast = match ast_result {
        Ok(ast) => ast,
        Err(err) => {
            print!("{}", err);
            return;
        }
    };
    // println!("{:?}", ast);
    match context::first_error() {
        Some(error) => {
            print!("{}", error);
            return;
        },
        None => {}
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    println!("{:?}", args);
    // Use args to check which cmd to run
    start_compiler();
}