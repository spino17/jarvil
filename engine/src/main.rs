mod errors;
mod lexer;
mod parser;
mod scope;
mod context;
mod constants;
mod reader;

use errors::CompilationError;
use crate::reader::read_file;
use crate::lexer::lexer::{CoreLexer, Lexer};
use std::env::args;
use crate::parser::packrat::PackratParser;
use crate::parser::packrat::Parser;

fn start_compiler() -> Result<(), CompilationError> {
    let args: Vec<String> = args().collect();
    let char_vec: Vec<char> = read_file("/Users/bhavyabhatt/Desktop/main.jv")?;
    let mut core_lexer = CoreLexer::new();
    let token_vec = core_lexer.tokenize(char_vec)?;
    let code_lines = core_lexer.get_code_lines();
    let mut parser = PackratParser::new(code_lines);
    if token_vec.len() > 0 {
        let ast = parser.parse(token_vec, )?;  // TODO - do bytecode generation using this ast object
    }
    Ok(())
}

fn main() {
    match start_compiler() {
        Ok(()) => {},
        Err(err) => {
            println!("{}", err);
        }
    }
}