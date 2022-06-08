mod errors;
mod lexer;
mod parser;
mod env;
mod context;
mod constants;
mod reader;

use errors::CompilationError;
use crate::reader::read_file;
use crate::lexer::lexer::{CoreLexer, Lexer};
use std::env::args;

fn main() -> Result<(), CompilationError> {
    let args: Vec<String> = args().collect();
    println!("{:?}", args);
    let char_vec: Vec<char> = read_file("/Users/bhavyabhatt/Desktop/main.jv")?;
    let mut core_lexer = CoreLexer::new();
    let token_vec = core_lexer.tokenize(char_vec)?;
    // TODO - pass this token_vec to parser and build AST!
    Ok(())
}