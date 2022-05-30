mod errors;
mod lexer;
mod env;

use std::fs;
use errors::CompilationError;
use crate::env::Env;

fn main() -> Result<(), CompilationError> {
    let contents = fs::read_to_string("/Users/bhavyabhatt/Desktop/main.jv")?;
    // call init on symbol_table to set keywords before lexical phase.
    // call scan from lexical analyzer to return iter of tokens.
    // attempt parsing for syntax and semantical analysis.
    Ok(())
}