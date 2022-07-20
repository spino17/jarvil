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

use crate::reader::read_file;
use crate::lexer::lexer::{CoreLexer, Lexer};
use std::env::args;
use crate::parser::parser::PackratParser;
use crate::parser::parser::Parser;
use colored::Colorize;

fn start_compiler() {
    let args: Vec<String> = args().collect();
    let char_vec: Vec<char> = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let mut core_lexer = CoreLexer::new();
    let token_vec = core_lexer.tokenize(char_vec);
    let (code_lines, lexical_errors) = core_lexer.get_lexical_data_useful_for_parser();
    if lexical_errors.len() > 0 {
        print!("{}", lexical_errors[0]);
        // TODO - dump all other errors in some log file, let users choose how many errors to show
        return;
    }
    let mut parser = PackratParser::new(code_lines);
    let (ast, syntax_errors) = parser.parse(token_vec);
    if syntax_errors.len() > 0 {
        println!("{}", syntax_errors[0]);
        // println!("{:?}", ast);
        // TODO - dump all other errors in some log file, let users choose how many errors to show
        return;
    }
}

fn main() {
    start_compiler();
}