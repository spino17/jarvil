use crate::ast::ast::BlockNode;
use crate::code::Code;
use crate::context;
use crate::errors::ParseError;
use crate::lexer::lexer::{CoreLexer, Lexer};
use crate::parser::parser::{PackratParser, Parser};

pub fn build_ast(code_vec: Vec<char>) -> Result<BlockNode, ParseError> {
    let mut code = Code::new(code_vec);
    let mut core_lexer = CoreLexer::new();
    let token_vec = core_lexer.tokenize(&mut code);
    match context::first_error() {
        Some(error) => return Err(error),
        None => {}
    }
    let parser = PackratParser::new(&code);
    let ast = parser.parse(token_vec);
    Ok(ast)
}