use crate::lexer::token::Token;
use crate::parser::ast::AST;
use crate::errors::ParseError;

pub trait Parser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<AST, ParseError>;  // return an AST
}