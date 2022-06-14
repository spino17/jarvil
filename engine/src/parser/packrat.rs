// Default parser for jarvil is Packrat developed by Bryan Ford in his master thesis at MIT. It is essentially a top down 
// recursive parsing with optimized backtracting in order to avoid exponential parse time and provide reliable linear time 
// parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use crate::parser::core::Parser;
use crate::lexer::token::Token;
use crate::parser::ast::AST;
use crate::errors::ParseError;

pub struct PackratParser {

}

impl Parser for PackratParser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<AST, ParseError> {
        todo!()
    }
}