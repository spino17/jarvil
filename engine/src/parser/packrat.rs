// Default parser for jarvil is Packrat developed by Bryan Ford in his master thesis at MIT. It is essentially a top down 
// recursive parsing with optimized backtracting in order to avoid exponential parse time and provide reliable linear time 
// parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use crate::parser::core::Parser;
use crate::lexer::token::Token;
use crate::parser::ast::AST;
use crate::errors::ParseError;
use crate::env::Env;

pub struct PackratParser {
    lookahead: usize,
    indent_level: usize,
    env: Env,
}

impl PackratParser {
    pub fn new() -> Self {
        let env = Env::new();
        PackratParser {
            lookahead: 0,
            indent_level: 0,
            env,
        }
    }
}

impl PackratParser {
    fn code(&mut self) -> Result<(), ParseError> {
        self.stmt()?;
        let stmt_c = || {
            self.stmt()?;
            Ok(())
        };
        self.zero_or_more(stmt_c)?;
        Ok(())
    }

    fn stmt(&self) -> Result<(), ParseError> {
        Ok(())
    }

    fn zero_or_more<F: FnMut() -> Result<(), ParseError>>(&self, mut f: F) -> Result<(), ParseError> {
        loop {
            match f() {
                Ok(()) => continue,
                Err(_) => return Ok(())
            }
        }
    }
}

impl Parser for PackratParser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        self.code()?;
        Ok(())
    }
}