// Default parser for jarvil is Packrat developed by Bryan Ford in his master thesis at MIT. It is essentially a top down 
// recursive parsing with optimized backtracting in order to avoid exponential parse time and provide reliable linear time 
// parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use crate::parser::core::Parser;
use crate::lexer::token::{Token, CoreToken};
use crate::parser::ast::AST;
use crate::errors::{ParseError, SyntaxError, aggregate_errors};
use crate::env::Env;

pub struct PackratParser {
    token_vec: Vec<Token>,
    lookahead: usize,
    indent_level: usize,
    env: Env,
    // TODO - add look up hash table for cached results
}

impl PackratParser {
    pub fn new() -> Self {
        let env = Env::new();
        PackratParser {
            token_vec: Vec::new(),
            lookahead: 0,
            indent_level: 0,
            env,
        }
    }
}

impl PackratParser {
    fn code(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        self.token_vec = token_vec;
        self.stmt()?;
        let stmt_c = || {
            self.stmt()?;
            Ok(())
        };
        PackratParser::expect_zero_or_more(stmt_c)?;
        Ok(())
    }

    fn stmt(&mut self) -> Result<(), ParseError> {
        if let Ok(_) = self.compound_stmt() {
            return Ok(())
        }
        if let Ok(_) = self.simple_stmts() {
            return Ok(())
        }
        Err(aggregate_errors())
    }

    fn compound_stmt(&mut self) -> Result<(), ParseError> {
        Ok(())
    }

    fn simple_stmts(&mut self) -> Result<(), ParseError> {
        self.simple_stmt()?;
        self.expect("\n")?;
        self.simple_stmts()?;
        Ok(())
    }

    fn simple_stmt(&mut self) -> Result<(), ParseError> {
        if let Ok(()) = self.decl() {
            return Ok(())
        }
        if let Ok(()) = self.assign() {
            return Ok(())
        }
        Err(aggregate_errors())
    }

    fn decl(&mut self) -> Result<(), ParseError> {
        todo!()
    }

    fn assign(&mut self) -> Result<(), ParseError> {
        let token = &self.token_vec[self.lookahead];
        match token.core_token {
            CoreToken::IDENTIFIER(_) => {
                self.expect("id")?;
                self.expect("=")?;
                self.expr()?;
                Ok(())
            },
            _ => Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, self.lookahead, "Expected an Identifier on left side of the assignment")))
        }
    }

    fn expr(&mut self) -> Result<(), ParseError> {
        todo!()
    }

    fn expect_zero_or_more<F: FnMut() -> Result<(), ParseError>>(mut f: F) -> Result<(), ParseError> {
        loop {
            match f() {
                Ok(()) => continue,
                Err(_) => return Ok(())
            }
        }
    }

    fn expect(&mut self, symbol: &'static str) -> Result<(), ParseError> {
        todo!()
    }

    fn expect_and_get_value(&mut self, symbol: &'static str) -> Result<(), ParseError> {
        todo!()
    }
}

impl Parser for PackratParser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        self.code(token_vec)?;
        Ok(())
    }
}