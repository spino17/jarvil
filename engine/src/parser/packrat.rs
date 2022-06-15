// Default parser for jarvil is Packrat developed by Bryan Ford in his master thesis at MIT. It is essentially a top down 
// recursive parsing with optimized backtracting in order to avoid exponential parse time and provide reliable linear time 
// parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use std::vec;
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
    // TODO - add AST data structure
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

impl Parser for PackratParser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        self.code(token_vec)?;
        Ok(())
    }
}

impl PackratParser {
    fn reset_lookahead(&mut self, reset_index: usize) {
        self.lookahead = reset_index;
    }

    fn code(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        self.token_vec = token_vec;
        self.stmt()?;
        let lookahead = self.lookahead;
        let new_lookahead = PackratParser::expect_zero_or_more(|| {
            let lookahead = self.stmt()?;
            Ok(lookahead)
        }, lookahead)?;
        self.reset_lookahead(new_lookahead);
        Ok(())
    }

    fn stmt(&mut self) -> Result<usize, ParseError> {
        let mut errors_vec: Vec<ParseError> = vec![];
        // TODO - handle lookahead index to reset it if a production fails
        // Below is a general pattern among many production rule cases where we always have to reset lookahead back to the original
        // value when trying out new production rule after prior one failed
        let curr_lookahead = self.lookahead;
        match self.compound_stmt() {
            Ok(lookahead) => return Ok(lookahead),
            Err(err) => {
                self.reset_lookahead(curr_lookahead);
                errors_vec.push(err);
            }
        }
        match self.simple_stmts() {
            Ok(lookahead) => return Ok(lookahead),
            Err(err) => {
                self.reset_lookahead(curr_lookahead);
                errors_vec.push(err);
            }
        }
        Err(aggregate_errors(errors_vec))
    }

    fn compound_stmt(&mut self) -> Result<usize, ParseError> {
        // TODO - add production rules for compound statements - see jarvil.gram
        Ok(self.lookahead)
    }

    fn simple_stmts(&mut self) -> Result<usize, ParseError> {
        self.simple_stmt()?;
        self.expect("\n")?;
        let lookahead = self.lookahead;
        let new_lookahead = PackratParser::expect_optionally(|| {
            let lookahead = self.simple_stmts()?;
            Ok(lookahead)
        }, lookahead)?;
        self.reset_lookahead(new_lookahead);
        Ok(self.lookahead)
    }

    fn simple_stmt(&mut self) -> Result<usize, ParseError> {
        let mut errors_vec: Vec<ParseError> = vec![];
        let curr_lookahead = self.lookahead;
        match self.decl() {
            Ok(lookahead) => return Ok(lookahead),
            Err(err) => {
                self.reset_lookahead(curr_lookahead);
                errors_vec.push(err);
            }
        }
        match self.assign() {
            Ok(lookahead) => return Ok(lookahead),
            Err(err) => {
                self.reset_lookahead(curr_lookahead);
                errors_vec.push(err);
            }
        }
        Err(aggregate_errors(errors_vec))
    }

    fn decl(&mut self) -> Result<usize, ParseError> {
        todo!()
    }

    fn assign(&mut self) -> Result<usize, ParseError> {
        let token = &self.token_vec[self.lookahead];
        match token.core_token {
            CoreToken::IDENTIFIER(_) => {
                self.expect("id")?;
                self.expect("=")?;
                self.expr()?;
                Ok(self.lookahead)
            },
            _ => Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, self.lookahead, "expected an identifier on left side of the assignment")))
        }
    }

    fn expr(&mut self) -> Result<usize, ParseError> {
        todo!()
    }

    fn expect_zero_or_more<F: FnMut() -> Result<usize, ParseError>>(mut f: F, curr_lookahead: usize) -> Result<usize, ParseError> {
        let mut curr_lookahead = curr_lookahead;
        loop {
            match f() {
                Ok(lookahead) => {
                    curr_lookahead = lookahead;
                    continue;
                },
                Err(_) => {
                    // TODO - propogate this error too outside!
                    return Ok(curr_lookahead);
                }
            }
        }
    }

    fn expect_optionally<F: FnMut() -> Result<usize, ParseError>>(mut f: F, curr_lookahead: usize) -> Result<usize, ParseError> {
        match f() {
            Ok(lookahead) => Ok(lookahead),
            Err(_) => {
                // TODO - propogate this error too outside!
                Ok(curr_lookahead)
            }
        }
    }

    fn expect(&mut self, symbol: &str) -> Result<usize, ParseError> {
        let token = &self.token_vec[self.lookahead];
        if token.is_eq(symbol) {
            self.lookahead = self.lookahead + 1;
            Ok(self.lookahead)
        } else {
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, 
                self.lookahead, "expected ")))
        }
    }
}