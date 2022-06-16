// Default parser for jarvil is Packrat developed by Bryan Ford in his master thesis at MIT. It is essentially a top down 
// recursive parsing with optimized backtracting in order to avoid exponential parse time and provide reliable linear time 
// parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use std::vec;
use crate::parser::core::Parser;
use crate::lexer::token::{Token, CoreToken, TokenValue};
use crate::parser::ast::AST;
use std::rc::Rc;
use crate::errors::{ParseError, SyntaxError, SemanticError, aggregate_errors};
use crate::env::{Env, SymbolData};
use crate::parser::components;

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
    pub fn get_lookahead(&mut self) -> usize {
        self.lookahead
    }

    pub fn reset_lookahead(&mut self, reset_index: usize) {
        self.lookahead = reset_index;
    }

    pub fn set_scope(&mut self, token_value: &TokenValue, data_type: &Rc<String>) {
        self.env.set(token_value, data_type);
    }

    pub fn code(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        let mut errors_vec: Vec<ParseError> = vec![];
        self.token_vec = token_vec;
        self.stmt()?;
        let curr_lookahead = self.lookahead;
        let (new_lookahead, err) = PackratParser::expect_zero_or_more(|| {
            let lookahead = self.stmt()?;
            Ok(lookahead)
        }, curr_lookahead);
        self.reset_lookahead(new_lookahead);
        if let Some(err) = err {
            errors_vec.push(err);
        }
        match self.expect("endmarker") {
            Ok((_, _)) => {
                return Ok(());
            },
            Err(err) => errors_vec.push(err)
        }
        Err(aggregate_errors(errors_vec))
    }

    pub fn stmt(&mut self) -> Result<usize, ParseError> {
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

    pub fn compound_stmt(&mut self) -> Result<usize, ParseError> {
        // TODO - add production rules for compound statements - see jarvil.gram
        Err(ParseError::SYNTAX_ERROR(SyntaxError::new(1000, 0, "This is just a demo error to test")))
        // Ok(self.lookahead)
    }

    pub fn simple_stmts(&mut self) -> Result<usize, ParseError> {
        let mut errors_vec: Vec<ParseError> = vec![];
        self.simple_stmt()?;
        self.expect("\n")?;
        let curr_lookahead = self.lookahead;
        match self.simple_stmts() {
            Ok(lookahead) => return Ok(lookahead),
            Err(err) => {
                self.reset_lookahead(curr_lookahead);
                errors_vec.push(err);
            }
        }
        match self.expect("empty") {
            Ok((lookahead, _)) => return Ok(lookahead),
            Err(err) => {
                self.reset_lookahead(curr_lookahead);
                errors_vec.push(err);
            }
        }
        Err(aggregate_errors(errors_vec))
    }

    pub fn simple_stmt(&mut self) -> Result<usize, ParseError> {
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

    pub fn decl(&mut self) -> Result<usize, ParseError> {
        components::simple_stmt::declaration::decl(self)
    }

    pub fn assign(&mut self) -> Result<usize, ParseError> {
        components::simple_stmt::assignment::assign(self)
    }

    pub fn expr(&mut self) -> Result<(usize, bool), ParseError> {
        components::expression::expression::expr(self)
    }

    pub fn term(&mut self) -> Result<(usize, bool), ParseError> {
        components::expression::expression::term(self)
    }

    pub fn additive(&mut self) -> Result<(usize, bool), ParseError> {
        components::expression::expression::additive(self)
    }

    pub fn factor(&mut self) -> Result<(usize, bool), ParseError> {
        components::expression::expression::factor(self)
    }

    pub fn multitive(&mut self) -> Result<(usize, bool), ParseError> {
        components::expression::expression::multitive(self)
    }

    pub fn bexpr(&mut self) -> Result<usize, ParseError> {
        components::expression::bexpression::bexpr(self)
    }

    pub fn expect_zero_or_more<F: FnMut() -> Result<usize, ParseError>>(mut f: F, 
        initial_lookahead: usize) -> (usize, Option<ParseError>) {
        let mut curr_lookahead = initial_lookahead;
        loop {
            match f() {
                Ok(lookahead) => {
                    curr_lookahead = lookahead;
                    continue;
                },
                Err(err) => {
                    return (curr_lookahead, Some(err));
                }
            }
        }
    }

    pub fn expect_optionally<T, F: FnMut() -> Result<T, ParseError>>(mut f: F, curr_value: T) -> Result<(bool, T), ParseError> {
        match f() {
            Ok(lookahead) => Ok((true, lookahead)),
            Err(_) => {
                // TODO - propogate this error too outside!
                Ok((false, curr_value))
            }
        }
    }

    pub fn expect(&mut self, symbol: &str) -> Result<(usize, usize), ParseError> {
        let token = &self.token_vec[self.lookahead];
        if String::from("empty").eq(symbol) {
            return Ok((self.lookahead, token.line_number))
        }
        if token.is_eq(symbol) {
            self.lookahead = self.lookahead + 1;
            Ok((self.lookahead, token.line_number))
        } else {
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, 
                self.lookahead, "expected ")))
        }
    }

    pub fn expect_type(&mut self) -> Result<(usize, usize, Rc<String>), ParseError> {
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::TYPE(token_value) => {
                self.lookahead = self.lookahead + 1;
                Ok((self.lookahead, token.line_number, token_value.0.clone()))
            },
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_table = token.check_declaration(&self.env, self.lookahead)?;
                if symbol_table.is_type() {
                    Ok((self.lookahead, token.line_number, token_value.0.clone()))
                } else {
                    Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, 
                        self.lookahead, "expected a type, identifier is not a type")))
                }
            },
            _ => Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number,
                 self.lookahead, "expected a type")))
        }
    }

    pub fn expect_and_get_value(&mut self, symbol: &str) -> Result<(usize, usize, TokenValue), ParseError> {
        let token = &self.token_vec[self.lookahead];
        if token.is_eq(symbol) {
            let (has_value, token_value) = match &token.core_token {
                CoreToken::IDENTIFIER(token_value)  =>      {
                    (true, Some(token_value))
                },
                CoreToken::INTEGER(token_value)     =>      {
                    (true, Some(token_value))
                },
                CoreToken::FLOAT(token_value)       =>      {
                    (true, Some(token_value))
                },
                CoreToken::LITERAL(token_value)     =>      {
                    (true, Some(token_value))
                },
                CoreToken::TYPE(token_value)        =>      {
                    (true, Some(token_value))
                }
                _ => {
                    (false, None)
                }
            };
            if has_value {
                if let Some(token_value) = token_value {
                    self.lookahead = self.lookahead + 1;
                    Ok((self.lookahead, token.line_number, TokenValue(token_value.0.clone())))
                } else {
                    unreachable!("any token which has a value can't be reached here")
                }
            } else {
                unreachable!("this method should only be called for tokens which have values")
            }
        } else {
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, 
                self.lookahead, "expected ")))
        }
    }

    // always use this for matching identifiers except in declarations
    pub fn expect_id_and_get_data(&mut self) -> Result<(usize, usize, SymbolData), ParseError> {
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::IDENTIFIER(_) => {
                let symbol_data = token.check_declaration(&self.env, self.lookahead)?;
                self.lookahead = self.lookahead + 1;
                Ok((self.lookahead, token.line_number, symbol_data))
            },
            _ => {
                Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number,
                    self.lookahead, "expected an identifier")))
            }
        }
    }
}