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

pub struct ParseSuccess {
    pub lookahead: usize,
    pub possible_err: Option<ParseError>,
}

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

    pub fn get_core_token(&mut self) -> &CoreToken {
        self.ignore_blanks();
        &self.token_vec[self.lookahead].core_token
    }

    pub fn get_line_number(&mut self) -> usize {
        self.ignore_blanks();
        self.token_vec[self.lookahead].line_number
    }

    pub fn get_token_name(&mut self) -> Rc<String> {
        self.ignore_blanks();
        self.token_vec[self.lookahead].name.clone()
    }

    pub fn code(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        let mut errors_vec: Vec<ParseError> = vec![];
        self.token_vec = token_vec;
        self.stmt()?;
        let curr_lookahead = self.lookahead;
        let response = PackratParser::expect_zero_or_more(|| {
            let response = self.stmt()?;
            Ok(response)
        }, curr_lookahead);
        self.reset_lookahead(response.lookahead);
        if let Some(err) = response.possible_err {
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

    pub fn stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        let mut errors_vec: Vec<ParseError> = vec![];
        // TODO - handle lookahead index to reset it if a production fails
        // Below is a general pattern among many production rule cases where we always have to reset lookahead back to the original
        // value when trying out new production rule after prior one failed
        let curr_lookahead = self.lookahead;
        match self.compound_stmt() {
            Ok(response) => return Ok(response),
            Err(err) => {
                self.reset_lookahead(curr_lookahead);
                errors_vec.push(err);
            }
        }
        match self.simple_stmts() {
            Ok(response) => return Ok(response),
            Err(err) => {
                self.reset_lookahead(curr_lookahead);
                errors_vec.push(err);
            }
        }
        Err(aggregate_errors(errors_vec))
    }

    pub fn compound_stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        // TODO - add production rules for compound statements - see jarvil.gram
        Err(ParseError::SYNTAX_ERROR(SyntaxError::new(1000, 0, 
            String::from("This is just a demo error to test"))))
        // Ok(self.lookahead)
    }

    pub fn simple_stmts(&mut self) -> Result<ParseSuccess, ParseError> {
        let mut errors_vec: Vec<ParseError> = vec![];
        let response = self.simple_stmt()?;
        if let Some(err) = response.possible_err {
            errors_vec.push(err);
        }
        match self.expect("\n") {
            Ok((_, _)) => {
                return Ok(ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                });
            },
            Err(err) => errors_vec.push(err)
        }
        Err(aggregate_errors(errors_vec))
        /*
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
         */
    }

    pub fn simple_stmt(&mut self) -> Result<ParseSuccess, ParseError> {
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

    pub fn decl(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::declaration::decl(self)
    }

    pub fn assign(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::assignment::assign(self)
    }

    pub fn expr(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::expr(self)
    }

    pub fn term(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::term(self)
    }

    pub fn additive(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::additive(self)
    }

    pub fn factor(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::factor(self)
    }

    pub fn multitive(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::multitive(self)
    }

    pub fn bexpr(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bexpr(self)
    }

    pub fn expect_zero_or_more<F: FnMut() -> Result<ParseSuccess, ParseError>>(mut f: F, 
        initial_lookahead: usize) -> ParseSuccess {
        let mut curr_lookahead = initial_lookahead;
        loop {
            match f() {
                Ok(response) => {
                    curr_lookahead = response.lookahead;
                    continue;
                },
                Err(err) => {
                    return ParseSuccess{
                        lookahead: curr_lookahead,
                        possible_err: Some(err)
                    };
                }
            }
        }
    }

    pub fn expect_optionally<T, F: FnMut() -> Result<T, ParseError>>(mut f: F, curr_value: T) -> (bool, T, Option<ParseError>) {
        match f() {
            Ok(lookahead) => (true, lookahead, None),
            Err(err) => {
                // TODO - propogate this error too outside!
                println!("inside optionally: {:?}", err);
                (false, curr_value, Some(err))
            }
        }
    }

    pub fn ignore_blanks(&mut self) {
        loop {
            let token = &self.token_vec[self.lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    // println!("{}", self.lookahead);
                    self.lookahead = self.lookahead + 1;
                },
                _ => return
            }
        }
    }

    pub fn expect(&mut self, symbol: &str) -> Result<(ParseSuccess, usize), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        if String::from("empty").eq(symbol) {
            return Ok((ParseSuccess{
                lookahead: self.lookahead,
                possible_err: None,
            }, token.line_number))
        }
        if token.is_eq(symbol) {
            self.lookahead = self.lookahead + 1;
            Ok((ParseSuccess{
                lookahead: self.lookahead,
                possible_err: None,
            }, token.line_number))
        } else {
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, 
                self.lookahead, 
                format!("expected '{}', got '{}'", symbol, token.name))))
        }
    }

    pub fn expect_type(&mut self) -> Result<(ParseSuccess, usize, Rc<String>), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::TYPE(token_value) => {
                self.lookahead = self.lookahead + 1;
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, token.line_number, token_value.0.clone()))
            },
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_table = token.check_declaration(&self.env, self.lookahead)?;
                if symbol_table.is_type() {
                    Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, token_value.0.clone()))
                } else {
                    Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, 
                        self.lookahead, 
                        String::from("expected a type, got an identifier"))))
                }
            },
            _ => Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number,
                 self.lookahead,
                  format!("expected a type, got '{}'", token.name))))
        }
    }

    pub fn expect_and_get_value(&mut self, symbol: &str) -> Result<(ParseSuccess, usize, TokenValue), ParseError> {
        self.ignore_blanks();
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
                    Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, TokenValue(token_value.0.clone())))
                } else {
                    unreachable!("any token which has a value can't be reached here")
                }
            } else {
                unreachable!("this method should only be called for tokens which have values")
            }
        } else {
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number, 
                self.lookahead, format!("expected '{}', got '{}'", symbol, token.name))))
        }
    }

    // always use this for matching identifiers except in declarations
    pub fn expect_id_and_get_data(&mut self) -> Result<(ParseSuccess, usize, SymbolData), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::IDENTIFIER(_) => {
                let symbol_data = token.check_declaration(&self.env, self.lookahead)?;
                self.lookahead = self.lookahead + 1;
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, token.line_number, symbol_data))
            },
            _ => {
                Err(ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number,
                    self.lookahead, format!("expected an identifier, got '{}'", token.name))))
            }
        }
    }
}