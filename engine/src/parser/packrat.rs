// Default parser for jarvil is Packrat developed by Bryan Ford in his master thesis at MIT. It is essentially a top down 
// recursive parsing with optimized backtracting in order to avoid exponential parse time and provide reliable linear time 
// parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use crate::parser::core::Parser;
use crate::lexer::token::{Token, CoreToken, TokenValue};
use crate::parser::ast::AST;
use std::rc::Rc;
use crate::errors::{ParseError, SyntaxError};
use crate::env::{Env, SymbolData};
use crate::parser::components;
use crate::context;

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
    pub fn get_lookahead(&self) -> usize {
        self.lookahead
    }

    pub fn reset_lookahead(&mut self, reset_index: usize) {
        self.lookahead = reset_index;
    }

    pub fn get_indent_level(&self) -> usize {
        self.indent_level
    }

    pub fn reset_indent_level(&mut self, reset_indent: usize) {
        self.indent_level = reset_indent;
    }

    pub fn get_env(&self) -> Env {
        Env(self.env.0.clone())
    }

    pub fn reset_env(&mut self, reset_env: &Env) {
        self.env = Env(reset_env.0.clone())
    }

    pub fn set_new_env_for_block(&mut self) {
        let curr_env = Env(self.env.0.clone());
        let env = Env::new_with_parent_env(&curr_env);
        self.env = env;
    }

    pub fn set_scope(&mut self, token_value: &TokenValue, data_type: &Rc<String>, is_init: bool) {
        self.env.set(token_value, data_type, is_init);
    }

    pub fn set_token_vec(&mut self, token_vec: Vec<Token>) {
        self.token_vec = token_vec;
    }

    pub fn get_curr_line_number(&self) -> usize {
        // self.ignore_blanks();
        self.token_vec[self.lookahead].line_number
    }

    pub fn get_curr_token_value(&mut self) -> Option<TokenValue> {
        self.ignore_blanks();
        self.token_vec[self.lookahead].get_value()
    }

    pub fn get_curr_core_token(&mut self) -> &CoreToken {
        self.ignore_blanks();
        &self.token_vec[self.lookahead].core_token
    }

    pub fn get_curr_token_name(&mut self) -> Rc<String> {
        self.ignore_blanks();
        self.token_vec[self.lookahead].name.clone()
    }

    pub fn get_next_token_name(&self) ->Rc<String> {
        let mut temp_lookahead = self.lookahead;
        loop {
            let token = &self.token_vec[temp_lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    temp_lookahead = temp_lookahead + 1;
                },
                _ => {
                    return token.name.clone();
                }
            }
        }
    }

    pub fn check_next_token(&self, symbol: &str) -> bool {
        let mut temp_lookahead = self.lookahead;
        loop {
            let token = &self.token_vec[temp_lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    temp_lookahead = temp_lookahead + 1;
                },
                _ => {
                    if token.is_eq(symbol) {
                        return true
                    } else {
                        return false
                    }
                }
            }
        }
    }

    pub fn ignore_blanks(&mut self) {
        loop {
            let token = &self.token_vec[self.lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    self.lookahead = self.lookahead + 1;
                },
                _ => return
            }
        }
    }

    // all production rule matching function declared below:

    // code
    pub fn code(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        components::code::code(self, token_vec)
    }

    pub fn block(&mut self) -> Result<ParseSuccess, ParseError> {
        components::block::block(self)
    }

    // statements
    pub fn stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::stmt::stmt(self)
    }

    pub fn compound_stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::core::compound_stmt(self)
    }

    pub fn simple_stmts(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::core::simple_stmts(self)
    }

    pub fn simple_stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::core::simple_stmt(self)
    }

    // simple statement - decl, assign
    pub fn decl(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::declaration::decl(self)
    }

    pub fn assign(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::assignment::assign(self)
    }

    pub fn r_asssign(&mut self, rule_index: usize, line_number: usize) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::helper::r_asssign(self, rule_index, line_number)
    }

    // expression
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

    pub fn factor_expr_in_parenthesis(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::factor_expr_in_parenthesis(self)
    }

    pub fn multitive_star(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::multitive_star(self)
    }

    pub fn multitive_slash(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::multitive_slash(self)
    }

    pub fn additive_plus(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::additive_plus(self)
    }

    pub fn additive_minus(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::additive_minus(self)
    }

    // boolean expression
    pub fn bexpr(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bexpr(self)
    }

    pub fn ortive(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::ortive(self)
    }

    pub fn bterm(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bterm(self)
    }

    pub fn bfactor(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor(self)
    }

    pub fn bfactor_lookahead_one(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor_lookahead_one(self)
    }

    pub fn andtive(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::andtive(self)
    }

    pub fn comp_op(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::comp_op(self)
    }

    pub fn bfactor_expr_comp_op_expr(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor_expr_comp_op_expr(self)
    }

    pub fn bfactor_expr_in_parenthesis(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor_expr_in_parenthesis(self)
    }

    pub fn bfactor_not(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor_not(self)
    }

    pub fn andtive_and(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::andtive_and(self)
    }

    pub fn ortive_or(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::ortive_or(self)
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
                format!(
                "expected '{}', got '{}'",
                PackratParser::parse_for_err_message(String::from(symbol)), 
                PackratParser::parse_for_err_message(token.name.to_string()))))
            )
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
                  format!("expected a type, got '{}'", 
                  PackratParser::parse_for_err_message( token.name.to_string())))))
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
                self.lookahead, format!(
                "expected '{}', got '{}'", 
                PackratParser::parse_for_err_message(String::from(symbol)), 
                PackratParser::parse_for_err_message(token.name.to_string()))))
            )
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
                    self.lookahead, format!(
                        "expected an identifier, got '{}'", 
                        PackratParser::parse_for_err_message(token.name.to_string())))))
            }
        }
    }

    pub fn expect_indent_spaces(&mut self) -> Result<(ParseSuccess, usize), ParseError> {
        let expected_indent_spaces = context::get_indent() * self.indent_level;
        let mut indent_spaces: usize = 0;
        loop {
            let token = &self.token_vec[self.lookahead];
            match &token.core_token {
                CoreToken::BLANK => indent_spaces = indent_spaces + 1,
                CoreToken::NEWLINE => indent_spaces = 0,
                CoreToken::TAB => unimplemented!("yet to handle tabs in indentation"),
                _ => {
                    if indent_spaces == expected_indent_spaces {
                        return Ok((ParseSuccess{
                            lookahead: self.lookahead,
                            possible_err: None,
                        }, indent_spaces))
                    } else {
                        let err =ParseError::SYNTAX_ERROR(SyntaxError::new(token.line_number,
                            self.lookahead, format!(
                                "indentation of the statement does not match. expected indent '{}' spaces, got '{}' spaces", 
                                expected_indent_spaces, indent_spaces)));
                        return Ok((ParseSuccess{
                            lookahead: self.lookahead,
                            possible_err: Some(err),
                        }, indent_spaces))
                    }
                }
            }
            self.lookahead = self.lookahead + 1;
        }
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
                (false, curr_value, Some(err))
            }
        }
    }

    pub fn parse_for_err_message(message: String) -> String {
        let mut parsed_message = message;
        if parsed_message.eq("\n") {
            parsed_message = String::from("newline")
        }
        parsed_message
    }
}