// Default parser for jarvil uses Packrat approach, first given by Bryan Ford in his master thesis at MIT. It is essentially a 
// top down recursive descent parsing with lazy memoization in order to avoid exponential parse time and provide reliable 
// linear time parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use crate::ast::ast::{IdentifierNode, TypeExpressionNode};
use crate::lexer::token::{Token, CoreToken};
use crate::scope::identifier;
use std::rc::Rc;
use crate::errors::{SyntaxError};
use crate::context;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use crate::types::core::{Type};

pub trait Parser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), SyntaxError>;  // return an AST
}

#[derive(Debug)]
pub enum RoutineCache {
    // currently only two routine (atom, expr) results are cached by the parser
    ATOM(Rc<RefCell<FxHashMap<usize, Result<(ParseSuccess, Option<Type>, bool, bool), SyntaxError>>>>),
    EXPR(Rc<RefCell<FxHashMap<usize, Result<(ParseSuccess, bool), SyntaxError>>>>),
}

#[derive(Debug)]
pub struct ParseSuccess {
    pub lookahead: usize,
    pub possible_err: Option<SyntaxError>,
}

pub struct PackratParser {
    token_vec: Vec<Token>,
    lookahead: usize,
    indent_level: i64,
    code_lines: Vec<(Rc<String>, usize)>,
    cache: Vec<Rc<RoutineCache>>,
    // TODO - add AST data structure
}

impl PackratParser {
    pub fn new(code_lines: Vec<(Rc<String>, usize)>) -> Self {
        let atom_cache_map: FxHashMap<usize, Result<(ParseSuccess, Option<Type>, bool, bool), SyntaxError>> 
        = FxHashMap::default();
        let expr_cache_map: FxHashMap<usize, Result<(ParseSuccess, bool), SyntaxError>> = FxHashMap::default();
        PackratParser {
            token_vec: Vec::new(),
            lookahead: 0,
            indent_level: -1,
            code_lines,
            cache: vec![
                Rc::new(RoutineCache::ATOM(Rc::new(RefCell::new(atom_cache_map)))),
                Rc::new(RoutineCache::EXPR(Rc::new(RefCell::new(expr_cache_map)))),
            ],
        }
    }
}

impl Parser for PackratParser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), SyntaxError> {
        //self.code(token_vec)?;
        Ok(())
    }
}

impl PackratParser {
    // parsing utilities
    pub fn get_lookahead(&self) -> usize {
        self.lookahead
    }

    pub fn reset_lookahead(&mut self, reset_index: usize) {
        self.lookahead = reset_index;
    }

    pub fn get_indent_level(&self) -> i64 {
        self.indent_level
    }

    pub fn reset_indent_level(&mut self, reset_indent: i64) {
        self.indent_level = reset_indent;
    }

    pub fn get_code_line(&self, mut curr_line_number: usize, index: usize) -> (Rc<String>, usize, usize, usize) {
        loop {
            let (s, line_start_index) = &self.code_lines[curr_line_number - 1];
            if index >= *line_start_index {
                return (s.clone(), *line_start_index, curr_line_number, index)
            }
            curr_line_number = curr_line_number - 1;
        }
    }

    pub fn set_token_vec(&mut self, token_vec: Vec<Token>) {
        self.token_vec = token_vec;
    }

    pub fn get_curr_line_number(&self) -> usize {
        self.token_vec[self.lookahead].line_number
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

    pub fn get_curr_core_token(&mut self) -> &CoreToken {
        self.ignore_blanks();
        &self.token_vec[self.lookahead].core_token
    }

    pub fn get_curr_token_name(&mut self) -> Rc<String> {
        self.ignore_blanks();
        self.token_vec[self.lookahead].name.clone()
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

    // parsing routines for terminals
    pub fn expect(&mut self, symbol: &str) -> Result<ParseSuccess, SyntaxError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        if String::from("empty").eq(symbol) {
            return Ok(ParseSuccess{
                lookahead: self.lookahead,
                possible_err: None,
            })
        }
        if token.is_eq(symbol) {
            self.lookahead = self.lookahead + 1;
            Ok(ParseSuccess{
                lookahead: self.lookahead,
                possible_err: None,
            })
        } else {
            return Err(SyntaxError::new(
                self.get_code_line(token.line_number, token.index()),
                format!(
                "expected '{}', got '{}'",
                PackratParser::parse_for_err_message(String::from(symbol)), 
                PackratParser::parse_for_err_message(token.name.to_string())))
            )
        }
    }

    pub fn expect_int(&mut self) -> Result<(ParseSuccess, Rc<String>), SyntaxError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::INTEGER(token_value) => {
                self.lookahead = self.lookahead + 1;
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, token_value.0.clone()))
            },
            _ => {
                return Err(SyntaxError::new(
                    self.get_code_line(token.line_number, token.index()),
                    format!(
                    "expected positive integer, got '{}'",
                    PackratParser::parse_for_err_message(token.name.to_string())))
                )
            }
        }
    }

    pub fn expect_float(&mut self) -> Result<(ParseSuccess, Rc<String>), SyntaxError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::FLOAT(token_value) => {
                self.lookahead = self.lookahead + 1;
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, token_value.0.clone()))
            },
            _ => {
                return Err(SyntaxError::new(
                    self.get_code_line(token.line_number, token.index()),
                    format!(
                    "expected floating-point number, got '{}'",
                    PackratParser::parse_for_err_message(token.name.to_string())))
                )
            }
        }
    }

    pub fn expect_literal(&mut self) -> Result<(ParseSuccess, Rc<String>), SyntaxError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::LITERAL(token_value) => {
                self.lookahead = self.lookahead + 1;
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, token_value.0.clone()))
            },
            _ => {
                return Err(SyntaxError::new(
                    self.get_code_line(token.line_number, token.index()),
                    format!(
                    "expected string literal, got '{}'",
                    PackratParser::parse_for_err_message(token.name.to_string())))
                )
            }
        }
    }

    pub fn expect_ident(&mut self) -> Result<(ParseSuccess, IdentifierNode), SyntaxError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::IDENTIFIER(token_value) => {
                self.lookahead = self.lookahead + 1;
                let node = IdentifierNode::new(&token_value.0, token.start_index, 
                    token.end_index, token.line_number);
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, node))
            },
            _ => {
                return Err(SyntaxError::new(
                    self.get_code_line(token.line_number, token.index()),
                    format!(
                    "expected identifier, got '{}'",
                    PackratParser::parse_for_err_message(token.name.to_string())))
                )
            }
        }
    }

    pub fn expect_type_expr(&mut self) -> Result<(ParseSuccess, TypeExpressionNode), SyntaxError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::ATOMIC_TYPE(atomic_type) => {
                self.lookahead = self.lookahead + 1;
                let node = TypeExpressionNode::new_with_atomic_type(&atomic_type.0);
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, node))
            },
            CoreToken::IDENTIFIER(_) => {
                let (_, identifier_node) = self.expect_ident()?;
                let node = TypeExpressionNode::new_with_user_defined_type(&identifier_node);
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, node))
            },
            CoreToken::LSQUARE => {
                self.expect("[")?;
                let (_, sub_type_node) = self.expect_type_expr()?;
                self.expect(",")?;
                let (_, array_size) = self.expect_int()?;
                self.expect("]")?;
                let node = TypeExpressionNode::new_with_array_type(array_size, &sub_type_node);
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, node))
            },
            _ => {
                return Err(SyntaxError::new(
                    self.get_code_line(token.line_number, token.index()),
                    format!(
                    "expected 'int', 'float', 'bool', 'string', identifier or '[', got '{}'",
                    PackratParser::parse_for_err_message(token.name.to_string())))
                )
            }
        }
    }

    pub fn expect_indent_spaces(&mut self) -> Result<(ParseSuccess, i64), SyntaxError> {
        let expected_indent_spaces = context::get_indent() * self.indent_level;
        let mut indent_spaces = 0;
        loop {
            let token = &self.token_vec[self.lookahead];
            match &token.core_token {
                CoreToken::BLANK => indent_spaces = indent_spaces + 1,
                CoreToken::NEWLINE => indent_spaces = 0,
                CoreToken::TAB => {
                    let err = SyntaxError::new(
                        self.get_code_line(token.line_number, token.index()),
                        String::from(
                        "incorrectly indented statement\n    tabs are not allowed for indentation")
                    );
                    return Err(err)
                },
                _ => {
                    if indent_spaces == expected_indent_spaces {
                        return Ok((ParseSuccess{
                            lookahead: self.lookahead,
                            possible_err: None,
                        }, indent_spaces))
                    } else {
                        let err = SyntaxError::new(
                            self.get_code_line(token.line_number, token.index()),
                            format!(
                                "incorrectly indented statement\n    expected indent of '{}' spaces, got '{}' spaces", 
                                expected_indent_spaces, indent_spaces));
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

    pub fn expect_zero_or_more<F: FnMut() -> Result<ParseSuccess, SyntaxError>>(mut f: F, 
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

    pub fn expect_optionally<T, F: FnMut() -> Result<T, SyntaxError>>(mut f: F, curr_value: T) -> (bool, T, Option<SyntaxError>) {
        match f() {
            Ok(response) => (true, response, None),
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

    pub fn get_or_set_cache<T: std::fmt::Debug,
    F: FnOnce(&mut PackratParser) -> Result<T, SyntaxError>, 
    G: FnOnce(&Result<T, SyntaxError>) -> Result<T, SyntaxError>,
    H: FnOnce(&T) -> usize>(
        &mut self,
        cache_map: &Rc<RefCell<FxHashMap<usize, Result<T, SyntaxError>>>>,
        routine_fn: F,
        clone_result_fn: G,
        get_lookahead_fn: H,
        curr_lookahead: usize,
        message: &str,
    ) -> Result<T, SyntaxError> {
        match cache_map.borrow().get(&curr_lookahead) {
            Some(result) => {
                let result = clone_result_fn(result);
                match result {
                    Ok(response) => {
                        self.reset_lookahead(get_lookahead_fn(&response));
                        return Ok(response)
                    },
                    Err(err) => return Err(err)
                }
            },
            _ => {}
        }
        let result = routine_fn(self);
        let result_entry = clone_result_fn(&result);
        // println!("cache missed: pushing the entry = {:?} in map of {}", result_entry, message);
        cache_map.borrow_mut().insert(curr_lookahead, result_entry);
        result
    }

    // ------------------- production rule matching function for terminals and non-terminals declared below -------------------
    // code
}