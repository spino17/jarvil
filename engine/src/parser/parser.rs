// Default parser for jarvil uses Packrat approach, first given by Bryan Ford in his master thesis at MIT. It is essentially a 
// top down recursive descent parsing with lazy memoization in order to avoid exponential parse time and provide reliable 
// linear time parsing!
// See `https://pdos.csail.mit.edu/~baford/packrat/thesis/` for more information.

use crate::ast::ast::{TypeExpressionNode, StatementNode, ParamNode, BlockNode, ASTNode, TokenNode};
use crate::ast::helper::IndentNode;
use crate::lexer::token::{Token, CoreToken, ErrorToken, MissingToken};
use std::rc::Rc;
use crate::errors::{SyntaxError};
use crate::context;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use crate::types::core::{Type};
use crate::parser::components;

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
    // errors: Vec<SyntaxError>  // reported errors during the parsing
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
    pub fn get_curr_lookahead(&self) -> usize {
        self.lookahead
    }

    pub fn reset_lookahead(&mut self, reset_index: usize) {
        self.lookahead = reset_index;
    }

    pub fn scan_next_token(&mut self) {
        self.lookahead = self.lookahead + 1;
    }

    pub fn get_curr_indent_level(&self) -> i64 {
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

    pub fn get_curr_token(&mut self) -> Token {
        self.ignore_whitespaces();
        self.token_vec[self.lookahead].clone()
    }

    pub fn get_curr_token_name(&mut self) -> Rc<String> {
        self.ignore_whitespaces();
        self.token_vec[self.lookahead].name.clone()
    }

    pub fn get_previous_token(&mut self) -> Token {
        self.token_vec[self.lookahead - 1].clone()
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
                    return token.is_eq(symbol)
                }
            }
        }
    }

    pub fn ignore_whitespaces(&mut self) {
        loop {
            let token = &self.token_vec[self.lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    self.scan_next_token();
                },
                CoreToken::TAB => {
                    self.scan_next_token();
                },
                _ => return
            }
        }
    }

    pub fn ignore_whitespaces_and_newlines(&mut self) {
        loop {
            let token = &self.token_vec[self.lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    self.scan_next_token();
                },
                CoreToken::TAB => {
                    self.scan_next_token();
                },
                CoreToken::NEWLINE => {
                    self.scan_next_token();
                }
                _ => return
            }
        }
    }

    pub fn ignore_whitespaces_and_optionally_newlines(&mut self, ignore_newline: bool) {
        if ignore_newline {
            self.ignore_whitespaces_and_newlines();
        } else {
            self.ignore_whitespaces();
        }
    }

    // parsing routines for terminals
    pub fn expect(&mut self, symbol: &str, ignore_newline: bool) -> TokenNode {
        self.ignore_whitespaces_and_optionally_newlines(ignore_newline);
        let token = self.get_curr_token();
        if token.is_eq(symbol) {
            self.scan_next_token();
            TokenNode::new_with_token(&token)
        } else {
            // TODO - return token.clone() and symbol in missing token
            TokenNode::new_with_missing_token(&Rc::new(String::from(symbol)), &token)
        }
    }

    pub fn expect_int(&mut self, ignore_newline: bool) -> TokenNode {
        self.ignore_whitespaces_and_optionally_newlines(ignore_newline);
        let token = self.get_curr_token();
        match &token.core_token {
            CoreToken::INTEGER(_) => {
                self.scan_next_token();
                TokenNode::new_with_token(&token)
            },
            _ => {
                TokenNode::new_with_missing_token(&Rc::new(String::from("int")), &token)
            }
        }
    }

    pub fn expect_float(&mut self, ignore_newline: bool) -> TokenNode {
        self.ignore_whitespaces_and_optionally_newlines(ignore_newline);
        let token = self.get_curr_token();
        match &token.core_token {
            CoreToken::FLOAT(_) => {
                self.scan_next_token();
                TokenNode::new_with_token(&token)
            },
            _ => {
                TokenNode::new_with_missing_token(&Rc::new(String::from("float")), &token)
            }
        }
    }

    pub fn expect_literal(&mut self, ignore_newline: bool) -> TokenNode {
        self.ignore_whitespaces_and_optionally_newlines(ignore_newline);
        let token = self.get_curr_token();
        match &token.core_token {
            CoreToken::LITERAL(_) => {
                self.scan_next_token();
                TokenNode::new_with_token(&token)
            },
            _ => {
                TokenNode::new_with_missing_token(&Rc::new(String::from("string literal")), &token)
            }
        }
    }

    pub fn expect_ident(&mut self, ignore_newline: bool) -> TokenNode {
        self.ignore_whitespaces_and_optionally_newlines(ignore_newline);
        let token = self.get_curr_token();
        match &token.core_token {
            CoreToken::IDENTIFIER(_) => {
                self.scan_next_token();
                TokenNode::new_with_token(&token)
            },
            _ => {
                TokenNode::new_with_missing_token(&Rc::new(String::from("identifier")), &token)
            }
        }
    }

    pub fn expect_indent_spaces(&mut self, saved_lookahead: usize, 
        stmts: &Rc<RefCell<Vec<StatementNode>>>, params: &Rc<Vec<ParamNode>>, parent: &Option<ASTNode>) -> IndentNode {
        let expected_indent_spaces = context::get_indent() * self.indent_level;
        let mut indent_spaces = 0;
        loop {
            let token = &self.token_vec[self.lookahead];
            match &token.core_token {
                CoreToken::BLANK => indent_spaces = (token.end_index - token.start_index) as i64,
                CoreToken::NEWLINE => indent_spaces = 0,
                CoreToken::TAB => {
                    let err = SyntaxError::new(
                        self.get_code_line(token.line_number, token.index()),
                        String::from(
                        "incorrectly indented statement\n    tabs are not allowed for indentation")
                    );
                    todo!()
                },
                _ => {
                    if indent_spaces == expected_indent_spaces {
                        return IndentNode::TOKEN(TokenNode::new_with_token(&self.get_previous_token()))
                    } else {
                        let indent_spaces_unit = context::get_indent();
                        let indent_factor = indent_spaces / indent_spaces_unit as i64;
                        let indent_remainder = indent_spaces - indent_factor * indent_spaces_unit;
                        if indent_remainder > 0 {
                            // TODO - handle indentation error here
                            todo!()
                        } else {
                            if indent_spaces > indent_spaces_unit * self.get_curr_indent_level() {
                                // TODO - handle indentation error here
                                todo!()
                            } else {
                                // block is over
                                self.reset_indent_level(self.get_curr_indent_level() - 1);
                                self.reset_lookahead(saved_lookahead);
                                return IndentNode::BLOCK(BlockNode::new(stmts, params, parent.clone()))
                            }
                        }
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
    pub fn code(&mut self, token_vec: Vec<Token>) -> BlockNode {
        components::code::code(self, token_vec)
    }

    pub fn check_block_indentation(&mut self, 
        indent_spaces: i64, err: SyntaxError, curr_lookahead: usize, 
        params: &Rc<Vec<ParamNode>>, stmts: &Rc<RefCell<Vec<StatementNode>>>, 
        parent: Option<ASTNode>) -> BlockNode {
        components::block::check_block_indentation(self, indent_spaces, err, curr_lookahead, params, stmts, parent)
    }

    pub fn block(&mut self, params: &Rc<Vec<ParamNode>>, parent: Option<ASTNode>) -> BlockNode {
        components::block::block(self, params, parent)
    }

    // statements
    pub fn stmt(&mut self) -> StatementNode {
        components::stmt::stmt(self)
    }

    // expression
    pub fn expect_type_expr(&mut self) -> TypeExpressionNode {
        components::expression::type_expression::type_expr(self)
    }
}