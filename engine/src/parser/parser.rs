// Default parser for jarvil uses Packrat approach, first given by Bryan Ford in his master thesis at MIT. It is essentially a 
// top down recursive descent parsing with lazy memoization in order to avoid exponential parse time and provide reliable 
// linear time parsing!
// See `https://pdos.csail.mit.edu/~baford/packrat/thesis/` for more information.

use crate::ast::ast::{TypeExpressionNode, StatementNode, BlockNode, TokenNode, ParamsNode};
use crate::constants::common::ENDMARKER;
use crate::lexer::token::{Token, CoreToken};
use std::rc::Rc;
use crate::errors::{ParseError, ErrorKind};
use crate::context;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use crate::types::core::{Type};
use crate::parser::components;
use crate::parser::helper::{IndentResult, IndentResultKind};

pub trait Parser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError>;
}

#[derive(Debug)]
pub enum RoutineCache {
    // currently only two routine (atom, expr) results are cached by the parser
    ATOM(Rc<RefCell<FxHashMap<usize, Result<(ParseSuccess, Option<Type>, bool, bool), ParseError>>>>),
    EXPR(Rc<RefCell<FxHashMap<usize, Result<(ParseSuccess, bool), ParseError>>>>),
}

#[derive(Debug)]
pub struct ParseSuccess {
    pub lookahead: usize,
    pub possible_err: Option<ParseError>,
}

pub struct PackratParser {
    token_vec: Vec<Token>,
    lookahead: usize,
    indent_level: i64,
    code_lines: Vec<(Rc<String>, usize)>,
    cache: Vec<Rc<RoutineCache>>,
    ignore_all_errors: bool,  // if this is set, no errors during parsing is saved inside error logs
    correction_indent: i64,
    errors: Vec<ParseError>  // reported errors during the parsing
}

impl PackratParser {
    pub fn new(code_lines: Vec<(Rc<String>, usize)>) -> Self {
        let atom_cache_map: FxHashMap<usize, Result<(ParseSuccess, Option<Type>, bool, bool), ParseError>> 
        = FxHashMap::default();
        let expr_cache_map: FxHashMap<usize, Result<(ParseSuccess, bool), ParseError>> = FxHashMap::default();
        PackratParser {
            token_vec: Vec::new(),
            lookahead: 0,
            indent_level: -1,
            code_lines,
            cache: vec![
                Rc::new(RoutineCache::ATOM(Rc::new(RefCell::new(atom_cache_map)))),
                Rc::new(RoutineCache::EXPR(Rc::new(RefCell::new(expr_cache_map)))),
            ],
            ignore_all_errors: false,
            correction_indent: 0,
            errors: vec![],
        }
    }
}

impl Parser for PackratParser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        //self.code(token_vec)?;
        Ok(())
    }
}

impl PackratParser {
    // parsing utilities
    pub fn set_token_vec(&mut self, token_vec: Vec<Token>) {
        self.token_vec = token_vec;
    }

    pub fn curr_lookahead(&self) -> usize {
        self.lookahead
    }

    pub fn set_lookahead(&mut self, reset_index: usize) {
        self.lookahead = reset_index;
    }

    pub fn scan_next_token(&mut self) {
        if self.lookahead == self.token_vec.len() - 1 {  // if token is endmarker, don't change
            return;
        }
        self.lookahead = self.lookahead + 1;
    }

    pub fn curr_indent_level(&self) -> i64 {
        self.indent_level
    }

    pub fn set_indent_level(&mut self, reset_indent: i64) {
        self.indent_level = reset_indent;
    }

    pub fn is_ignore_all_errors(&self) -> bool {
        self.ignore_all_errors
    }

    pub fn set_ignore_all_errors(&mut self, is_ignore_all_errors: bool) {
        self.ignore_all_errors = is_ignore_all_errors;
    }

    pub fn correction_indent(&self) -> i64 {
        self.correction_indent
    }

    pub fn set_correction_indent(&mut self, correction_indent: i64) {
        self.correction_indent = correction_indent;
    }

    pub fn add_to_correction_indent(&mut self, addition: i64) {
        self.correction_indent = self.correction_indent + addition;
    }

    pub fn code_line_data(&self, mut curr_line_number: usize, index: usize) -> (Rc<String>, usize, usize, usize) {
        loop {
            let (s, line_start_index) = &self.code_lines[curr_line_number - 1];
            if index >= *line_start_index {
                return (s.clone(), *line_start_index, curr_line_number, index)
            }
            curr_line_number = curr_line_number - 1;
        }
    }

    pub fn curr_line_number(&self) -> usize {
        self.token_vec[self.lookahead].line_number
    }

    pub fn curr_token(&mut self) -> Token {
        self.token_vec[self.lookahead].clone()
    }

    pub fn check_curr_token(&self, symbol: &str) -> bool {
        self.token_vec[self.lookahead].is_eq(symbol)
    }

    pub fn previous_token(&mut self) -> Token {
        if self.lookahead == 0 {
            return Token {
                line_number: 1,
                core_token: CoreToken::NEWLINE,
                name: Rc::new(String::from("\n")),
                start_index: 0,
                end_index: 0,
                trivia: None,
                parent: None,
            }
        }
        self.token_vec[self.lookahead - 1].clone()
    }

    pub fn log_missing_token_error(&mut self, expected_symbol: &str, recevied_token: &Token) {
        if self.ignore_all_errors {
            return;
        }
        let (err_code_line, err_line_start_index, err_line_number, err_index) 
        = self.code_line_data(recevied_token.line_number, recevied_token.index());
        let errors_len = self.errors.len();
        if errors_len > 0 && self.errors[errors_len - 1].end_line_number == err_line_number {
            return;
        } else {
            let err_str = format!("expected `{}`, got `{}`", expected_symbol, recevied_token.name());
            let err_message = ParseError::form_single_line_error(err_index, err_line_number, err_line_start_index, 
                err_code_line, err_str, ErrorKind::SYNTAX_ERROR);
            let err 
            = ParseError::new(err_line_number, err_line_number, err_message);
            self.errors.push(err);
        }
    }

    pub fn log_incorrectly_indented_block_error(&mut self, start_line_number: usize, end_line_number: usize) {
        if self.ignore_all_errors {
            return;
        }

    }

    pub fn ignore_newlines(&mut self) {
        loop {
            let token = &self.token_vec[self.lookahead];
            if token.is_eq("\n") {
                self.scan_next_token();
            } else {
                return;
            }
        }
    }

    pub fn is_eof_reached(&self) -> bool {
        if self.token_vec[self.lookahead].is_eq(ENDMARKER) {
            true
        } else {
            false
        }
    }

    pub fn is_curr_token_on_newline(&self) -> bool {
        if self.lookahead == 0 {
            return true
        }
        if self.token_vec[self.lookahead - 1].is_eq("\n")
        || self.token_vec[self.lookahead].is_eq("\n")
        || self.token_vec[self.lookahead].is_eq(ENDMARKER) {
            true
        } else {
            false
        }
    }

    pub fn skip_to_newline(&mut self) -> Vec<TokenNode> {
        let mut skipped_tokens: Vec<TokenNode> = vec![];
        loop {
            let token = &self.token_vec[self.lookahead];
            if token.is_eq("\n") || token.is_eq(ENDMARKER) {
                skipped_tokens.push(TokenNode::new_with_skipped_token(&token, self.curr_lookahead()));
                self.scan_next_token();
                return skipped_tokens
            } else {
                skipped_tokens.push(TokenNode::new_with_skipped_token(&token, self.curr_lookahead()));
                self.scan_next_token();
            }
        }
    }

    // parsing routines for terminals
    pub fn expect(&mut self, symbol: &str, ignore_newline: bool) -> TokenNode {
        if ignore_newline {
            self.ignore_newlines();
        }
        let token = self.curr_token();
        if token.is_eq(symbol) {
            self.scan_next_token();
            TokenNode::new_with_token(&token, self.curr_lookahead())
        } else {
            // TODO - log the related error into a error log struct to output on terminal based compilation
            self.log_missing_token_error(symbol, &token);
            TokenNode::new_with_missing_token(
                &Rc::new(String::from(symbol)),
                &token,
                self.curr_lookahead()
            )
        }
    }

    pub fn expect_indent_spaces(&mut self) -> IndentResult {
        let mut skipped_tokens: Vec<TokenNode> = vec![];
        let mut extra_newlines: Vec<TokenNode> = vec![];
        if !self.is_curr_token_on_newline() {
            // start skipping tokens until you reach newline and declare that chain of tokens as skipped
            skipped_tokens = self.skip_to_newline();
        }
        let mut expected_indent_spaces = context::get_indent() * self.indent_level;
        let mut indent_spaces = 0;
        loop {
            let token = &self.token_vec[self.lookahead];
            match &token.core_token {
                CoreToken::NEWLINE => {
                    extra_newlines.push(TokenNode::new_with_token(token, self.curr_lookahead()));
                    indent_spaces = 0;
                }
                CoreToken::ENDMARKER => {
                    self.set_indent_level(self.curr_indent_level() - 1);
                    return IndentResult{
                        kind: IndentResultKind::BLOCK_OVER,
                        skipped_tokens,
                        extra_newlines,
                    }
                },
                _ => {
                    match &token.trivia {
                        Some(trivia_vec) => {
                            match trivia_vec[0].core_token {
                                CoreToken::BLANK => {
                                    indent_spaces = (token.end_index - token.start_index) as i64;
                                },
                                _ => indent_spaces = 0,
                            }
                        },
                        None => indent_spaces = 0,
                    }
                    expected_indent_spaces = expected_indent_spaces + self.correction_indent();
                    if indent_spaces == expected_indent_spaces {
                        // correctly indented statement
                        return IndentResult{
                            kind: IndentResultKind::CORRECT_INDENTATION,
                            skipped_tokens,
                            extra_newlines,
                        }
                    } else if indent_spaces > expected_indent_spaces {
                        // incorrectly indented statement
                        return IndentResult{
                            kind: IndentResultKind::INCORRECT_INDENTATION((expected_indent_spaces, indent_spaces)),
                            skipped_tokens,
                            extra_newlines,
                        }
                    } else {
                        // over the block
                        self.set_indent_level(self.curr_indent_level() - 1);
                        return IndentResult{
                            kind: IndentResultKind::BLOCK_OVER,
                            skipped_tokens,
                            extra_newlines,
                        }
                    } 
                }
            }
            self.scan_next_token();
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
    F: FnOnce(&mut PackratParser) -> Result<T, ParseError>, 
    G: FnOnce(&Result<T, ParseError>) -> Result<T, ParseError>,
    H: FnOnce(&T) -> usize>(
        &mut self,
        cache_map: &Rc<RefCell<FxHashMap<usize, Result<T, ParseError>>>>,
        routine_fn: F,
        clone_result_fn: G,
        get_lookahead_fn: H,
        curr_lookahead: usize,
        message: &str,
    ) -> Result<T, ParseError> {
        match cache_map.borrow().get(&curr_lookahead) {
            Some(result) => {
                let result = clone_result_fn(result);
                match result {
                    Ok(response) => {
                        self.set_lookahead(get_lookahead_fn(&response));
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

    // ------------------- production rule matching function for terminals and non-terminals -------------------
    // code
    pub fn code(&mut self, token_vec: Vec<Token>) -> BlockNode {
        components::code::code(self, token_vec)
    }

    pub fn block<F: Fn(&Token) -> bool>(&mut self, 
        params: Option<&ParamsNode>, is_starting_with_fn: F) -> BlockNode {
        components::block::block(self, params, is_starting_with_fn)
    }

    // statements
    pub fn stmt(&mut self) -> StatementNode {
        components::stmt::stmt(self)
    }

    // expression
    pub fn type_expr(&mut self) -> TypeExpressionNode {
        components::expression::type_expression::type_expr(self)
    }
}