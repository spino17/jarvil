// Default parser for jarvil uses Packrat approach, first given by Bryan Ford in his master thesis at MIT. It is essentially a
// top down recursive descent parsing with lazy memoization in order to avoid exponential parse time and provide reliable
// linear time parsing!
// See `https://pdos.csail.mit.edu/~baford/packrat/thesis/` for more information.

use super::helper::format_symbol;
use crate::ast::ast::{
    AssignmentNode, AtomNode, AtomicExpressionNode, BlockNode, ExpressionNode, FuncKeywordKind,
    FunctionDeclarationNode, NameTypeSpecNode, NameTypeSpecsNode, ParamsNode, RAssignmentNode,
    SkippedTokenNode, StatementNode, TokenNode, TypeDeclarationNode, TypeExpressionNode,
    UnaryExpressionNode, VariableDeclarationNode,
};
use crate::ast::ast::{ErrornousNode, OkTokenKind};
use crate::code::Code;
use crate::constants::common::{ENDMARKER, IDENTIFIER};
use crate::context;
use crate::errors::{JarvilError, ParseErrorKind};
use crate::lexer::token::{CoreToken, Token};
use crate::parser::components;
use crate::parser::helper::{IndentResult, IndentResultKind};
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub trait Parser {
    fn parse(self, token_vec: Vec<Token>) -> BlockNode;
}

#[derive(Debug)]
pub enum RoutineCache {
    // currently only two routine (atom, expr) results are cached by the parser
    ATOM(
        Rc<
            RefCell<
                FxHashMap<usize, Result<(ParseSuccess, Option<Type>, bool, bool), JarvilError>>,
            >,
        >,
    ),
    EXPR(Rc<RefCell<FxHashMap<usize, Result<(ParseSuccess, bool), JarvilError>>>>),
}

#[derive(Debug)]
pub struct ParseSuccess {
    pub lookahead: usize,
    pub possible_err: Option<JarvilError>,
}

pub struct PackratParser {
    token_vec: Vec<Token>,
    lookahead: usize,
    indent_level: i64,
    code: Code,
    cache: Vec<Rc<RoutineCache>>,
    ignore_all_errors: bool, // if this is set, no errors during parsing is saved inside error logs
    correction_indent: i64,
    // errors: Vec<ParseError>  // reported errors during the parsing, useful for terminal based compilation
}

impl PackratParser {
    pub fn new(code: &Code) -> Self {
        let atom_cache_map: FxHashMap<
            usize,
            Result<(ParseSuccess, Option<Type>, bool, bool), JarvilError>,
        > = FxHashMap::default();
        let expr_cache_map: FxHashMap<usize, Result<(ParseSuccess, bool), JarvilError>> =
            FxHashMap::default();
        PackratParser {
            token_vec: Vec::new(),
            lookahead: 0,
            indent_level: -1,
            code: code.clone(),
            cache: vec![
                Rc::new(RoutineCache::ATOM(Rc::new(RefCell::new(atom_cache_map)))),
                Rc::new(RoutineCache::EXPR(Rc::new(RefCell::new(expr_cache_map)))),
            ],
            ignore_all_errors: false,
            correction_indent: 0,
            // errors: vec![],
        }
    }
}

impl Parser for PackratParser {
    fn parse(mut self, token_vec: Vec<Token>) -> BlockNode {
        let code_node = self.code(token_vec);
        code_node
    }
}

impl PackratParser {
    // ------------------- parsing utilities -------------------
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
        if self.lookahead == self.token_vec.len() - 1 {
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

    pub fn curr_line_number(&self) -> usize {
        self.token_vec[self.lookahead].line_number
    }

    pub fn curr_token(&mut self) -> Token {
        self.token_vec[self.lookahead].clone()
    }

    pub fn check_curr_token(&self, symbol: &str) -> bool {
        self.token_vec[self.lookahead].is_eq(symbol)
    }

    pub fn curr_token_precedence(&self) -> u8 {
        self.token_vec[self.lookahead].get_precedence()
    }

    pub fn previous_token(&mut self) -> Token {
        if self.lookahead == 0 {
            return Token {
                line_number: 1,
                core_token: CoreToken::NEWLINE,
                start_index: 0,
                end_index: 0,
                trivia: None,
                parent: None,
            };
        }
        self.token_vec[self.lookahead - 1].clone()
    }

    pub fn ignore_newlines(&mut self) -> Vec<SkippedTokenNode> {
        let mut ignored_newlines_vec = vec![];
        loop {
            let token = &self.token_vec[self.lookahead];
            if token.is_eq("\n") {
                ignored_newlines_vec.push(SkippedTokenNode::new(&token, self.curr_lookahead()));
                self.scan_next_token();
            } else {
                return ignored_newlines_vec;
            }
        }
    }

    pub fn is_curr_token_on_newline(&self) -> bool {
        if self.lookahead == 0 {
            return true;
        }
        if self.token_vec[self.lookahead - 1].is_eq("\n")
            || self.token_vec[self.lookahead].is_eq("\n")
            || self.token_vec[self.lookahead].is_eq(ENDMARKER)
        {
            true
        } else {
            false
        }
    }

    pub fn skip_to_newline(&mut self) -> Vec<SkippedTokenNode> {
        let mut skipped_tokens: Vec<SkippedTokenNode> = vec![];
        loop {
            let token = &self.token_vec[self.lookahead].clone();
            if token.is_eq("\n") || token.is_eq(ENDMARKER) {
                self.log_trailing_skipped_tokens_error(&skipped_tokens);
                skipped_tokens.push(SkippedTokenNode::new(&token, self.curr_lookahead()));
                self.scan_next_token();
                return skipped_tokens;
            } else {
                skipped_tokens.push(SkippedTokenNode::new(&token, self.curr_lookahead()));
                self.scan_next_token();
            }
        }
    }

    // ------------------- error logging utilities for terminal-based compilation -------------------
    pub fn log_missing_token_error_for_single_expected_symbol(
        &mut self,
        expected_symbol: &str,
        recevied_token: &Token,
    ) {
        // This type of error handling is inspired from Golang programming language
        // See /src/go/parser/parser.go -> `func (p *parser) error(pos token.Pos, msg string) {...}`
        if self.ignore_all_errors {
            return;
        }
        let (code_line, line_start_index, line_number, err_index) = self
            .code
            .line_data(recevied_token.line_number, recevied_token.index());
        let errors_len = context::errors_len();
        if errors_len > 0 && context::curr_error_line_number() == line_number {
            return;
        } else {
            let err_str = format!(
                "expected `{}`, got `{}`",
                format_symbol(expected_symbol),
                recevied_token.name()
            );
            let err_message = JarvilError::form_single_line_single_pointer_error(
                err_index,
                line_number,
                line_start_index,
                code_line,
                err_str,
                ParseErrorKind::SYNTAX_ERROR,
            );
            let err = JarvilError::new(line_number, line_number, err_message);
            context::push_error(err);
        }
    }

    pub fn log_missing_token_error_for_multiple_expected_symbols(
        &mut self,
        expected_symbols: &[&'static str],
        recevied_token: &Token,
    ) {
        if self.ignore_all_errors {
            return;
        }
        if expected_symbols.len() == 1 {
            return self.log_missing_token_error_for_single_expected_symbol(
                expected_symbols[0],
                recevied_token,
            );
        }
        let errors_len = context::errors_len();
        let (code_line, line_start_index, line_number, err_index) = self
            .code
            .line_data(recevied_token.line_number, recevied_token.index());
        if errors_len > 0 && context::curr_error_line_number() == line_number {
            return;
        } else {
            let mut err_str = String::from("expected ");
            let mut flag = false;
            let symbols_len = expected_symbols.len();
            for index in 0..symbols_len - 1 {
                if flag {
                    err_str.push_str(", ");
                }
                err_str.push_str(&format!("`{}`", format_symbol(expected_symbols[index])));
                flag = true;
            }
            err_str.push_str(&format!(
                " or `{}`, got `{}`",
                format_symbol(expected_symbols[symbols_len - 1]),
                recevied_token.name()
            ));
            let err_message = JarvilError::form_single_line_single_pointer_error(
                err_index,
                line_number,
                line_start_index,
                code_line,
                err_str,
                ParseErrorKind::SYNTAX_ERROR,
            );
            let err = JarvilError::new(line_number, line_number, err_message);
            context::push_error(err);
        }
    }

    pub fn log_trailing_skipped_tokens_error(&mut self, skipped_tokens: &Vec<SkippedTokenNode>) {
        if self.ignore_all_errors {
            return;
        }
        let errors_len = context::errors_len();
        let skipped_tokens_len = skipped_tokens.len();
        let (code_line, line_start_index, line_number, start_err_index) = self
            .code
            .line_data(skipped_tokens[0].line_number(), skipped_tokens[0].index());
        if errors_len > 0 && context::curr_error_line_number() == line_number {
            return;
        } else {
            let err_str = String::from("invalid sequence of tokens found at the trail of the line");
            let end_err_index = skipped_tokens[skipped_tokens_len - 1].index();
            let err_message = JarvilError::form_single_line_underline_pointer_error(
                start_err_index,
                end_err_index,
                line_number,
                line_start_index,
                code_line,
                err_str,
                ParseErrorKind::SYNTAX_ERROR,
            );
            let err = JarvilError::new(line_number, line_number, err_message);
            context::push_error(err);
        }
    }

    pub fn log_incorrectly_indented_block_error(
        &mut self,
        start_line_number: usize,
        end_line_number: usize,
        expected_indent: i64,
        received_indent: i64,
    ) {
        if self.ignore_all_errors {
            return;
        }
        let errors_len = context::errors_len();
        if errors_len > 0 && context::curr_error_line_number() == start_line_number {
            return;
        } else {
            let code_lines: Vec<String> = self.code.lines(start_line_number, end_line_number);
            let err_str = format!(
                "expected an indented block with `{}` spaces, got `{}` spaces",
                expected_indent, received_indent
            );
            let err_message = JarvilError::form_multi_line_error(
                start_line_number,
                end_line_number,
                code_lines,
                err_str,
                ParseErrorKind::SYNTAX_ERROR,
            );
            let err = JarvilError::new(start_line_number, end_line_number, err_message);
            context::push_error(err);
        }
    }

    // ------------------- parsing routines for terminals and block indentation -------------------
    pub fn expect(&mut self, symbol: &'static str) -> TokenNode {
        let token = self.curr_token();
        if token.is_eq(symbol) {
            self.scan_next_token();
            let kind = if symbol == IDENTIFIER {
                OkTokenKind::IDENTIFIER(None)
            } else {
                OkTokenKind::NON_IDENTIFIER
            };
            TokenNode::new_with_ok_token(&token, self.curr_lookahead(), kind)
        } else {
            self.log_missing_token_error_for_single_expected_symbol(symbol, &token);
            TokenNode::new_with_missing_tokens(
                &Rc::new(vec![symbol]),
                &token,
                self.curr_lookahead(),
            )
        }
    }

    pub fn expects(&mut self, symbols: &[&'static str]) -> TokenNode {
        let token = self.curr_token();
        for &symbol in symbols {
            if token.is_eq(symbol) {
                self.scan_next_token();
                let kind = if symbol == IDENTIFIER {
                    OkTokenKind::IDENTIFIER(None)
                } else {
                    OkTokenKind::NON_IDENTIFIER
                };
                return TokenNode::new_with_ok_token(&token, self.curr_lookahead(), kind);
            }
        }
        // self.log_skipped_token_error(symbols, &token);
        TokenNode::new_with_missing_tokens(
            &Rc::new(symbols.to_vec()),
            &token,
            self.curr_lookahead(),
        )
    }

    pub fn expect_binary_operator(&mut self) -> TokenNode {
        self.expects(&["or", "and", ">", ">=", "<", "<=", "==", "!=", "+", "-", "*", "/"])
    }

    pub fn expect_terminals(&mut self) -> TokenNode {
        self.expects(&["\n", ENDMARKER])
    }

    pub fn expect_indent_spaces(&mut self) -> IndentResult {
        let mut skipped_tokens: Vec<SkippedTokenNode> = vec![];
        let mut extra_newlines: Vec<SkippedTokenNode> = vec![];
        if !self.is_curr_token_on_newline() {
            skipped_tokens = self.skip_to_newline();
        }
        let mut expected_indent_spaces = context::indent_spaces() * self.indent_level;
        let mut indent_spaces = 0;
        loop {
            let token = &self.token_vec[self.lookahead];
            match &token.core_token {
                CoreToken::NEWLINE => {
                    extra_newlines.push(SkippedTokenNode::new(token, self.curr_lookahead()));
                    indent_spaces = 0;
                }
                CoreToken::ENDMARKER => {
                    return IndentResult {
                        kind: IndentResultKind::BLOCK_OVER,
                        skipped_tokens,
                        extra_newlines,
                    }
                }
                _ => {
                    indent_spaces = (token.start_index
                        - self.code.get_line_start_index(token.line_number))
                        as i64;
                    expected_indent_spaces = expected_indent_spaces + self.correction_indent();
                    if indent_spaces == expected_indent_spaces {
                        return IndentResult {
                            kind: IndentResultKind::CORRECT_INDENTATION,
                            skipped_tokens,
                            extra_newlines,
                        };
                    } else if indent_spaces > expected_indent_spaces {
                        return IndentResult {
                            kind: IndentResultKind::INCORRECT_INDENTATION((
                                expected_indent_spaces,
                                indent_spaces,
                            )),
                            skipped_tokens,
                            extra_newlines,
                        };
                    } else {
                        return IndentResult {
                            kind: IndentResultKind::BLOCK_OVER,
                            skipped_tokens,
                            extra_newlines,
                        };
                    }
                }
            }
            self.scan_next_token();
        }
    }

    // ------------------- packrat parser caching utilities -------------------
    pub fn get_or_set_cache<
        T: std::fmt::Debug,
        F: FnOnce(&mut PackratParser) -> Result<T, JarvilError>,
        G: FnOnce(&Result<T, JarvilError>) -> Result<T, JarvilError>,
        H: FnOnce(&T) -> usize,
    >(
        &mut self,
        cache_map: &Rc<RefCell<FxHashMap<usize, Result<T, JarvilError>>>>,
        routine_fn: F,
        clone_result_fn: G,
        get_lookahead_fn: H,
        curr_lookahead: usize,
        message: &str,
    ) -> Result<T, JarvilError> {
        match cache_map.borrow().get(&curr_lookahead) {
            Some(result) => {
                let result = clone_result_fn(result);
                match result {
                    Ok(response) => {
                        self.set_lookahead(get_lookahead_fn(&response));
                        return Ok(response);
                    }
                    Err(err) => return Err(err),
                }
            }
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

    pub fn block<F: Fn(&Token) -> bool, G: Fn(&mut PackratParser) -> StatementNode>(
        &mut self,
        is_starting_with_fn: F,
        statement_parsing_fn: G,
        expected_symbols: &[&'static str],
    ) -> BlockNode {
        components::block::block(
            self,
            is_starting_with_fn,
            statement_parsing_fn,
            expected_symbols,
        )
    }

    // statements
    pub fn stmt(&mut self) -> StatementNode {
        components::statement::core::stmt(self)
    }

    pub fn assignment(&mut self, expr: &ExpressionNode) -> AssignmentNode {
        components::assignment::assignment(self, expr)
    }

    // type expression
    pub fn type_expr(&mut self) -> TypeExpressionNode {
        components::expression::type_expression::type_expr(self)
    }

    // expression
    pub fn atomic_expr(&mut self) -> AtomicExpressionNode {
        components::expression::core::atomic_expr(self)
    }

    pub fn unary_expr(&mut self) -> UnaryExpressionNode {
        components::expression::core::unary_expr(self)
    }

    pub fn factor(&mut self) -> ExpressionNode {
        components::expression::core::factor(self)
    }

    pub fn term(&mut self) -> ExpressionNode {
        components::expression::core::term(self)
    }

    pub fn comparison(&mut self) -> ExpressionNode {
        components::expression::core::comparison(self)
    }

    pub fn logical_and(&mut self) -> ExpressionNode {
        components::expression::core::logical_and(self)
    }

    pub fn logical_or(&mut self) -> ExpressionNode {
        components::expression::core::logical_or(self)
    }

    pub fn expr(&mut self) -> ExpressionNode {
        components::expression::core::expr(self)
    }

    pub fn expr2(&mut self) -> ExpressionNode {
        components::expression::pratt::expr(self)
    }

    pub fn pratt_expr(&mut self, precedence: u8) -> ExpressionNode {
        components::expression::pratt::pratt_expr(self, precedence)
    }

    pub fn trailing_atom(&mut self, atom_start: AtomNode) -> AtomNode {
        components::expression::atom::trailing_atom(self, atom_start)
    }

    pub fn atom(&mut self) -> AtomNode {
        components::expression::atom::atom(self)
    }

    // function
    pub fn params(&mut self) -> ParamsNode {
        components::expression::common::params(self)
    }

    pub fn params_within_parenthesis(&mut self) -> (Option<ParamsNode>, TokenNode, TokenNode) {
        components::expression::common::params_within_parenthesis(self)
    }

    // declaration
    pub fn variable_decl(&mut self) -> VariableDeclarationNode {
        components::variable_declaration::variable_decl(self)
    }

    pub fn name_type_spec(&mut self) -> NameTypeSpecNode {
        components::function_declaration::name_type_spec(self)
    }

    pub fn name_type_specs(&mut self) -> NameTypeSpecsNode {
        components::function_declaration::name_type_specs(self)
    }

    pub fn name_type_specs_within_parenthesis(
        &mut self,
    ) -> (Option<NameTypeSpecsNode>, TokenNode, TokenNode) {
        components::function_declaration::name_type_specs_within_parenthesis(self)
    }

    pub fn r_assign(&mut self, identifier_name: Option<&TokenNode>) -> RAssignmentNode {
        components::common::r_assign(self, identifier_name)
    }

    pub fn function_name(&mut self) -> (TokenNode, TokenNode) {
        components::function_declaration::function_name(self)
    }

    pub fn function_decl(
        &mut self,
        name: Option<&TokenNode>,
        func_keyword: &FuncKeywordKind,
    ) -> FunctionDeclarationNode {
        components::function_declaration::function_decl(self, name, func_keyword)
    }

    pub fn struct_stmt(&mut self) -> StatementNode {
        components::statement::core::struct_stmt(self)
    }

    pub fn type_decl(&mut self) -> TypeDeclarationNode {
        components::type_declaration::type_decl(self)
    }
}
