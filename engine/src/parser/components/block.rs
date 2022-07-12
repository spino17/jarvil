use crate::ast::ast::{BlockNode, ParamsNode, StatemenIndentWrapper, SkippedTokens, TokenNode};
use crate::constants::common::ENDMARKER;
use crate::parser::helper::{IndentResultKind};
use crate::parser::parser::{PackratParser};
use std::rc::Rc;
use std::mem;
use std::cell::RefCell;

use super::stmt::is_statement_starting_with;
 
pub fn block(parser: &mut PackratParser, params: Option<&ParamsNode>) -> BlockNode {
    let newline_node = parser.expect("\n", false);
    parser.reset_indent_level(parser.get_curr_indent_level() + 1);
    let stmts_vec: Rc<RefCell<Vec<StatemenIndentWrapper>>> = Rc::new(RefCell::new(vec![]));
    let mut is_indent_check_enabled = true;
    let mut leading_skipped_tokens: Vec<TokenNode> = vec![];
    loop {
        let mut incorrect_indent_data: Option<(i64, i64)> = None;
        if is_indent_check_enabled {
            let indent_result = parser.expect_indent_spaces();
            let skipped_tokens = indent_result.skipped_tokens;
            if skipped_tokens.len() > 0 {
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::TRAILING_SKIPPED_TOKENS(
                    SkippedTokens::new_with_trailing_skipped_tokens(&Rc::new(skipped_tokens))
                ));
            }
            let extra_newlines = indent_result.extra_newlines;
            if extra_newlines.len() > 0 {
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::EXTRA_NEWLINES(
                    SkippedTokens::new_with_extra_newlines(&Rc::new(extra_newlines))
                ));
            }
            let local_incorrect_indent_data
            = match indent_result.kind {
                IndentResultKind::CORRECT_INDENTATION => None,
                IndentResultKind::INCORRECT_INDENTATION(indent_data) => Some(indent_data),
                IndentResultKind::BLOCK_OVER => {
                    // use skipped_token_vec
                    return BlockNode::new(&stmts_vec, params)
                }
            };
            incorrect_indent_data = local_incorrect_indent_data;
        }
        let token = &parser.get_curr_token();
        if token.is_eq(ENDMARKER) {
            if leading_skipped_tokens.len() > 0 {
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::LEADING_SKIPPED_TOKENS(
                    SkippedTokens::new_with_leading_skipped_tokens(&Rc::new(mem::take(&mut leading_skipped_tokens)))
                ));
            }
            return BlockNode::new(&stmts_vec, params)
        }
        if token.is_eq("\n") {
            is_indent_check_enabled = true;
            leading_skipped_tokens.push(TokenNode::new_with_skipped_token(token, parser.get_curr_lookahead()));
            if leading_skipped_tokens.len() > 0 {
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::LEADING_SKIPPED_TOKENS(
                    SkippedTokens::new_with_leading_skipped_tokens(&Rc::new(mem::take(&mut leading_skipped_tokens)))
                ));
            }
            parser.scan_next_token();
            continue;
        }
        if !is_statement_starting_with(token) {
            is_indent_check_enabled = false;
            leading_skipped_tokens.push(TokenNode::new_with_skipped_token(token, parser.get_curr_lookahead()));
            parser.scan_next_token();
            continue;
        }
        else {
            is_indent_check_enabled = true;
            if leading_skipped_tokens.len() > 0 {
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::LEADING_SKIPPED_TOKENS(
                    SkippedTokens::new_with_leading_skipped_tokens(&Rc::new(mem::take(&mut leading_skipped_tokens)))
                ));
            }
        }
        match incorrect_indent_data {
            Some(indent_data) => {
                let stmt_node = if parser.is_ignore_all_errors() {

                    // a sub stmt of already incorrectly indented stmt some levels higher
                    let saved_correction_indent = parser.get_correction_indent();
                    parser.add_to_correction_indent(indent_data.1 - indent_data.0);
                    let stmt_node = parser.stmt();
                    parser.set_correction_indent(saved_correction_indent);
                    stmt_node
                } else {

                    // the highest level incorrectly indented stmt
                    parser.set_ignore_all_errors(true);
                    let before_line_number = parser.get_curr_line_number();
                    parser.set_correction_indent(indent_data.1 - indent_data.0);
                    let stmt_node = parser.stmt();
                    parser.set_ignore_all_errors(false);
                    let after_line_number = parser.get_curr_line_number();
                    parser.set_correction_indent(0);
                    // TODO - log the related error into a error log struct to output on terminal based compilation
                    // (use before and after line_number to show the non-local nature of the error)
                    stmt_node
                };
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::INCORRECTLY_INDENTED((stmt_node, indent_data)));
            },
            None => {
                let stmt_node = parser.stmt();
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::CORRECTLY_INDENTED(stmt_node));
            }
        }
    }
}