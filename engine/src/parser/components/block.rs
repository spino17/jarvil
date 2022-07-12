use crate::ast::ast::{BlockNode, ParamsNode, StatemenIndentWrapper, TrailingSkippedTokens};
use crate::parser::helper::{IndentResultKind};
use crate::parser::parser::{PackratParser};
use std::rc::Rc;
use std::cell::RefCell;

pub fn block(parser: &mut PackratParser, params: Option<&ParamsNode>) -> BlockNode {
    let newline_node = parser.expect("\n", false);
    parser.reset_indent_level(parser.get_curr_indent_level() + 1);
    let stmts_vec: Rc<RefCell<Vec<StatemenIndentWrapper>>> = Rc::new(RefCell::new(vec![]));
    loop {
        let indent_result = parser.expect_indent_spaces();
        let skipped_tokens = indent_result.skipped_tokens;
        stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::TRAILING_SKIPPED_TOKENS(
            TrailingSkippedTokens::new(&Rc::new(skipped_tokens))
        ));
        let incorrect_indent_data  // (expected_indent_spaces, received_indent_spaces)
        = match indent_result.kind {
            IndentResultKind::CORRECT_INDENTATION => None,
            IndentResultKind::INCORRECT_INDENTATION(indent_data) => Some(indent_data),
            IndentResultKind::BLOCK_OVER => {
                // use skipped_token_vec
                return BlockNode::new(&stmts_vec, params)
            }
        };
        // check if current token lies in FIRST(stmt) - if not then declare it as skipping token and advance
        // lookahead by one and continue to next loop iteration
        /*
        if !parser.is_stmt_starting_with(&parser.get_curr_token()) {
            // skip the current token and set the skippped token trivia to the next token and continue to next loop iteration
            todo!()
        }
         */
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