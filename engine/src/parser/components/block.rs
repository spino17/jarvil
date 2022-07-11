use crate::ast::ast::{StatementNode, ParamNode, BlockNode, ASTNode, StatemenIndentWrapper};
use crate::parser::helper::IndentResult;
use crate::parser::parser::{PackratParser};
use std::rc::Rc;
use std::cell::RefCell;

pub fn block(parser: &mut PackratParser, params: &Rc<Vec<ParamNode>>, 
    parent: Option<ASTNode>) -> BlockNode {
    let newline_node = parser.expect("\n", false);
    parser.reset_indent_level(parser.get_curr_indent_level() + 1);
    let stmts_vec: Rc<RefCell<Vec<StatemenIndentWrapper>>> = Rc::new(RefCell::new(vec![]));
    loop {
        let incorrect_indent_data  // (expected_indent_spaces, received_indent_spaces)
        = match parser.expect_indent_spaces(&stmts_vec, params, &parent) {
            IndentResult::CORRECT_INDENTATION => None,
            IndentResult::INCORRECT_INDENTATION(indent_data) => Some(indent_data),
            IndentResult::BLOCK_OVER(block_node) => return block_node
        };
        match incorrect_indent_data {
            Some(indent_data) => {
                let stmt_node = if parser.is_ignore_all_errors() {
                    parser.stmt()
                } else {
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