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
        let incorrect_indent_data
        = match parser.expect_indent_spaces(&stmts_vec, params, &parent) {
            IndentResult::CORRECT_INDENTATION => None,
            IndentResult::INCORRECT_INDENTATION(indent_data) => Some(indent_data),
            IndentResult::BLOCK_OVER(block_node) => return block_node
        };
        let stmt_node = parser.stmt();
        match incorrect_indent_data {
            Some(indent_data) => {
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::INCORRECTLY_INDENTED((stmt_node, indent_data)));
            },
            None => stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::CORRECTLY_INDENTED(stmt_node)),
        }
    }
}