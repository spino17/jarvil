use crate::ast::ast::{StatementNode, ParamNode, BlockNode, ASTNode};
use crate::parser::parser::{PackratParser};
use std::rc::Rc;
use std::cell::RefCell;

pub fn block(parser: &mut PackratParser, params: &Rc<Vec<ParamNode>>, 
    parent: Option<ASTNode>) -> BlockNode {
    let newline_node = parser.expect("\n", false);
    parser.reset_indent_level(parser.get_curr_indent_level() + 1);
    let stmts_vec: Rc<RefCell<Vec<StatementNode>>> = Rc::new(RefCell::new(vec![]));
    loop {
        match parser.expect_indent_spaces(&stmts_vec, params, &parent) {
            Some(block_node) => return block_node,
            None => {}
        };
        let stmt_node = parser.stmt();
        stmts_vec.as_ref().borrow_mut().push(stmt_node);
    }
}