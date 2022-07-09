use crate::ast::ast::{StatementNode, ParamNode, BlockNode, ASTNode};
use crate::ast::helper::IndentNode;
use crate::context;
use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::errors::SyntaxError;
use std::rc::Rc;
use std::cell::RefCell;

pub fn check_block_indentation(parser: &mut PackratParser,
    indent_spaces: i64, err: SyntaxError, curr_lookahead: usize, 
    params: &Rc<Vec<ParamNode>>, stmts: &Rc<RefCell<Vec<StatementNode>>>, 
    parent: Option<ASTNode>) -> BlockNode {
    let indent_spaces_unit = context::get_indent();
    let indent_factor = indent_spaces / indent_spaces_unit as i64;
    let indent_remainder = indent_spaces - indent_factor * indent_spaces_unit;
    if indent_remainder > 0 {
        // TODO - handle indentation error here
        todo!()
    } else {
        if indent_spaces > indent_spaces_unit * parser.get_curr_indent_level() {
            // TODO - handle indentation error here
            todo!()
        } else {
            // block is over
            parser.reset_indent_level(parser.get_curr_indent_level() - 1);
            parser.reset_lookahead(curr_lookahead);
            BlockNode::new(stmts, params, parent)
        }
    }
}

pub fn block(parser: &mut PackratParser, params: &Rc<Vec<ParamNode>>, 
    parent: Option<ASTNode>) -> BlockNode {
    let newline_node = parser.expect("\n", false);
    let mut curr_lookahead = parser.get_curr_lookahead();
    parser.reset_indent_level(parser.get_curr_indent_level() + 1);
    let stmts_vec: Rc<RefCell<Vec<StatementNode>>> = Rc::new(RefCell::new(vec![]));
    loop {
        let indent_node 
        = match parser.expect_indent_spaces(curr_lookahead, &stmts_vec, params, &parent) {
            IndentNode::BLOCK(block_node) => return block_node,
            IndentNode::TOKEN(token_node) => token_node,
        };
        let stmt_node = parser.stmt();
        stmts_vec.as_ref().borrow_mut().push(stmt_node);
        curr_lookahead = parser.get_curr_lookahead();
    }
}