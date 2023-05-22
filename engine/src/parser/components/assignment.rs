use super::expression::core::is_expression_starting_with;
use crate::{
    ast::ast::{AssignmentNode, ErrornousNode, ExpressionNode, Node, RAssignmentNode},
    lexer::token::Token,
    parser::parser::JarvilParser,
};
use std::rc::Rc;
pub const R_ASSIGNMENT_STARTING_SYMBOLS: [&'static str; 1] = ["<expression>"];

pub fn is_r_assignment_starting_with(token: &Token) -> bool {
    is_expression_starting_with(token)
}

pub fn assignment(parser: &mut JarvilParser, l_expr: &ExpressionNode) -> AssignmentNode {
    let equal_node = parser.expect("=");
    let token = &parser.curr_token();
    let r_assign_node = if !is_r_assignment_starting_with(token) {
        parser.log_missing_token_error(&["<expression>"], token);
        RAssignmentNode::new_with_missing_tokens(
            &Rc::new(R_ASSIGNMENT_STARTING_SYMBOLS.to_vec()),
            token,
        )
    } else {
        let expr_node = parser.expr();
        let newline = parser.expect_terminators();
        RAssignmentNode::new_with_expr(&expr_node, &newline)
    };
    match l_expr.is_valid_l_value() {
        Some(atom_node) => AssignmentNode::new(&atom_node, &r_assign_node, &equal_node),
        None => {
            parser.log_invalid_l_value_error(l_expr.range());
            AssignmentNode::new_with_invalid_l_value(&l_expr, &r_assign_node, &equal_node)
        }
    }
}
