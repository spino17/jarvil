use super::expression::core::is_expr_starting_with;
use crate::{
    ast::{
        ast::{AssignmentNode, ExpressionNode, RAssignmentNode},
        traits::Node,
    },
    lexer::token::Token,
    parser::parser::JarvilParser,
};
pub const R_ASSIGNMENT_STARTING_SYMBOLS: [&str; 1] = ["<expression>"];

pub fn is_r_assignment_starting_with(token: &Token) -> bool {
    is_expr_starting_with(token)
}

pub fn assignment(parser: &mut JarvilParser, l_expr: ExpressionNode) -> AssignmentNode {
    let equal_node = parser.expect("=");
    let expr_node = parser.expr();
    let newline = parser.expect_terminators();
    let r_assign_node = RAssignmentNode::new_with_expr(expr_node, newline);

    match l_expr.is_valid_l_value() {
        Some(atom_node) => AssignmentNode::new(atom_node, r_assign_node, equal_node),
        None => {
            parser.log_invalid_l_value_error(l_expr.range());
            AssignmentNode::new_with_invalid_l_value(l_expr, r_assign_node, equal_node)
        }
    }
}
