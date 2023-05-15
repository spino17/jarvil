use crate::{
    ast::ast::{AssignmentNode, ExpressionNode, Node},
    parser::parser::PackratParser,
};

pub fn assignment(parser: &mut PackratParser, expr: &ExpressionNode) -> AssignmentNode {
    let equal_node = parser.expect("=");
    let (r_assign_node, is_lambda) = parser.r_assign(None);
    if let Some(lambda_range) = is_lambda {
        parser.log_invalid_r_lambda_error(lambda_range);
        return AssignmentNode::new_with_invalid_l_value(&expr, &r_assign_node, &equal_node);
    }
    match expr.is_valid_l_value() {
        Some(atom_node) => AssignmentNode::new(&atom_node, &r_assign_node, &equal_node),
        None => {
            parser.log_invalid_l_value_error(expr.range());
            AssignmentNode::new_with_invalid_l_value(&expr, &r_assign_node, &equal_node)
        }
    }
}
