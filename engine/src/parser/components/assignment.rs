use crate::{
    ast::ast::{AssignmentNode, ExpressionNode, Node},
    parser::parser::PackratParser,
};

pub fn assignment(parser: &mut PackratParser, expr: &ExpressionNode) -> AssignmentNode {
    let equal_node = parser.expect("=");
    let r_assign_node = parser.r_assign(None);
    match expr.is_valid_l_value() {
        Some(atom_node) => AssignmentNode::new(&atom_node, &r_assign_node, &equal_node),
        None => {
            parser.log_invalid_l_value_error(
                expr.start_index(),
                expr.end_index(),
                expr.start_line_number(),
            );
            AssignmentNode::new_with_invalid_l_value(&expr, &r_assign_node, &equal_node)
        }
    }
}
