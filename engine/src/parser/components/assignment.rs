use crate::{
    ast::ast::{AssignmentNode, ExpressionNode},
    parser::parser::PackratParser,
};

pub fn assignment(parser: &mut PackratParser, expr: &ExpressionNode) -> AssignmentNode {
    let equal_node = parser.expect("=");
    let r_assign_node = parser.r_assign(None);
    match expr.is_valid_l_value() {
        Some(atom_node) => AssignmentNode::new(&atom_node, &r_assign_node, &equal_node),
        None => {
            // TODO - log the error and return an appropiate node
            todo!()
        }
    }
}
