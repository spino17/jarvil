use crate::{parser::parser::PackratParser, ast::ast::AssignmentNode};

pub fn assignment(parser: &mut PackratParser) -> AssignmentNode {
    let atom_node = parser.atom();
    let equal_node = parser.expect("=", true);
    let r_assign_node = parser.r_assign(None);
    AssignmentNode::new(&atom_node, &r_assign_node)
}