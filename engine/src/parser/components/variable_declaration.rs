use crate::{
    ast::ast::VariableDeclarationNode,
    constants::common::{IDENTIFIER},
    parser::parser::PackratParser,
};

pub fn variable_decl(parser: &mut PackratParser) -> VariableDeclarationNode {
    let let_node = parser.expect("let");
    let identifier_node = parser.expect(IDENTIFIER);
    let equal_node = parser.expect("=");
    let r_assign_node = parser.r_assign(Some(&identifier_node));
    VariableDeclarationNode::new(
        &identifier_node,
        &r_assign_node,
        &let_node,
        &equal_node,
    )
}
