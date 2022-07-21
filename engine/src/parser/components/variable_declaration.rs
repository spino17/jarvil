use crate::{parser::parser::PackratParser, constants::common::{IDENTIFIER, ENDMARKER}, ast::ast::VariableDeclarationNode};

pub fn variable_decl(parser: &mut PackratParser) -> VariableDeclarationNode {
    let let_node = parser.expect("let", false);
    let identifier_node = parser.expect(IDENTIFIER, false);
    let equal_node = parser.expect("=", false);
    // let r_expr = parser.expr();
    let r_assign_node = parser.r_assign();
    let newline_node = parser.expects(&["\n", ENDMARKER], false);
    VariableDeclarationNode::new(&identifier_node, &r_assign_node)
}