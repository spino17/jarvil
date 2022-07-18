use crate::{parser::parser::PackratParser, constants::common::IDENTIFIER, ast::ast::VariableDeclarationNode};

pub fn variable_decl(parser: &mut PackratParser) -> VariableDeclarationNode {
    let let_node = parser.expect("let", false);
    let identifier_node = parser.expect(IDENTIFIER, false);
    let equal_node = parser.expect("=", false);
    let r_expr = parser.expr();
    let newline_node = parser.expect("\n", false);
    VariableDeclarationNode::new(&identifier_node, &r_expr)
}