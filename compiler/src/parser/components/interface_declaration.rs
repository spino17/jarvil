use crate::{
    ast::ast::{DeclareCallablePrototypeNode, InterfaceDeclarationNode},
    constants::common::{DEF, IDENTIFIER},
    lexer::token::CoreToken,
    parser::{parser::JarvilParser, resolver::BlockKind},
};

pub fn interface_decl(parser: &mut JarvilParser) -> InterfaceDeclarationNode {
    let interface_keyword_node = parser.expect("interface");
    let name_node = parser.expect_identifier_in_decl();
    let colon_node = parser.expect(":");
    let block_node = parser.block(
        |token| match token.core_token() {
            CoreToken::IDENTIFIER => true,
            CoreToken::DEF => true,
            _ => false,
        },
        |parser| parser.interface_stmt(),
        &[IDENTIFIER, DEF],
        BlockKind::Interface,
    );
    InterfaceDeclarationNode::new(interface_keyword_node, name_node, colon_node, block_node)
}

pub fn interface_method_prototype_wrapper(
    parser: &mut JarvilParser,
) -> DeclareCallablePrototypeNode {
    let def_keyword_node = parser.expect("def");
    let func_name_node = parser.expect_identifier_in_decl(); // decl
    let prototype = parser.callable_prototype();
    let newline = parser.expect_terminators();
    DeclareCallablePrototypeNode::new(def_keyword_node, func_name_node, prototype, newline)
}
