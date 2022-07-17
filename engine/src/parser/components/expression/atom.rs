use crate::ast::ast::{AtomStartNode, AtomNode};
use crate::{parser::parser::PackratParser, constants::common::IDENTIFIER};
use crate::lexer::token::CoreToken;

pub fn atom(parser: &mut PackratParser, atom_start: AtomNode) -> AtomNode {
    todo!()
}

pub fn atom_start(parser: &mut PackratParser) -> AtomNode {
    let leading_identifier_node = parser.expect(IDENTIFIER, false);
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::DOT              => {
            // return parser.atom(leading_identifier_node);
        },
        CoreToken::LPAREN           => {},
        CoreToken::DOUBLE_COLON     => {},
        CoreToken::LSQUARE          => {},
        _                           => {}
    }
    todo!()
}