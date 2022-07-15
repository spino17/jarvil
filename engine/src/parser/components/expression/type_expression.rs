use crate::ast::ast::{TypeExpressionNode, TokenNode}; 
use crate::constants::common::{INTEGER, IDENTIFIER, ATOMIC_TYPE};
use crate::parser::parser::{PackratParser};
use crate::lexer::token::{CoreToken, Token};
use std::rc::Rc;
use crate::ast::ast::MissingTokenNode;

pub fn is_type_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::ATOMIC_TYPE(_)   => true,
        CoreToken::IDENTIFIER(_)    => true,
        CoreToken::LSQUARE          => true,
        _                           => false,
    }
}

pub const TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS: [&'static str; 3] 
= [ATOMIC_TYPE, IDENTIFIER, "["];

pub fn type_expr(parser: &mut PackratParser) -> TypeExpressionNode {
    let token = parser.curr_token();
    match &token.core_token {
        CoreToken::ATOMIC_TYPE(_) => {
            let atomic_type_node = parser.expect(ATOMIC_TYPE, false);
            TypeExpressionNode::new_with_atomic_type(&atomic_type_node)
        },
        CoreToken::IDENTIFIER(_) => {
            let identifier_node = parser.expect(IDENTIFIER, false);
            TypeExpressionNode::new_with_user_defined_type(&identifier_node)
        },
        CoreToken::LSQUARE => {
            let l_square_node = parser.expect("[", false);
            let sub_type_node = parser.type_expr();
            let semicolon_node = parser.expect(";", false);
            let array_size_node = parser.expect(INTEGER, false);
            let r_square_node = parser.expect("]", false);
            TypeExpressionNode::new_with_array_type(&array_size_node, &sub_type_node)
        },
        _ => {
            TypeExpressionNode::new_with_missing_tokens(
                &Rc::new(TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS.to_vec()),
                &token,
                parser.curr_lookahead(),
            )
        }
    }
}