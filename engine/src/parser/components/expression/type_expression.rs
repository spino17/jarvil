use crate::ast::ast::TypeExpressionNode;
use crate::constants::common::{INTEGER, IDENTIFIER, ATOMIC_TYPE};
use crate::parser::parser::{PackratParser};
use crate::lexer::token::{CoreToken, Token};
use std::rc::Rc;
use crate::ast::ast::ErrornousNode;

pub fn is_type_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::ATOMIC_TYPE      => true,
        CoreToken::IDENTIFIER       => true,
        CoreToken::LSQUARE          => true,
        _                           => false,
    }
}

pub const TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS: [&'static str; 3] 
= [ATOMIC_TYPE, IDENTIFIER, "["];

pub fn type_expr(parser: &mut PackratParser) -> TypeExpressionNode {
    let token = &parser.curr_token();
    if !is_type_expression_starting_with(token) {
        parser.log_missing_token_error_for_multiple_expected_symbols(
            &TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS, &token
        );
        return TypeExpressionNode::new_with_missing_tokens(
            &Rc::new(TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS.to_vec()),
            &token,
            parser.curr_lookahead(),
        )
    }
    match token.core_token {
        CoreToken::ATOMIC_TYPE  => {
            let atomic_type_node = parser.expect(ATOMIC_TYPE);
            TypeExpressionNode::new_with_atomic_type(&atomic_type_node)
        },
        CoreToken::IDENTIFIER   => {
            let identifier_node = parser.expect(IDENTIFIER);
            TypeExpressionNode::new_with_user_defined_type(&identifier_node)
        },
        CoreToken::LSQUARE      => {
            let lsquare_node = parser.expect("[");
            let sub_type_node = parser.type_expr();
            let semicolon_node = parser.expect(";");
            let array_size_node = parser.expect(INTEGER);
            let rsquare_node = parser.expect("]");
            TypeExpressionNode::new_with_array_type(
                &array_size_node, &sub_type_node, &lsquare_node, &rsquare_node, &semicolon_node
            )
        },
        _ => unreachable!("tokens not matching `starting_with_symbols` for type expression would already be eliminated")
    }
}