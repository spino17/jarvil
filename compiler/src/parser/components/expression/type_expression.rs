use crate::ast::ast::ErrornousNode;
use crate::ast::ast::Node;
use crate::ast::ast::TypeExpressionNode;
use crate::constants::common::{ATOMIC_TYPE, IDENTIFIER};
use crate::lexer::token::CoreToken;
use crate::parser::errors::log_missing_token_error;
use crate::parser::errors::log_single_sub_type_in_tuple_error;
use crate::parser::parser::JarvilParser;

pub const TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS: [&'static str; 5] =
    [ATOMIC_TYPE, IDENTIFIER, "[", "{", "("];

pub fn type_expr(parser: &mut JarvilParser) -> TypeExpressionNode {
    let token = parser.curr_token();
    match token.core_token {
        CoreToken::ATOMIC_TYPE => {
            let atomic_type_node = parser.expect(ATOMIC_TYPE);
            TypeExpressionNode::new_with_atomic_type(&atomic_type_node)
        }
        CoreToken::IDENTIFIER => {
            let identifier_node = parser.expect_ident(); // usage
            TypeExpressionNode::new_with_user_defined_type(&identifier_node)
        }
        CoreToken::LSQUARE => {
            let lsquare_node = parser.expect("[");
            let sub_type_node = parser.type_expr();
            let rsquare_node = parser.expect("]");
            TypeExpressionNode::new_with_array_type(&sub_type_node, &lsquare_node, &rsquare_node)
        }
        CoreToken::LPAREN => {
            let lparen_node = parser.expect("(");
            let (types_node, num_types) = parser.type_tuple();
            // TODO - check here that `num_types` > 1 as tuple type should have atleast two subtypes
            let rparen_node = parser.expect(")");
            if num_types < 2 {
                log_single_sub_type_in_tuple_error(parser, types_node.range());
            }
            TypeExpressionNode::new_with_tuple_type(&lparen_node, &rparen_node, &types_node)
        }
        CoreToken::LBRACE => {
            let lcurly_node = parser.expect("{");
            let key_type_node = parser.type_expr();
            let colon_node = parser.expect(":");
            let value_type_node = parser.type_expr();
            let rcurly_node = parser.expect("}");
            TypeExpressionNode::new_with_hashmap_type(
                &lcurly_node,
                &rcurly_node,
                &colon_node,
                &key_type_node,
                &value_type_node,
            )
        }
        _ => {
            log_missing_token_error(parser, &TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS, &token);
            return TypeExpressionNode::new_with_missing_tokens(
                TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS.to_vec(),
                &token,
            );
        }
    }
}
