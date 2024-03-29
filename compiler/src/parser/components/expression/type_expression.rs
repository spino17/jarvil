use crate::ast::ast::TypeExpressionNode;
use crate::ast::traits::{ErrornousNode, Node};
use crate::constants::common::{ATOMIC_TYPE, IDENTIFIER};
use crate::lexer::token::CoreToken;
use crate::parser::parser::JarvilParser;

pub const TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS: [&str; 5] =
    [ATOMIC_TYPE, IDENTIFIER, "[", "{", "("];

pub fn ty_expr(parser: &mut JarvilParser) -> TypeExpressionNode {
    let token = parser.curr_token();
    match token.core_token() {
        CoreToken::ATOMIC_TYPE => {
            let atomic_ty_node = parser.expect(ATOMIC_TYPE);
            TypeExpressionNode::new_with_atomic_ty(atomic_ty_node)
        }
        CoreToken::IDENTIFIER => {
            let identifier_node = parser.expect_identifier_in_use();
            TypeExpressionNode::new_with_user_defined_ty(identifier_node)
        }
        CoreToken::LSQUARE => {
            let lsquare_node = parser.expect("[");
            let sub_ty_node = parser.ty_expr();
            let rsquare_node = parser.expect("]");
            TypeExpressionNode::new_with_array_ty(sub_ty_node, lsquare_node, rsquare_node)
        }
        CoreToken::LPAREN => {
            let lparen_node = parser.expect("(");
            let (types_node, num_types) = parser.ty_tuple();
            let rparen_node = parser.expect(")");
            if num_types < 2 {
                parser.log_single_sub_ty_in_tuple_error(types_node.range());
            }
            TypeExpressionNode::new_with_tuple_ty(lparen_node, rparen_node, types_node)
        }
        CoreToken::LBRACE => {
            let lcurly_node = parser.expect("{");
            let key_ty_node = parser.ty_expr();
            let colon_node = parser.expect(":");
            let value_ty_node = parser.ty_expr();
            let rcurly_node = parser.expect("}");
            TypeExpressionNode::new_with_hashmap_ty(
                lcurly_node,
                rcurly_node,
                colon_node,
                key_ty_node,
                value_ty_node,
            )
        }
        _ => {
            parser.log_missing_token_error(&TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS, token);
            TypeExpressionNode::new_with_missing_tokens(
                TYPE_EXPRESSION_EXPECTED_STARTING_SYMBOLS.to_vec(),
                token.clone(),
            )
        }
    }
}
