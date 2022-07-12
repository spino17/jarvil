use crate::ast::ast::TypeExpressionNode; 
use crate::constants::common::{INTEGER, IDENTIFIER};
use crate::parser::parser::{PackratParser};
use crate::lexer::token::{CoreToken, Token};

pub fn is_type_expr_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::ATOMIC_TYPE(_)   => true,
        CoreToken::IDENTIFIER(_)    => true,
        CoreToken::LSQUARE          => true,
        _                           => false
    }
}

pub fn type_expr(parser: &mut PackratParser) -> TypeExpressionNode {
    let token = parser.get_curr_token();
    match &token.core_token {
        CoreToken::ATOMIC_TYPE(atomic_type) => {
            parser.scan_next_token();
            TypeExpressionNode::new_with_atomic_type(&atomic_type.0)
        },
        CoreToken::IDENTIFIER(_) => {
            let identifier_node = parser.expect(IDENTIFIER, false);
            TypeExpressionNode::new_with_user_defined_type(&identifier_node)
        },
        CoreToken::LSQUARE => {
            let l_square_node = parser.expect("[", false);
            let sub_type_node = parser.type_expr();
            let comma_node = parser.expect(",", false);
            let array_size_node = parser.expect(INTEGER, false);
            let r_square_node = parser.expect("]", false);
            TypeExpressionNode::new_with_array_type(&array_size_node, &sub_type_node)
        },
        _ => {
            unreachable!("curr token not matching with starting token of type expression is not possible")
        }
    }
}