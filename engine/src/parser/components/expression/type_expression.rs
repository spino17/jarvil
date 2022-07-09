use crate::ast::ast::TypeExpressionNode; 
use crate::parser::parser::{PackratParser};
use crate::lexer::token::CoreToken;

pub fn type_expr(parser: &mut PackratParser) -> TypeExpressionNode {
    parser.ignore_whitespaces();
    let token = parser.get_curr_token();
    match &token.core_token {
        CoreToken::ATOMIC_TYPE(atomic_type) => {
            parser.scan_next_token();
            TypeExpressionNode::new_with_atomic_type(&atomic_type.0)
        },
        CoreToken::IDENTIFIER(_) => {
            let identifier_node = parser.expect_ident(false);
            TypeExpressionNode::new_with_user_defined_type(&identifier_node)
        },
        CoreToken::LSQUARE => {
            let l_square_node = parser.expect("[", false);
            let sub_type_node = parser.expect_type_expr();
            let common_node = parser.expect(",", false);
            let array_size_node = parser.expect_int(false);
            let r_square_node = parser.expect("]", false);
            TypeExpressionNode::new_with_array_type(&array_size_node, &sub_type_node)
        },
        _ => {
            todo!()
        }
    }
}