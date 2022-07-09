use crate::ast::ast::TypeExpressionNode; 
use crate::parser::parser::{PackratParser, tokenize};
use crate::lexer::token::CoreToken;

pub fn type_expr(parser: &mut PackratParser) -> TypeExpressionNode {
    parser.ignore_blanks();
    let token = parser.get_curr_token();
    match &token.core_token {
        CoreToken::ATOMIC_TYPE(atomic_type) => {
            parser.scan_next_token();
            TypeExpressionNode::new_with_atomic_type(&atomic_type.0)
        },
        CoreToken::IDENTIFIER(_) => {
            let identifier_node = tokenize(parser.expect_ident());
            TypeExpressionNode::new_with_user_defined_type(&identifier_node)
        },
        CoreToken::LSQUARE => {
            let l_square_node = tokenize(parser.expect("["));
            let sub_type_node = parser.expect_type_expr();
            let common_node = tokenize(parser.expect(","));
            let array_size_node = tokenize(parser.expect_int());
            let r_square_node = tokenize(parser.expect("]"));
            TypeExpressionNode::new_with_array_type(&array_size_node, &sub_type_node)
        },
        _ => {
            todo!()
        }
    }
}