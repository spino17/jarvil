use crate::lexer::token::Token;

use super::type_expression::is_type_expression_starting_with;

pub fn is_expression_starting_with(token: &Token) -> bool {
    // TODO - add here other alternatives of expressions
    is_type_expression_starting_with(token)
}