use crate::ast::ast::{RAssignmentNode, ErrornousNode, TokenNode};
use crate::parser::parser::PackratParser;
use crate::lexer::token::{Token,CoreToken};
use std::rc::Rc;
use super::expression::core::is_expression_starting_with;

pub fn is_r_assign_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::FUNC => true,
        _ => is_expression_starting_with(token)
    }
}

pub const R_ASSIGNMENT_STARTING_SYMBOLS: [&'static str; 2] = ["<expression>", "func"];

pub fn r_assign(parser: &mut PackratParser, identifier_name: Option<&TokenNode>) -> RAssignmentNode {
    let token = &parser.curr_token();
    if !is_r_assign_starting_with(token) {
        parser.log_missing_token_error_for_multiple_expected_symbols(
            &R_ASSIGNMENT_STARTING_SYMBOLS, token
        );
        return RAssignmentNode::new_with_missing_tokens(
            &Rc::new(R_ASSIGNMENT_STARTING_SYMBOLS.to_vec()), 
            token, 
            parser.curr_lookahead(),
        )
    }
    match token.core_token {
        CoreToken::FUNC => {
            let func_keyword_node = parser.expect("func");
            let func_decl_node = parser.function_decl(identifier_name);
            RAssignmentNode::new_with_lambda(&func_decl_node)
        },
        _ => {
            let expr_node = parser.expr();
            RAssignmentNode::new_with_expr(&expr_node)
        }
    }
}