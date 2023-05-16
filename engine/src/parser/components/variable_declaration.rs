use super::expression::core::is_expression_starting_with;
use crate::ast::ast::{ErrornousNode, LambdaDeclarationNode, Node};
use crate::constants::common::LAMBDA_KEYWORD;
use crate::{
    ast::ast::RVariableDeclarationNode,
    lexer::token::{CoreToken, Token},
};
use crate::{ast::ast::VariableDeclarationNode, parser::parser::PackratParser};
use std::rc::Rc;

pub const R_VARIABLE_DECLARATION_STARTING_SYMBOLS: [&'static str; 2] = ["<expression>", "lambda"];

pub fn is_r_variable_declaration_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::LAMBDA_KEYWORD => true,
        _ => is_expression_starting_with(token),
    }
}

pub fn variable_decl(parser: &mut PackratParser) -> VariableDeclarationNode {
    let let_keyword = parser.expect("let");
    let identifier_node = parser.expect_ident();
    let equal_node = parser.expect("=");
    let token = &parser.curr_token();
    if !is_r_variable_declaration_starting_with(token) {
        parser.log_missing_token_error(&["<expression>", "lambda"], token);
        let r_node = RVariableDeclarationNode::new_with_missing_tokens(
            &Rc::new(R_VARIABLE_DECLARATION_STARTING_SYMBOLS.to_vec()),
            token,
        );
        return VariableDeclarationNode::new(&identifier_node, &r_node, &let_keyword, &equal_node);
    }
    let r_node = match token.core_token {
        CoreToken::LAMBDA_KEYWORD => {
            let lambda_keyword = parser.expect(LAMBDA_KEYWORD);
            let callable_body = parser.callable_body();
            let lambda_decl_node =
                LambdaDeclarationNode::new(&identifier_node, &lambda_keyword, &callable_body);
            RVariableDeclarationNode::new_with_lambda(&lambda_decl_node)
        }
        _ => {
            let expr_node = parser.expr();
            let newline = parser.expect_terminators();
            RVariableDeclarationNode::new_with_expr(&expr_node, &newline)
        }
    };
    return VariableDeclarationNode::new(&identifier_node, &r_node, &let_keyword, &equal_node);
}
