use crate::ast::ast::StatementNode;
use crate::constants::common::IDENTIFIER;
use crate::parser::parser::{PackratParser};
use crate::lexer::token::{Token,CoreToken};
use crate::parser::components::expression::core::is_expression_starting_with;
use std::rc::Rc;

pub fn is_statement_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::LET                  => true,
        CoreToken::DEF                  => true,
        CoreToken::FOR                  => true,
        CoreToken::WHILE                => true,
        CoreToken::IF                   => true,
        CoreToken::TYPE_KEYWORD         => true,
        CoreToken::INTERFACE_KEYWORD    => true,
        CoreToken::IMPL                 => true,
        CoreToken::IDENTIFIER(_)        => true,
        _                               => is_expression_starting_with(token),
    }
}

pub const STATEMENT_EXPECTED_STARTING_SYMBOLS: [&'static str; 10]
= ["let", "def", "for", "while", "if", "type", "interface", "impl", IDENTIFIER, "expression"];

pub fn stmt(parser: &mut PackratParser) -> StatementNode {
    let token = &parser.curr_token();
    if !is_statement_starting_with(token) {
        parser.log_missing_token_error_for_multiple_expected_symbols(
            &STATEMENT_EXPECTED_STARTING_SYMBOLS, token
        );
        return StatementNode::new_with_missing_tokens(
            &Rc::new(STATEMENT_EXPECTED_STARTING_SYMBOLS.to_vec()), 
            token,
            parser.curr_lookahead(),
        )
    }
    let statement_node = match token.core_token {
        CoreToken::LET                  => {
            let variable_decl_node = parser.variable_decl();
            StatementNode::new_with_variable_declaration(&variable_decl_node)
        },
        CoreToken::DEF                  => todo!(),
        CoreToken::FOR                  => todo!(),
        CoreToken::WHILE                => todo!(),
        CoreToken::IF                   => todo!(),
        CoreToken::TYPE_KEYWORD         => todo!(),
        CoreToken::INTERFACE_KEYWORD    => todo!(),
        CoreToken::IMPL                 => todo!(),
        _                               => {
            let expr_node = parser.expr();
            StatementNode::new_with_expression(&expr_node)
        }
    };
    statement_node
}