use crate::ast::ast::StatementNode;
use crate::parser::parser::{PackratParser};
use crate::lexer::token::{Token,CoreToken};
use crate::parser::components::expression::core::is_expression_starting_with;

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
        _                               => is_expression_starting_with(token)
    }
}

pub const STATEMENT_EXPECTED_STARTING_SYMBOLS: [&'static str; 8] 
= ["let", "def", "for", "while", "if", "type", "interface", "impl"];

pub fn stmt(parser: &mut PackratParser) -> StatementNode {
    todo!()
}