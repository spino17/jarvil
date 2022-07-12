use crate::ast::ast::StatementNode;
use crate::parser::parser::{PackratParser};
use crate::lexer::token::{Token,CoreToken};

use super::expression::core::is_expression_starting_with;

pub fn is_statement_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::LET                  => true,
        CoreToken::DEF                  => true,
        CoreToken::RETURN               => true,
        CoreToken::FOR                  => true,
        CoreToken::WHILE                => true,
        CoreToken::CONTINUE             => true,
        CoreToken::BREAK                => true,
        CoreToken::IF                   => true,
        CoreToken::TYPE_KEYWORD         => true,
        CoreToken::INTERFACE_KEYWORD    => true,
        CoreToken::IMPL                 => true,
        _                               => is_expression_starting_with(token)
    }
}

pub fn stmt(parser: &mut PackratParser) -> StatementNode {
    todo!()
}