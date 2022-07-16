use crate::{lexer::token::{Token, CoreToken}, constants::common::{INTEGER, FLOATING_POINT_NUMBER, STRING_LITERAL, 
IDENTIFIER, ATOMIC_TYPE}, parser::parser::PackratParser};

pub fn is_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::PLUS                         => true,
        CoreToken::DASH                         => true,
        CoreToken::NOT                          => true,
        CoreToken::INTEGER(_)                   => true,
        CoreToken::FLOATING_POINT_NUMBER(_)     => true,
        CoreToken::LITERAL(_)                   => true,
        CoreToken::IDENTIFIER(_)                => true,
        _                                       => false,
    }
}

pub const EXPRESSION_EXPECTED_STARTING_SYMBOLS: [&'static str; 7]
= ["+", "-", "not", INTEGER, FLOATING_POINT_NUMBER, STRING_LITERAL, IDENTIFIER];

pub fn expr(parser: &mut PackratParser) {
    let token = &parser.curr_token();
    if !is_expression_starting_with(token) {
        parser.log_skipped_token_error(&EXPRESSION_EXPECTED_STARTING_SYMBOLS, &token);
        // TODO - return missing tokens AST node
    }
}