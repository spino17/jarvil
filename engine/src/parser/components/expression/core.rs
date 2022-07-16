use crate::{lexer::token::{Token, CoreToken}, constants::common::{INTEGER, FLOATING_POINT_NUMBER, STRING_LITERAL, 
IDENTIFIER}, parser::parser::PackratParser};

pub fn is_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        _  => is_atomic_expression_starting_with(token) || is_unary_expression_starting_with(token),
    }
}

pub const EXPRESSION_EXPECTED_STARTING_SYMBOLS: [&'static str; 10] = UNARY_EXPRESSION_STARTING_SYMBOLS;

pub fn expr(parser: &mut PackratParser) {
    let token = &parser.curr_token();
    if !is_expression_starting_with(token) {
        parser.log_skipped_token_error(&EXPRESSION_EXPECTED_STARTING_SYMBOLS, token);
        // TODO - return missing tokens AST node
    }
    while let Some(node) = parser.expects(&["+", "-"], false).is_ok() {
        todo!()
    }
}

pub fn is_unary_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::PLUS => true,
        CoreToken::DASH => true,
        CoreToken::NOT  => true,
        _               => is_atomic_expression_starting_with(token),
    }
}

pub const UNARY_EXPRESSION_STARTING_SYMBOLS: [&'static str; 10] 
= ["+", "-", "not", "true", "false", INTEGER, FLOATING_POINT_NUMBER, STRING_LITERAL, IDENTIFIER, "("];

pub fn unary_expr(parser: &mut PackratParser) {
    let token = &parser.curr_token();
    if !is_unary_expression_starting_with(token) {
        parser.log_skipped_token_error(&UNARY_EXPRESSION_STARTING_SYMBOLS, token)
    }
    match token.core_token {
        CoreToken::PLUS => {
            let plus_node = parser.expect("+", false);
            parser.unary_expr();
        },
        CoreToken::DASH => {
            let dash_node = parser.expect("-", false);
            parser.unary_expr();
        },
        CoreToken::NOT  => {
            let not_node = parser.expect("not", false);
            parser.unary_expr();
        },
        _ => return parser.atomic_expr()
    }
}

pub fn is_atomic_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::INTEGER(_)                   => true,
        CoreToken::FLOATING_POINT_NUMBER(_)     => true,
        CoreToken::LITERAL(_)                   => true,
        CoreToken::TRUE                         => true,
        CoreToken::FALSE                        => true,
        CoreToken::IDENTIFIER(_)                => true,
        CoreToken::LPAREN                       => true,
        _                                       => false,
    }
}

pub const ATOMIC_EXPRESSION_STARTING_SYMBOLS: [&'static str; 7] 
= ["true", "false", INTEGER, FLOATING_POINT_NUMBER, STRING_LITERAL, IDENTIFIER, "("];

pub fn atomic_expr(parser: &mut PackratParser) {
    let token = &parser.curr_token();
    if !is_atomic_expression_starting_with(token) {
        parser.log_skipped_token_error(&ATOMIC_EXPRESSION_STARTING_SYMBOLS, token)
    }
    let token_node = match token.core_token {
        CoreToken::TRUE                         => parser.expect("true", false),
        CoreToken::FALSE                        => parser.expect("false", false),
        CoreToken::INTEGER(_)                   => parser.expect(INTEGER, false),
        CoreToken::FLOATING_POINT_NUMBER(_)     => parser.expect(FLOATING_POINT_NUMBER, false),
        CoreToken::LITERAL(_)                   => parser.expect(STRING_LITERAL, false),
        CoreToken::IDENTIFIER(_)                => {
            // parser.atom();
            todo!("parsing routine for atom is yet to be implemented")
        }
        CoreToken::LPAREN                       => {
            let lparen_node = parser.expect("(", false);
            parser.expr();
            let rparen_node = parser.expect(")", false);
            todo!()
        }
        _ => unreachable!("tokens not matching `starting_with_symbols` for atomic expression would already be eliminated")
    };
}