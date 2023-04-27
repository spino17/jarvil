use crate::ast::ast::{ErrornousNode, TokenNode};
use crate::{
    ast::ast::{AtomicExpressionNode, ExpressionNode, UnaryExpressionNode},
    constants::common::{FALSE, FLOATING_POINT_NUMBER, IDENTIFIER, INTEGER, LITERAL, NOT, TRUE},
    lexer::token::UnaryOperatorKind,
    lexer::token::{CoreToken, Token},
    parser::parser::PackratParser,
};
use std::rc::Rc;

// all the unary operators are right assosiative and all the binary operators are left assosiative.
// below is the operator precedence in jarvil (lower to higher). This may be quite resembling with Python programming language.
// "or"
// "and"
// ">", ">=", "<", "<=", "==", "!="
// "-", "+"
// "/", "*"
// "+", "-", "not", (...)

pub fn is_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        _ => is_unary_expression_starting_with(token),
    }
}

pub const EXPRESSION_EXPECTED_STARTING_SYMBOLS: [&'static str; 10] =
    UNARY_EXPRESSION_STARTING_SYMBOLS;

pub fn expr(parser: &mut PackratParser) -> ExpressionNode {
    let token = &parser.curr_token();
    if !is_expression_starting_with(token) {
        parser.log_missing_token_error(&EXPRESSION_EXPECTED_STARTING_SYMBOLS, token);
        return ExpressionNode::new_with_missing_tokens(
            &Rc::new(EXPRESSION_EXPECTED_STARTING_SYMBOLS.to_vec()),
            token,
        );
    }
    parser.pratt_expr(0)
}

pub fn is_unary_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::PLUS => true,
        CoreToken::DASH => true,
        CoreToken::NOT => true,
        _ => is_atomic_expression_starting_with(token),
    }
}

pub const UNARY_EXPRESSION_STARTING_SYMBOLS: [&'static str; 10] = [
    "+",
    "-",
    NOT,
    TRUE,
    FALSE,
    INTEGER,
    FLOATING_POINT_NUMBER,
    LITERAL,
    IDENTIFIER,
    "(",
];

pub fn unary_expr(parser: &mut PackratParser) -> UnaryExpressionNode {
    let token = &parser.curr_token();
    if !is_unary_expression_starting_with(token) {
        parser.log_missing_token_error(&UNARY_EXPRESSION_STARTING_SYMBOLS, token);
        return UnaryExpressionNode::new_with_missing_tokens(
            &Rc::new(UNARY_EXPRESSION_STARTING_SYMBOLS.to_vec()),
            token,
        );
    }
    let unary_expr_node = match token.core_token {
        CoreToken::PLUS => {
            let plus_node = parser.expect("+");
            let unary_expr_node = parser.unary_expr();
            UnaryExpressionNode::new_with_unary(
                &unary_expr_node,
                &plus_node,
                UnaryOperatorKind::Plus,
            )
        }
        CoreToken::DASH => {
            let dash_node = parser.expect("-");
            let unary_expr_node = parser.unary_expr();
            UnaryExpressionNode::new_with_unary(
                &unary_expr_node,
                &dash_node,
                UnaryOperatorKind::Minus,
            )
        }
        CoreToken::NOT => {
            let not_node = parser.expect("not");
            let unary_expr_node = parser.unary_expr();
            UnaryExpressionNode::new_with_unary(&unary_expr_node, &not_node, UnaryOperatorKind::Not)
        }
        _ => {
            let atomic_expr_node = parser.atomic_expr();
            UnaryExpressionNode::new_with_atomic(&atomic_expr_node)
        }
    };
    unary_expr_node
}

pub fn is_atomic_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::INTEGER => true,
        CoreToken::FLOATING_POINT_NUMBER => true,
        CoreToken::LITERAL => true,
        CoreToken::TRUE => true,
        CoreToken::FALSE => true,
        CoreToken::IDENTIFIER => true,
        CoreToken::LPAREN => true,
        _ => false,
    }
}

pub const ATOMIC_EXPRESSION_STARTING_SYMBOLS: [&'static str; 7] = [
    TRUE,
    FALSE,
    INTEGER,
    FLOATING_POINT_NUMBER,
    LITERAL,
    IDENTIFIER,
    "(",
];

pub fn atomic_expr(parser: &mut PackratParser) -> AtomicExpressionNode {
    let token = &parser.curr_token();
    if !is_atomic_expression_starting_with(token) {
        parser.log_missing_token_error(&ATOMIC_EXPRESSION_STARTING_SYMBOLS, token);
        return AtomicExpressionNode::new_with_missing_tokens(
            &Rc::new(ATOMIC_EXPRESSION_STARTING_SYMBOLS.to_vec()),
            token,
        );
    }
    let atomic_expr_node = match token.core_token {
        CoreToken::TRUE                         => {
            let true_node = parser.expect(TRUE);
            AtomicExpressionNode::new_with_bool(&true_node)
        }
        CoreToken::FALSE                        => {
            let false_node = parser.expect(FALSE);
            AtomicExpressionNode::new_with_bool(&false_node)
        }
        CoreToken::INTEGER                      => {
            let integer_node = parser.expect(INTEGER);
            AtomicExpressionNode::new_with_integer(&integer_node)
        }
        CoreToken::FLOATING_POINT_NUMBER        => {
            let floating_point_number_node = parser.expect(FLOATING_POINT_NUMBER);
            AtomicExpressionNode::new_with_floating_point_number(&floating_point_number_node)
        }
        CoreToken::LITERAL                      => {
            let literal_node = parser.expect(LITERAL);
            AtomicExpressionNode::new_with_literal(&literal_node)
        }
        CoreToken::IDENTIFIER                   => {
            let atom = parser.atom();
            AtomicExpressionNode::new_with_atom(&atom)
        }
        CoreToken::LPAREN                       => {
            let lparen_node = parser.expect("(");
            let expr_node = parser.expr();
            let rparen_node = parser.expect(")");
            AtomicExpressionNode::new_with_parenthesised_expr(&expr_node, &lparen_node, &rparen_node)
        }
        _ => unreachable!("tokens not matching `starting_with_symbols` for atomic expression would already be eliminated")
    };
    atomic_expr_node
}
