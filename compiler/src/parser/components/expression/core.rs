use crate::ast::{
    ast::{KeyValuePairNode, SymbolSeparatedSequenceNode},
    traits::ErrornousNode,
};
use crate::{
    ast::ast::{AtomicExpressionNode, ExpressionNode, UnaryExpressionNode},
    constants::common::{FALSE, FLOATING_POINT_NUMBER, IDENTIFIER, INTEGER, LITERAL, TRUE},
    lexer::token::UnaryOperatorKind,
    lexer::token::{CoreToken, Token},
    parser::parser::JarvilParser,
};

// all the unary operators are right assosiative and all the binary operators are left assosiative.
// below is the operator precedence in jarvil (lower to higher). This may be quite resembling with Python programming language.
// "or"
// "and"
// ">", ">=", "<", "<=", "==", "!="
// "-", "+"
// "/", "*"
// "+", "-", "not", (...)

pub fn is_expr_starting_with(token: &Token) -> bool {
    is_unary_expr_starting_with(token)
}

pub fn expr(parser: &mut JarvilParser) -> ExpressionNode {
    parser.pratt_expr(0)
}

pub fn is_unary_expr_starting_with(token: &Token) -> bool {
    match token.core_token() {
        CoreToken::PLUS => true,
        CoreToken::DASH => true,
        CoreToken::NOT => true,
        _ => is_atomic_expr_starting_with(token),
    }
}

pub fn unary_expr(parser: &mut JarvilParser) -> UnaryExpressionNode {
    let token = parser.curr_token();

    match token.core_token() {
        CoreToken::PLUS => {
            let plus_node = parser.expect("+");
            let unary_expr_node = parser.unary_expr();

            UnaryExpressionNode::new_with_unary(unary_expr_node, plus_node, UnaryOperatorKind::Plus)
        }
        CoreToken::DASH => {
            let dash_node = parser.expect("-");
            let unary_expr_node = parser.unary_expr();

            UnaryExpressionNode::new_with_unary(
                unary_expr_node,
                dash_node,
                UnaryOperatorKind::Minus,
            )
        }
        CoreToken::NOT => {
            let not_node = parser.expect("not");
            let unary_expr_node = parser.unary_expr();

            UnaryExpressionNode::new_with_unary(unary_expr_node, not_node, UnaryOperatorKind::Not)
        }
        _ => {
            let atomic_expr_node = parser.atomic_expr();

            UnaryExpressionNode::new_with_atomic(atomic_expr_node)
        }
    }
}

pub fn is_atomic_expr_starting_with(token: &Token) -> bool {
    match token.core_token() {
        CoreToken::INTEGER => true,
        CoreToken::FLOATING_POINT_NUMBER => true,
        CoreToken::LITERAL => true,
        CoreToken::TRUE => true,
        CoreToken::FALSE => true,
        CoreToken::IDENTIFIER => true,
        CoreToken::SELF => true,
        CoreToken::LPAREN => true,
        CoreToken::LSQUARE => true,
        CoreToken::LBRACE => true,
        _ => false,
    }
}

pub const ATOMIC_EXPR_STARTING_SYMBOLS: [&str; 10] = [
    TRUE,
    FALSE,
    INTEGER,
    FLOATING_POINT_NUMBER,
    LITERAL,
    IDENTIFIER,
    "self",
    "(",
    "[",
    "{",
];

pub fn atomic_expr(parser: &mut JarvilParser) -> AtomicExpressionNode {
    let token = parser.curr_token();
    if !is_atomic_expr_starting_with(token) {
        parser.log_missing_token_error(&ATOMIC_EXPR_STARTING_SYMBOLS, token);

        return AtomicExpressionNode::new_with_missing_tokens(
            ATOMIC_EXPR_STARTING_SYMBOLS.to_vec(),
            token.clone(),
        );
    }
    let atomic_expr_node = match token.core_token() {
        CoreToken::TRUE                         => {
            let true_node = parser.expect(TRUE);
            AtomicExpressionNode::new_with_bool(true_node)
        }
        CoreToken::FALSE                        => {
            let false_node = parser.expect(FALSE);
            AtomicExpressionNode::new_with_bool(false_node)
        }
        CoreToken::INTEGER                      => {
            let integer_node = parser.expect(INTEGER);
            AtomicExpressionNode::new_with_integer(integer_node)
        }
        CoreToken::FLOATING_POINT_NUMBER        => {
            let floating_point_number_node = parser.expect(FLOATING_POINT_NUMBER);
            AtomicExpressionNode::new_with_floating_point_number(floating_point_number_node)
        }
        CoreToken::LITERAL                      => {
            let literal_node = parser.expect(LITERAL);
            AtomicExpressionNode::new_with_literal(literal_node)
        }
        CoreToken::IDENTIFIER | CoreToken::SELF => {
            let atom = parser.atom();
            AtomicExpressionNode::new_with_atom(atom)
        }
        CoreToken::LSQUARE => {
            let mut initials_node = None;
            let lsquare_node = parser.expect("[");
            let curr_token = parser.curr_token();

            if !curr_token.is_eq("]") {
                initials_node = Some(parser.expect_symbol_separated_sequence(|parser: &mut JarvilParser| {
                    parser.expr()
                }, ","));
            }

            let rsquare_node = parser.expect("]");

            AtomicExpressionNode::new_with_array_expr(lsquare_node, rsquare_node, initials_node)
        }
        CoreToken::LBRACE => {
            let mut initials_node = None;
            let lcurly_node = parser.expect("{");
            let curr_token = parser.curr_token();

            if !curr_token.is_eq("}") {
                initials_node = Some(parser.expect_symbol_separated_sequence(|parser: &mut JarvilParser| {
                    let key_expr_node = parser.expr();
                    let colon_node = parser.expect(":");
                    let value_expr_node = parser.expr();

                    KeyValuePairNode::new(key_expr_node, value_expr_node, colon_node)
                }, ","))
            }

            let rcurly_node = parser.expect("}");

            AtomicExpressionNode::new_with_hashmap_expr(lcurly_node, rcurly_node, initials_node)
        }
        CoreToken::LPAREN                       => {
            let lparen_node = parser.expect("(");
            let expr_node = parser.expr();
            let curr_token = parser.curr_token();

            if curr_token.is_eq(",") {
                let comma_node = parser.expect(",");
                let remaining_tuple_exprs_node = parser.expect_symbol_separated_sequence(|parser: &mut JarvilParser| {
                    parser.expr()
                }, ",");
                let exprs_node = SymbolSeparatedSequenceNode::new_with_entities(expr_node, remaining_tuple_exprs_node, comma_node);
                let rparen_node = parser.expect(")");

                AtomicExpressionNode::new_with_tuple_expr(lparen_node, rparen_node, exprs_node)
            } else {
                let rparen_node = parser.expect(")");

                AtomicExpressionNode::new_with_parenthesised_expr(expr_node, lparen_node, rparen_node)
            }
        }
        _ => unreachable!("tokens not matching `starting_with_symbols` for atomic expression would already be eliminated")
    };
    atomic_expr_node
}
