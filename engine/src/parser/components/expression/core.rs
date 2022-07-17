use crate::{lexer::token::{Token, CoreToken}, constants::common::{INTEGER, FLOATING_POINT_NUMBER, STRING_LITERAL, 
IDENTIFIER}, parser::parser::PackratParser, ast::ast::{ExpressionNode, AtomicExpressionNode, UnaryExpressionNode, 
    UnaryOperatorKind}};
use std::rc::Rc;

// all the unary operators are right assosiative and all the binary operators are left assosiative. 
// below is the operator precedence in jarvil (lower to higher). This may be quite resembling with Python programming language.
// "and", "or"
// ">", ">=", "<", "<=", "==", "!="
// "-", "+"
// "/", "*"
// +, -, not, ()

pub fn is_expression_starting_with(token: &Token) -> bool {
    match token.core_token {
        _  => is_unary_expression_starting_with(token),
    }
}

pub const EXPRESSION_EXPECTED_STARTING_SYMBOLS: [&'static str; 10] = UNARY_EXPRESSION_STARTING_SYMBOLS;

pub fn expr(parser: &mut PackratParser) -> ExpressionNode {
    let token = &parser.curr_token();
    if !is_expression_starting_with(token) {
        parser.log_missing_token_error_for_multiple_expected_symbols(
            &EXPRESSION_EXPECTED_STARTING_SYMBOLS, token
        );
        return ExpressionNode::new_with_missing_tokens(
            &Rc::new(EXPRESSION_EXPECTED_STARTING_SYMBOLS.to_vec()),
            token,
            parser.curr_lookahead(),
        )
    }
    parser.logical()
}

pub fn logical(parser: &mut PackratParser) -> ExpressionNode {
    let mut leading_comparison_expr_node = parser.comparison();
    while let Some(node) = parser.expects(&["and", "or"], false).is_ok() {
        let operator_node = node;
        let trailing_comparison_expr_node = parser.comparison();
        leading_comparison_expr_node = ExpressionNode::new_with_binary(
            &operator_node, &leading_comparison_expr_node, &trailing_comparison_expr_node
        );
    }
    leading_comparison_expr_node
}

pub fn comparison(parser: &mut PackratParser) -> ExpressionNode {
    let mut leading_term_expr_node = parser.term();
    while let Some(node) = parser.expects(&[">", ">=", "<", "<=", "==", "!="], false).is_ok() {
        let operator_node = node;
        let trailing_term_expr_node = parser.term();
        leading_term_expr_node = ExpressionNode::new_with_binary(
            &operator_node, &leading_term_expr_node, &trailing_term_expr_node
        );
    }
    leading_term_expr_node
}

pub fn term(parser: &mut PackratParser) -> ExpressionNode {
    let mut leading_factor_expr_node = parser.factor();
    while let Some(node) = parser.expects(&["-", "+"], false).is_ok() {
        let operator_node = node;
        let trailing_factor_expr_node = parser.factor();
        leading_factor_expr_node = ExpressionNode::new_with_binary(
            &operator_node, &leading_factor_expr_node, &trailing_factor_expr_node
        );
    }
    leading_factor_expr_node
}

pub fn factor(parser: &mut PackratParser) -> ExpressionNode {
    let leading_unary_expr_node = parser.unary_expr();
    let mut left_expr: ExpressionNode = ExpressionNode::new_with_unary(&leading_unary_expr_node);
    while let Some(node) = parser.expects(&["/", "*"], false).is_ok() {
        let operator_node = node;
        let trailing_unary_expr_node = parser.unary_expr();
        let right_expr = ExpressionNode::new_with_unary(&trailing_unary_expr_node);
        left_expr = ExpressionNode::new_with_binary(&operator_node, &left_expr, &right_expr);
    }
    left_expr
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

pub fn unary_expr(parser: &mut PackratParser) -> UnaryExpressionNode {
    let token = &parser.curr_token();
    if !is_unary_expression_starting_with(token) {
        parser.log_missing_token_error_for_multiple_expected_symbols(&UNARY_EXPRESSION_STARTING_SYMBOLS, token);
        return UnaryExpressionNode::new_with_missing_tokens(
            &Rc::new(UNARY_EXPRESSION_STARTING_SYMBOLS.to_vec()),
            token,
            parser.curr_lookahead(),
        )
    }
    let unary_expr_node = match token.core_token {
        CoreToken::PLUS => {
            let plus_node = parser.expect("+", false);
            let unary_expr_node = parser.unary_expr();
            UnaryExpressionNode::new_with_unary(UnaryOperatorKind::PLUS, &unary_expr_node)
        },
        CoreToken::DASH => {
            let dash_node = parser.expect("-", false);
            let unary_expr_node = parser.unary_expr();
            UnaryExpressionNode::new_with_unary(UnaryOperatorKind::MINUS, &unary_expr_node)
        },
        CoreToken::NOT  => {
            let not_node = parser.expect("not", false);
            let unary_expr_node = parser.unary_expr();
            UnaryExpressionNode::new_with_unary(UnaryOperatorKind::NOT, &unary_expr_node)
        },
        _ => {
            let atomic_expr_node = parser.atomic_expr();
            UnaryExpressionNode::new_with_atomic(&atomic_expr_node)
        }
    };
    unary_expr_node
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

pub fn atomic_expr(parser: &mut PackratParser) -> AtomicExpressionNode {
    let token = &parser.curr_token();
    if !is_atomic_expression_starting_with(token) {
        parser.log_missing_token_error_for_multiple_expected_symbols(&ATOMIC_EXPRESSION_STARTING_SYMBOLS, token);
        return AtomicExpressionNode::new_with_missing_tokens(
            &Rc::new(ATOMIC_EXPRESSION_STARTING_SYMBOLS.to_vec()),
            token,
            parser.curr_lookahead(),
        )
    }
    let atomic_expr_node = match token.core_token {
        CoreToken::TRUE                         => {
            let true_node = parser.expect("true", false);
            AtomicExpressionNode::new_with_true()
        }
        CoreToken::FALSE                        => {
            let false_node = parser.expect("false", false);
            AtomicExpressionNode::new_with_false()
        }
        CoreToken::INTEGER(_)                   => {
            let integer_node = parser.expect(INTEGER, false);
            AtomicExpressionNode::new_with_integer(&integer_node)
        }
        CoreToken::FLOATING_POINT_NUMBER(_)     => {
            let floating_point_number_node = parser.expect(FLOATING_POINT_NUMBER, false);
            AtomicExpressionNode::new_with_floating_point_number(&floating_point_number_node)
        }
        CoreToken::LITERAL(_)                   => {
            let literal_node = parser.expect(STRING_LITERAL, false);
            AtomicExpressionNode::new_with_literal(&literal_node)
        }
        CoreToken::IDENTIFIER(_)                => {
            let atom = parser.atom();
            AtomicExpressionNode::new_with_atom(&atom)
        }
        CoreToken::LPAREN                       => {
            let lparen_node = parser.expect("(", false);
            let expr_node = parser.expr();
            let rparen_node = parser.expect(")", false);
            AtomicExpressionNode::new_with_parenthesised_expr(&expr_node)
        }
        _ => unreachable!("tokens not matching `starting_with_symbols` for atomic expression would already be eliminated")
    };
    atomic_expr_node
}