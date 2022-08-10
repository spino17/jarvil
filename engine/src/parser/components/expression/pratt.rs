// This is alternate (and better) implementation for expression parser called Pratt Parser.
// Many famous production-grade parsers like microsoft's `tolerant-php-parser`, `Golang` and `JSLint` uses this technique (among many others).
// See following for more information:
// 1. `http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/`
// 2. `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`

use crate::{parser::parser::PackratParser, ast::ast::{ExpressionNode}};
use super::core::{is_expression_starting_with, EXPRESSION_EXPECTED_STARTING_SYMBOLS};
use crate::ast::ast::ErrornousNode;
use std::rc::Rc;

// all the unary operators are right assosiative and all the binary operators are left assosiative.
// below is the operator precedence in jarvil (lower to higher). This may be quite resembling with Python programming language.
// "or"
// "and"
// ">", ">=", "<", "<=", "==", "!="
// "-", "+"
// "/", "*"
// +, -, not, ()

pub fn expr(parser: &mut PackratParser) -> ExpressionNode {
    // check is_starting_with_expr here
    // 1. parser prefix first -> (+|-|not) prefix => prefix checks +, -, not, INT, FLOAT, BOOL, LITERAL, ATOM (ID), (
    //    if +, - or not then recursively call prefix, else return from prefix with appropiate node with unary operator
    // 2. then move to parser infix expr. Infix requires left node, operator and right node. 
    //    Loop until precedence of the current token is greater than precedence of the current expr being parsed (`precedence` arg)
    //    inside loop collect attach left, operator and right expression (with precedence of this operator) and update left
    //    if precedence of current token is less than or equal to precedence of the expression then stop and return from the expr.
    let token = &parser.curr_token();
    if !is_expression_starting_with(token) {
        parser.log_missing_token_error_for_multiple_expected_symbols(
            &EXPRESSION_EXPECTED_STARTING_SYMBOLS,
            token,
        );
        return ExpressionNode::new_with_missing_tokens(
            &Rc::new(EXPRESSION_EXPECTED_STARTING_SYMBOLS.to_vec()),
            token,
            parser.curr_lookahead(),
        );
    }
    parser.pratt_expr(0)
}

pub fn pratt_expr(parser: &mut PackratParser, precedence: u8) -> ExpressionNode {
    let prefix = parser.unary_expr();
    let mut left_expr: ExpressionNode = ExpressionNode::new_with_unary(&prefix);
    loop {
        let operator_precedence = parser.curr_token_precedence();
        if precedence < operator_precedence {
            break;
        }
        let operator_node = parser.expect_binary_operator();
        let right_expr = parser.pratt_expr(operator_precedence);
        // TODO - also if operator is comparison, collect nodes in array to make sense of a < b >= c as a < b and b >= c
        if operator_precedence <= 2 {
            left_expr = ExpressionNode::new_with_logical(&operator_node, &left_expr, &right_expr);
        } else {
            left_expr = ExpressionNode::new_with_binary(&operator_node, &left_expr, &right_expr);
        }
    }
    left_expr
}