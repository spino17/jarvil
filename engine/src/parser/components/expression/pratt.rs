// This is alternate (and better) top-down approach for parsing expression called Pratt Parser.
// Many famous production-grade parsers like microsoft's `tolerant-php-parser`, `Golang` and Douglas Crockford’s `JSLint` uses this technique
// (among many others).
// See following for more information:
// 1. `http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/`
// 2. `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`
// NOTE: By default jarvil uses this implementation

use crate::{
    ast::ast::{ExpressionNode, TokenNode},
    parser::parser::PackratParser,
};

pub fn pratt_expr(parser: &mut PackratParser, precedence: u8) -> ExpressionNode {
    let prefix = parser.unary_expr();
    let mut left_expr: ExpressionNode = ExpressionNode::new_with_unary(&prefix);
    loop {
        let (operator_precedence, operator_str) = parser.curr_token_precedence_and_name();
        if precedence >= operator_precedence {
            // equality gives left-assosiativity for equal precedence operators
            break;
        }
        let operator_node = parser.expect(operator_str);
        left_expr = parser.infix(&left_expr, &operator_node, operator_precedence);
    }
    left_expr
}

pub fn infix(
    parser: &mut PackratParser,
    left_expr: &ExpressionNode,
    operator_node: &TokenNode,
    operator_precedence: u8,
) -> ExpressionNode {
    if is_comparison(operator_precedence) {
        // precedence of comparison operators - TODO - write tests for this
        parser.infix_comparison_expr(left_expr, operator_node, operator_precedence)
    } else {
        parser.infix_binary_expr(left_expr, operator_node, operator_precedence)
    }
}

pub fn infix_comparison_expr(
    parser: &mut PackratParser,
    left_expr: &ExpressionNode,
    operator_node: &TokenNode,
    operator_precedence: u8,
) -> ExpressionNode {
    let mut operands: Vec<ExpressionNode> = vec![left_expr.clone()];
    let mut operators: Vec<TokenNode> = vec![operator_node.clone()];
    let right_expr = parser.pratt_expr(operator_precedence);
    operands.push(right_expr);
    loop {
        let (operator_precedence, operator_str) = parser.curr_token_precedence_and_name();
        if !is_comparison(operator_precedence) {
            break;
        }
        let operator_node = parser.expect(operator_str);
        let right_expr = parser.pratt_expr(operator_precedence);
        operands.push(right_expr);
        operators.push(operator_node);
    }
    if operators.len() == 1 {
        return ExpressionNode::new_with_binary(&operators[0], &operands[0], &operands[1]);
    }
    ExpressionNode::new_with_comparison(operands, operators)
}

pub fn infix_binary_expr(
    parser: &mut PackratParser,
    left_expr: &ExpressionNode,
    operator_node: &TokenNode,
    operator_precedence: u8,
) -> ExpressionNode {
    let right_expr = parser.pratt_expr(operator_precedence);
    ExpressionNode::new_with_binary(&operator_node, &left_expr, &right_expr)
}

pub fn is_comparison(precedence: u8) -> bool {
    if precedence == 3 {
        true
    } else {
        false
    }
}
