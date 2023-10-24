// This is alternate (and better) top-down approach for parsing expression called Pratt Parser.
// Many famous production-grade parsers like microsoft's `tolerant-php-parser`, `Golang` and Douglas Crockford’s `JSLint` uses this technique
// (among many others).
// See following for more information:
// 1. `http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/`
// 2. `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`
// NOTE: By default Jarvil uses this implementation

use crate::{
    ast::ast::{ExpressionNode, TokenNode},
    parser::parser::JarvilParser,
};

pub fn pratt_expr(parser: &mut JarvilParser, precedence: u8) -> ExpressionNode {
    let prefix_node = parser.unary_expr();
    let mut left_expr_node: ExpressionNode = ExpressionNode::new_with_unary(prefix_node);
    loop {
        let (operator_precedence, operator_str) = parser.curr_token_precedence_and_name();
        if precedence >= operator_precedence {
            // equality gives left-assosiativity for equal precedence operators
            break;
        }
        let operator_node = parser.expect(operator_str);
        left_expr_node = parser.infix(&left_expr_node, &operator_node, operator_precedence);
    }
    left_expr_node
}

pub fn infix(
    parser: &mut JarvilParser,
    left_expr_node: &ExpressionNode,
    operator_node: &TokenNode,
    operator_precedence: u8,
) -> ExpressionNode {
    if is_comparison(operator_precedence) {
        parser.infix_comparison_expr(left_expr_node, operator_node, operator_precedence)
    } else {
        parser.infix_binary_expr(left_expr_node, operator_node, operator_precedence)
    }
}

pub fn infix_comparison_expr(
    parser: &mut JarvilParser,
    left_expr_node: &ExpressionNode,
    operator_node: &TokenNode,
    operator_precedence: u8,
) -> ExpressionNode {
    let mut operands: Vec<ExpressionNode> = vec![left_expr_node.clone()];
    let mut operators: Vec<TokenNode> = vec![operator_node.clone()];
    let right_expr_node = parser.pratt_expr(operator_precedence);
    operands.push(right_expr_node);
    loop {
        let (operator_precedence, operator_str) = parser.curr_token_precedence_and_name();
        if !is_comparison(operator_precedence) {
            break;
        }
        let operator_node = parser.expect(operator_str);
        let right_expr_node = parser.pratt_expr(operator_precedence);
        operands.push(right_expr_node);
        operators.push(operator_node);
    }
    if operators.len() == 1 {
        return ExpressionNode::new_with_binary(
            operators[0].clone(),
            operands[0].clone(),
            operands[1].clone(),
        );
    }
    ExpressionNode::new_with_comparison(operands, operators)
}

pub fn infix_binary_expr(
    parser: &mut JarvilParser,
    left_expr_node: &ExpressionNode,
    operator_node: &TokenNode,
    operator_precedence: u8,
) -> ExpressionNode {
    let right_expr_node = parser.pratt_expr(operator_precedence);
    ExpressionNode::new_with_binary(
        operator_node.clone(),
        left_expr_node.clone(),
        right_expr_node,
    )
}

pub fn is_comparison(precedence: u8) -> bool {
    precedence == 3
}
