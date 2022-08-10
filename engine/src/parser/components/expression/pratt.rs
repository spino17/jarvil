// This is alternate (and better) implementation for expression parser called Pratt Parser.
// Many famous production-grade parsers like microsoft's `tolerant-php-parser`, `Golang` and `JSLint` uses this technique (among many others).
// See following for more information:
// 1. `http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/`
// 2. `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`

use crate::{parser::parser::PackratParser, ast::ast::{ExpressionNode}};

pub fn pratt_expr(parser: &mut PackratParser, precedence: u8) -> ExpressionNode {
    let prefix = parser.unary_expr();
    let mut left_expr: ExpressionNode = ExpressionNode::new_with_unary(&prefix);
    loop {
        let (operator_precedence, operator_str) = parser.curr_token_precedence();
        if precedence < operator_precedence {
            break;
        }
        let operator_node = parser.expect(operator_str);
        let right_expr = parser.pratt_expr(operator_precedence);
        // TODO - also if operator is comparison, collect nodes in array to make sense of a < b >= c as a < b and b >= c
        left_expr = ExpressionNode::new_with_binary(&operator_node, &left_expr, &right_expr);
    }
    left_expr
}