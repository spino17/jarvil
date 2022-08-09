// This is alternate (and better) implementation for expression parser called Pratt Parser.
// Many famous production-grade parsers like microsoft's `tolerant-php-parser`, `Golang` and `JSLint` uses this technique (among many others).
// See following for more information:
// 1. `http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/`
// 2. `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`

use crate::{parser::parser::PackratParser, ast::ast::ExpressionNode};

pub fn expression(parser: &mut PackratParser, precedence: u8) -> ExpressionNode {
    // check is_starting_with_expr here
    // 1. parser prefix first -> (+|-|not) prefix => prefix checks +, -, not, INT, FLOAT, BOOL, LITERAL, ATOM (ID), (
    //    if +, - or not then recursively call prefix, else return from prefix with appropiate node with unary operator
    // 2. then move to parser infix expr. Infix requires left node, operator and right node. 
    //    Loop until precedence of the current token is greater than precedence of the current expr being parsed (`precedence` arg)
    //    inside loop collect attach left, operator and right expression (with precedence of this operator) and update left
    //    if precedence of current token is less than or equal to precedence of the expression then stop and return from the expr.
    todo!()
}