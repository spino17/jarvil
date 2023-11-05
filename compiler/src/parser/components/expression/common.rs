use crate::ast::ast::{ExpressionNode, SymbolSeparatedSequenceNode};
use crate::parser::parser::JarvilParser;

pub fn params(parser: &mut JarvilParser) -> SymbolSeparatedSequenceNode<ExpressionNode> {
    parser.expect_symbol_separated_sequence(|parser: &mut JarvilParser| parser.expr(), ",")
}
