use crate::ast::ast::StatementNode;
use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::errors::{SyntaxError};

pub fn stmt(parser: &mut PackratParser) -> Result<(ParseSuccess, StatementNode), SyntaxError> {
    todo!()
}