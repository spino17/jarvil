use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SyntaxError, aggregate_errors};

pub fn compound_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    // TODO - add production rules for compound statements - see jarvil.gram
    Err(ParseError::SYNTAX_ERROR(SyntaxError::new(1000, 0, 
        String::from("This is just a demo error to test"))))
    // Ok(self.lookahead)
}