use crate::context;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::Token;
use crate::errors::{ParseError,aggregate_errors};

pub fn expect_indent_spaces(indent_spaces: usize) -> Result<ParseSuccess, ParseError> {
    todo!()
}

pub fn block<F: FnMut() -> Result<ParseSuccess, ParseError>>(parser: &mut PackratParser, 
    mut f: F) -> Result<ParseSuccess, ParseError> {
    let indent_spaces = context::get_indent();
    // TODO - add a new scope table here
    let curr_indent_level = parser.get_indent_level();  // if block parsing fails, reset indent level to this value
    parser.expect("\n")?;
    // do below thing on loop - zero of more times
    let curr_lookahead = parser.get_lookahead();
    let response = PackratParser::expect_zero_or_more(|| {
        expect_indent_spaces(indent_spaces)?;  // check here whether block got over! by comparing the spaces found and expected
        let response = f()?;
        Ok(response)
    }, curr_lookahead);
    parser.reset_lookahead(response.lookahead);
    // TODO - reser the parent scope table here
    Ok(response)
}