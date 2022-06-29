use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, aggregate_errors};
use std::rc::Rc;

pub fn param_decl(parser: &mut PackratParser) -> Result<(ParseSuccess, usize, Rc<String>, Rc<String>), ParseError> {
    let (_, _, data_type, _) = parser.expect_type()?;
    let (response, line_number, token_value) = parser.expect_any_id()?;
    Ok((response, line_number, data_type, token_value))
}

pub fn r_assign(parser: &mut PackratParser) -> Result<(ParseSuccess, Rc<String>, usize), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    match parser.param() {  // bexpr | expr | literal | atom
        Ok(response) => return Ok((response.0, response.1.0, response.1.1)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err)
        }
    }
    Err(aggregate_errors(errors_vec))
}