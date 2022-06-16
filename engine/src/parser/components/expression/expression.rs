use crate::parser::packrat::PackratParser;
use crate::errors::ParseError;

pub fn term(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    todo!()
}

pub fn additive(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    todo!()
}

pub fn expr(parser: &mut PackratParser) -> Result<(usize, bool), ParseError> {
    let (_, is_float_in_term) = parser.term()?;
    let (_, is_float_in_additive) = parser.additive()?;
    if is_float_in_term || is_float_in_additive {
        Ok((parser.get_lookahead(), true))
    } else {
        Ok((parser.get_lookahead(), false))
    }
}