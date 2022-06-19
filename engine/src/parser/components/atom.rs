use crate::{parser::packrat::{PackratParser, ParseSuccess}, errors::ParseError};
use std::rc::Rc;

pub fn atom(parser: &mut PackratParser) -> Result<(ParseSuccess, Rc<String>, bool), ParseError> {
    todo!();
}