use crate::{parser::packrat::{PackratParser, ParseSuccess}};
use crate::errors::ParseError;

pub fn function_call(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    // expect callable - identifier is a function or has lambda type
    // take out the params, return type structure
    // using datatypes of arguments start using production rules to determine expr, bexpr, literal, atom
    // each arguments should match the datatype
    todo!()
}