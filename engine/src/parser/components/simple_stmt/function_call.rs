use crate::{parser::packrat::{PackratParser, ParseSuccess}};
use crate::errors::ParseError;
use std::rc::Rc;

pub fn function_call(parser: &mut PackratParser, 
    is_method: bool) -> Result<(ParseSuccess, Rc<String>, Vec<Rc<String>>), ParseError> {
    // expect callable - identifier is a function or has lambda type
    // take out the params, return type structure
    // using datatypes of arguments start using production rules to determine expr, bexpr, literal, atom
    // each arguments should match the datatype
    todo!()
}