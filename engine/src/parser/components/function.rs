use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::ParseError;
use std::rc::Rc;

pub fn params(parser: &mut PackratParser) -> Result<(ParseSuccess, Vec<Rc<String>>), ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::RPAREN => {
            let (response, _) = parser.expect(")")?;
            Ok((response, vec![]))
        },
        _ => {
            todo!()
        }
    }
}