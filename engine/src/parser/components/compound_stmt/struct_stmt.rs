use std::rc::Rc;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::ParseError;

pub fn struct_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("type")?;
    let (_, _, token_value) = parser.expect_any_id()?;
    parser.expect(":")?;
    let (response, fields_map) = parser.struct_block()?;
    parser.set_user_defined_type_to_scope(&token_value, &Rc::new(fields_map));
    Ok(response)
}