use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::ParseError;

pub fn struct_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("struct")?;
    let (_, line_number, token_value) = parser.expect_and_get_value("identifier")?;
    parser.expect(":")?;
    let (response, fields_vec) = parser.struct_block()?;
    // TODO - use above token value as key for symbol table
    // TODO - use fields_vec to add into meta data of symbol_data
    Ok(response)
}