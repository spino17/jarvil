use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError};

pub fn assign(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let (_, _, symbol_data) = parser.expect_id_and_get_data()?;
    let (_, line_number) = parser.expect("=")?;
    let rule_index;
    if symbol_data.type_eq("int") {
        rule_index = 0;
    } else if symbol_data.type_eq("float") {
        rule_index = 1
    } else if symbol_data.type_eq("bool") {
        rule_index = 2;
    } else if symbol_data.type_eq("string") {
        rule_index = 3;
    } else {
        unimplemented!("yet to be implemented for user-defined types")
    }
    parser.r_asssign(rule_index, line_number)?;
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}