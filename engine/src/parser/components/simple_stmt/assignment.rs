use crate::lexer::token;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError};

pub fn assign(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let (_, _, token_value, data_type, _) = parser.expect_id()?;
    let (_, line_number) = parser.expect("=")?;
    let rule_index;
    if data_type.to_string().eq("int") {
        rule_index = 0;
    } else if data_type.to_string().eq("float") {
        rule_index = 1
    } else if data_type.to_string().eq("bool") {
        rule_index = 2;
    } else if data_type.to_string().eq("string") {
        rule_index = 3;
    } else {
        unimplemented!("yet to be implemented for user-defined types")
    }
    parser.r_asssign(rule_index, line_number)?;
    parser.set_identifier_init_to_scope(&token_value);
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}