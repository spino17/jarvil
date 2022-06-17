use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError};
use crate::parser::packrat::ParseSuccess;

pub fn decl(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let (_, _, data_type) = parser.expect_type()?;
    let (_, line_number, token_value) = parser.expect_and_get_value("identifier")?;
    let rule_index;
    if data_type.as_ref().eq("int") {
        rule_index = 0;
    } else if data_type.as_ref().eq("float") {
        rule_index = 1
    } else if data_type.as_ref().eq("bool") {
        rule_index = 2;
    } else if data_type.as_ref().eq("string") {
        rule_index = 3;
    } else {
        unimplemented!("yet to be implemented for user-defined types")
    }
    let curr_lookahead = parser.get_lookahead();
    let (is_matched, response, err) = 
    PackratParser::expect_optionally(|| {
        let (_, _) = parser.expect("=")?;
        let response = parser.r_asssign_alternatives(rule_index, line_number)?;
        Ok(response)
    }, ParseSuccess{
        lookahead: curr_lookahead,
        possible_err: None,
    });
    parser.reset_lookahead(response.lookahead);
    parser.set_scope(&token_value, &data_type, is_matched);

    // semantic check -> type-checking
    if is_matched {
        Ok(ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: None,
        })
    } else {
        Ok(ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: err,
        })
    }
}