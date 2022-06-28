use crate::lexer::token;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};

pub fn assign(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    /*
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
     */
    let index = parser.get_index();
    let (_, data_type, is_assignable) = parser.atom()?;
    parser.expect("=")?;
    if !is_assignable {
        let line_number = parser.get_curr_line_number();
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            String::from("value is not assignable")))
        )
    }
    if let Some(l_data_type) = data_type {
        let (response, (r_data_type, index)) = parser.param()?;
        if !l_data_type.eq(&r_data_type) {
            let line_number = parser.get_curr_line_number();
            return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                parser.get_code_line(line_number, index),
                format!("mismatched types\n    left side of the assignment has type '{}', right side of the assignment has type '{}'",
                l_data_type, r_data_type)))
            )
        } else {
            Ok(response)
        }
    } else {
        let line_number = parser.get_curr_line_number();
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            String::from("value with type 'None' is not assignable")))
        )
    }
}