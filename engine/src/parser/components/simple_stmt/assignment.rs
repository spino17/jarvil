use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};
use crate::types::core::{Type, TypeCheck};

pub fn assign(parser: &mut PackratParser, data_type: Option<Type>, is_assignable: bool, index: usize) -> Result<ParseSuccess, ParseError> {
    parser.expect("=")?;
    if !is_assignable {

        // semantic check - value should be assignable, for example - any output from a function call is not assignable
        let line_number = parser.get_curr_line_number();
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            String::from("value is not assignable")))
        )
    }

    // semantic check - types on both the side of assignment operator should match
    if let Some(l_data_type) = data_type {
        let (response, r_data_type, index) = parser.r_assign()?;
        if !l_data_type.is_eq(&r_data_type) {
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
            String::from("'None' value is not assignable")))
        )
    }
}