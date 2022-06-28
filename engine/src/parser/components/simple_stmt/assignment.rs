use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};

pub fn assign(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
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
        let (response, r_data_type, index) = parser.r_assign()?;
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