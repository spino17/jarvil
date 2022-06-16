use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError, SemanticError};
use crate::parser::components::simple_stmt::helper::r_asssign_alternatives;

pub fn assign(parser: &mut PackratParser) -> Result<usize, ParseError> {
    let (_, _, symbol_data) = parser.expect_id_and_get_data()?;
    let (_, line_number) = parser.expect("=")?;
    let (_, rule_index, has_float) = r_asssign_alternatives(parser)?;
    let _ = match rule_index {
        0 => {
            if has_float {
                if !symbol_data.type_eq("float") {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                        parser.get_lookahead(), "mismatched types\n right side of the declaration is a float")))
                } else {
                    ()
                }
            } else {
                if !symbol_data.type_eq("int") {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                        parser.get_lookahead(), "mismatched types\n right side of the declaration is an int")))
                } else {
                    ()
                }
            }
        },
        1 => {
            if !symbol_data.type_eq("bool") {
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                    parser.get_lookahead(), "mismatched types\n right side of the declaration is a bool")))
            } else {
                ()
            }
        },
        2 => {
            if !symbol_data.type_eq("string") {
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                    parser.get_lookahead(), "mismatched types\n right side of the declaration is a string")))
            } else {
                ()
            }
        },
        _ => unreachable!("rule index can only be 0, 1 and 2 as there are three alternatives to declaration")
    };
    Ok(parser.get_lookahead())
}