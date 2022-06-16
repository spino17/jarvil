use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};
use crate::parser::components::simple_stmt::helper::r_asssign_alternatives;

pub fn assign(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let (_, _, mut symbol_data) = parser.expect_id_and_get_data()?;
    let (_, line_number) = parser.expect("=")?;
    let (_, rule_index, has_float) = r_asssign_alternatives(parser)?;
    let _ = match rule_index {
        0 => {
            if has_float {
                if !symbol_data.type_eq("float") {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                        parser.get_lookahead(), 
                        format!(
                            "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                            symbol_data.get_type(), "float")))
                        )
                } else {
                    ()
                }
            } else {
                if !symbol_data.type_eq("int") {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                        parser.get_lookahead(), 
                        format!(
                            "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                        symbol_data.get_type(), "int")))
                    )
                } else {
                    ()
                }
            }
        },
        1 => {
            if !symbol_data.type_eq("bool") {
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                    parser.get_lookahead(), 
                    format!(
                        "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                        symbol_data.get_type(), "bool"))))
            } else {
                ()
            }
        },
        2 => {
            if !symbol_data.type_eq("string") {
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                    parser.get_lookahead(), 
                    format!(
                        "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                        symbol_data.get_type(), "string")))
                    )
            } else {
                ()
            }
        },
        _ => unreachable!("rule index can only be 0, 1 and 2 as there are three alternatives to assignment")
    };
    symbol_data.set_init(true);
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}