use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};
use crate::parser::components::simple_stmt::helper::r_asssign_alternatives;

pub fn assign(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let (_, _, symbol_data) = parser.expect_id_and_get_data()?;
    let (_, line_number) = parser.expect("=")?;
    let (_, rule_index, has_float) = r_asssign_alternatives(parser)?;

    // semantic check -> type-checking
    let expected_data_type = match rule_index {
        0 => {
            if has_float {
                "float"
            } else {
                "int"
            }
        },
        1 => {
            "bool"
        },
        2 => {
            "string"
        },
        _ => unreachable!("rule index can only be 0, 1 and 2 as there are three alternatives to assignment")
    };
    if symbol_data.type_eq(expected_data_type) {
        symbol_data.set_init(true);
        Ok(ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: None,
        })
    } else {
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
            parser.get_lookahead(), 
            format!(
                "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                symbol_data.get_type(), expected_data_type)))
            )
    }
}