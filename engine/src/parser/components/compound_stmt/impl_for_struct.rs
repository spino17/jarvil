use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};

pub fn impl_for_struct(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("impl")?;
    // TODO - optionally expect an interface name
    parser.expect("for")?;
    let index = parser.get_index();
    let (_, line_number, struct_name, symbol_data) = parser.expect_any_id_in_scope()?;
    if let Some(_) = symbol_data.get_user_defined_struct_type_data() {
        parser.expect(":")?;
        let response = parser.impl_for_struct_block(&struct_name)?;
        Ok(response)
    } else {
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            format!("expected struct type, got {} '{}'",
            symbol_data.get_category_of_identifier(), struct_name.clone())))
        )
    }
}