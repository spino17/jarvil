use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError, SyntaxError, SemanticError};
use crate::parser::packrat::ParseSuccess;
use crate::lexer::token::CoreToken;

pub fn variable_decl(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("let")?;
    let index = parser.get_index();
    let (_, _, identifier_name) = parser.expect_any_id()?;
    parser.expect("=")?;
    let (response, data_type, _) = parser.r_assign()?;

    // semantic check - identifier name should not be same as any type in the current scope
    if let Some(identifier_category) = parser.set_identifier_to_scope(&identifier_name, &data_type, true) {
        let line_number = parser.get_curr_line_number();
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            format!(
            "'{}' already declared in the current scope as '{}'", identifier_name, identifier_category
            )
        )));
    }
    Ok(response)
}

pub fn variable_decls(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let response = parser.decl()?;
    match parser.get_curr_core_token() {
        CoreToken::COMMA => {
            parser.expect(",")?;
            parser.variable_decls()
        },
        CoreToken::NEWLINE => {
            return Ok(response)
        },
        _ => {
            let line_number = parser.get_curr_line_number();
            let index = parser.get_index();
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                parser.get_code_line(line_number, index),
                format!(
                "expected ',' or 'newline', got '{}'",
                PackratParser::parse_for_err_message(parser.get_next_token_name().to_string())
                )
            )))
        }
    }
}