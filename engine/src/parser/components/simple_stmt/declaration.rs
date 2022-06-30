use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError, SyntaxError, SemanticError};
use crate::parser::packrat::ParseSuccess;
use crate::lexer::token::CoreToken;

pub fn decl(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("let")?;
    let index = parser.get_index();
    let (_, _, identifier_name) = parser.expect_any_id()?;
    parser.expect("=")?;
    let (response, data_type, _) = parser.r_assign()?;
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

pub fn decl_factor(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::COMMA => {
            parser.expect(",")?;
            parser.decls()
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => {
                    // FOLLOW(decls)
                    if parser.check_next_token("\n") {
                        return Ok(response)
                    } else {
                        let line_number = parser.get_curr_line_number();
                        let index = parser.get_index();
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_code_line(line_number, index),
                            format!(
                            "expected 'newline', got '{}'", PackratParser::parse_for_err_message(
                                parser.get_next_token_name().to_string())
                            )
                        ));
                        return Err(err);
                    }
                },
                Err(err) => {
                    unreachable!("parsing empty string never give error, got {:?}", err)
                }
            }
        }
    }
}

pub fn decls(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let response = parser.decl()?;
    if let Some(err) = response.possible_err {
        if !parser.check_next_token("\n") 
        && !parser.check_next_token(",") {
            return Err(err)
        }
    }
    let response = parser.decl_factor()?;
    // parser.expect("\n")?;
    Ok(response)
}