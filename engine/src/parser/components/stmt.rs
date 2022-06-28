use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError};
use crate::lexer::token::CoreToken;

pub fn try_compound_stmt(parser: &mut PackratParser) -> Result<(Option<ParseSuccess>, bool), ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::TYPE_KEYWORD => {
            match parser.type_decl_stmt() {
                Ok(response) => return Ok((Some(response), true)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        CoreToken::DEF => {
            match parser.function_declaration() {
                Ok(response) => return Ok((Some(response), true)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        _ => {
            return Ok((None, false))
        }
    }
}

pub fn stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let (response, is_success) = try_compound_stmt(parser)?;
    if is_success {
        if let Some(response) = response {
            return Ok(response)
        } else {
            unreachable!("a successfull parse of compound statement always give some response")
        }
    } else {
        parser.simple_stmt()
    }
}