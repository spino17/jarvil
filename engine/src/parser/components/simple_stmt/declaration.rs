use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError, SyntaxError};
use crate::parser::packrat::ParseSuccess;
use crate::lexer::token::CoreToken;
use std::rc::Rc;

pub fn r_assign(parser: &mut PackratParser) -> Result<(ParseSuccess, Rc<String>), ParseError> {
    todo!()
}

pub fn decl(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    /*
    let (_, line_number, data_type, token_value) = parser.param_decl()?;
    let rule_index;
    if data_type.0.as_ref().eq("int") {
        rule_index = 0;
    } else if data_type.0.as_ref().eq("float") {
        rule_index = 1
    } else if data_type.0.as_ref().eq("bool") {
        rule_index = 2;
    } else if data_type.0.as_ref().eq("string") {
        rule_index = 3;
    } else {
        // TODO - if user-defined type then choose rule_index = 4 (new type)
        // unimplemented!("yet to be implemented for user-defined types")
        parser.set_identifier_to_scope(&token_value.0, &data_type.0, true);
        return Ok(ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: None,
        })
    }
    let curr_lookahead = parser.get_lookahead();
    let (is_matched, response, err) = 
    PackratParser::expect_optionally(|| {
        let (_, _) = parser.expect("=")?;
        let response = parser.r_asssign(rule_index, line_number)?;
        Ok(response)
    }, ParseSuccess{
        lookahead: curr_lookahead,
        possible_err: None,
    });
    parser.reset_lookahead(response.lookahead);
    parser.set_identifier_to_scope(&token_value.0, &data_type.0, is_matched);
    // semantic check -> type-checking
    if is_matched {
        Ok(ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: None,
        })
    } else {
        Ok(ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: err,
        })
    }
     */
    parser.expect("let")?;
    let (_, _, token_value) = parser.expect_any_id()?;
    parser.expect("=")?;
    let (response, data_type) = parser.r_assign()?;
    parser.set_identifier_to_scope(&token_value.0, &data_type, true);
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
    Ok(response)
}