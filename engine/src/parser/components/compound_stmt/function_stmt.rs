use std::rc::Rc;
use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::errors::ParseError;
use crate::lexer::token::{CoreToken};
use crate::errors::{SyntaxError, SemanticError};
use crate::types::Type;

pub fn optparams(parser: &mut PackratParser) -> Result<(ParseSuccess, Vec<(Rc<String>, Type)>), ParseError> {
    let mut params: Vec<(Rc<String>, Type)> = vec![];
    let (response, _, data_type, param_name) = parser.param_decl()?;
    params.push((param_name.clone(), Type(data_type.0.clone())));
    let (response, mut remaining_params) = match parser.get_curr_core_token() {
        CoreToken::COMMA => {
            parser.expect(",")?;
            parser.optparams()?
        },
        CoreToken::RPAREN => {
            return Ok((response, params))
        },
        _ => {
            let line_number = parser.get_curr_line_number();
            let index = parser.get_index();
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                parser.get_code_line(line_number, index),
                format!(
                "expected ',' or ')', got '{}'", 
                PackratParser::parse_for_err_message(parser.get_next_token_name().to_string())
                )
            )))
        }
    };
    params.append(&mut remaining_params);
    Ok((response, params))
}

pub fn function_input_output(parser: &mut PackratParser)
-> Result<(ParseSuccess, Vec<(Rc<String>, Type)>, bool, Option<Type>, Option<ParseError>), ParseError> {
    // TODO - check for any generic symbols inside '<' '>'
    parser.expect("(")?;
    let mut params = vec![];
    if !parser.check_next_token(")") {
        let (_, opt_params) = parser.optparams()?;
        params = opt_params;
        println!("{:?}", params);
    }
    parser.expect(")")?;
    let curr_lookahead = parser.get_lookahead();
    let (is_matched, (response, return_type), err) = 
    PackratParser::expect_optionally(|| {
        let (_, _) = parser.expect("->")?;
        let (response, _, data_type) = parser.expect_type()?;
        Ok((response, Some(Type(data_type.0.clone()))))
    }, (ParseSuccess{
        lookahead: curr_lookahead,
        possible_err: None,
    }, None));
    parser.reset_lookahead(response.lookahead);
    Ok((response, params, is_matched, return_type, err))
}

pub fn function_declaration(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("def")?;
    match parser.get_curr_core_token() {
        CoreToken::IDENTIFIER(_) => {
            let index = parser.get_index();
            let (_, _, function_name) = parser.expect_any_id()?;
            let (_, params, 
                _, return_type, err) = parser.function_input_output()?;
            match parser.expect(":") {
                Ok((_, _)) => {},
                Err(error) => {
                    if let Some(possible_err) = err {
                        return Err(possible_err)
                    } else {
                        return Err(error)
                    }
                }
            }
            let response = parser.block(Some(&params))?;

            // semantic check - function name should not be same as any type in the current scope
            if let Some(identifier_category) = parser.set_function_to_scope(&function_name, &Rc::new(params), &Rc::new(return_type)) {
                let line_number = parser.get_curr_line_number();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!(
                    "'{}' already declared in the current scope as '{}'", function_name, identifier_category
                    )
                )));
            }
            Ok(response)
        },
        _ => {
            let line_number = parser.get_curr_line_number();
            let index = parser.get_index();
            Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
            parser.get_code_line(line_number, index),
            format!("expected '(' or identifier, got '{}'",
            PackratParser::parse_for_err_message(parser.get_curr_token_name().to_string())))))
        }
    }
}