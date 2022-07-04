use crate::{parser::packrat::{PackratParser, ParseSuccess}, errors::{ParseError, SyntaxError, SemanticError}};
use crate::lexer::token::CoreToken;
use crate::types::Type;

pub fn atom_index_access(parser: &mut PackratParser, 
    data_type: Option<Type>, mut is_assignable: bool, 
    mut is_function_call: bool) -> Result<(ParseSuccess, Option<Type>, bool, bool), ParseError> {
    let index = parser.get_index();
    parser.expect("[")?;
    let (_, (index_data_type, index)) = parser.param()?;
    let (response, _) = parser.expect("]")?;
    let data_type = match data_type {
        Some(data_type) => data_type,
        None => {
            let line_number = parser.get_curr_line_number();
            return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                parser.get_code_line(line_number, index),
               format!("'None' value is not indexable with key of type '{}'", index_data_type))
            ))
        }
    };
    is_function_call = false;
    Ok((response, Some(index_data_type), is_assignable, is_function_call))
}

pub fn atom_propertry_or_method_access(parser: &mut PackratParser, 
    data_type: Option<Type>, mut is_assignable: bool, 
    mut is_function_call: bool) -> Result<(ParseSuccess, Option<Type>, bool, bool), ParseError> {
    parser.expect(".")?;
    let index = parser.get_index();
    let (response, _, method_or_propertry_name) = parser.expect_any_id()?;
    let data_type = match data_type {
        Some(data_type) => data_type,
        None => {
            let line_number = parser.get_curr_line_number();
            return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                parser.get_code_line(line_number, index),
               format!("'None' value has no propertry or method named '{}'", method_or_propertry_name))
            ))
        }
    };
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            parser.expect("(")?;
            let (expected_params, return_type)
            = if let Some(function_data) 
            = parser.has_method_with_name(&data_type, &method_or_propertry_name) {
                let return_type = function_data.return_type;
                let expected_params = function_data.params;
                (expected_params, return_type)
            } else {
                let line_number = parser.get_curr_line_number();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!("type '{}' has no method named '{}'", data_type, method_or_propertry_name))
                ))
            };
            parser.params(&expected_params, 0)?;
            let (response, _) = parser.expect(")")?;
            let return_type = match return_type.as_ref() {
                Some(value) => {
                    Some(Type(value.0.clone()))
                },
                None => None,
            };
            is_assignable = false;
            is_function_call = true;
            Ok((response, return_type, is_assignable, is_function_call))
        },
        _ => {
            let field_data_type
            = if let Some(field_data_type) = parser.has_field_with_name(&data_type, &method_or_propertry_name) {
                Type(field_data_type.0.clone())
            } else {
                let line_number = parser.get_curr_line_number();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                   format!("type '{}' has no propertry named '{}'", data_type, method_or_propertry_name))
                ))
            };
            is_function_call = false;
            Ok((response, Some(field_data_type), is_assignable, is_function_call))
        }
    }
}

pub fn atom_factor(parser: &mut PackratParser, 
    data_type: Option<Type>, is_assignable: bool, 
    is_function_call: bool) -> Result<(ParseSuccess, Option<Type>, bool, bool), ParseError> {
    let (_, return_data_type, return_is_assignable, return_is_function_call)
    = match parser.get_curr_core_token() {
        CoreToken::LSQUARE => {
            parser.atom_index_access(data_type, is_assignable, is_function_call)?
        },
        CoreToken::DOT => {
            parser.atom_propertry_or_method_access(data_type, is_assignable, is_function_call)?
        },
        _ => {
            // TODO - check FOLLOW(atom) before returning
            return Ok((ParseSuccess{
                lookahead: parser.get_lookahead(),
                possible_err: None,
            }, data_type, is_assignable, is_function_call))
        }
    };
    let (response, final_data_type, final_is_assignable, final_is_function_call)
    = parser.atom_factor(return_data_type, return_is_assignable, return_is_function_call)?;
    Ok((response, final_data_type, final_is_assignable, final_is_function_call))
}

pub fn atom(parser: &mut PackratParser) -> Result<(ParseSuccess, Option<Type>, bool, bool), ParseError> {
    let index = parser.get_index();
    let (_, line_number, 
        token_value, symbol_data) = parser.expect_any_id_in_scope()?;
    let mut is_assignable = true;
    let mut is_function_call = false;
    match parser.get_curr_core_token() {
        CoreToken::DOUBLE_COLON => {
            if let Some(struct_data) = symbol_data.get_user_defined_struct_type_data() {
                parser.expect("::")?;
                let index = parser.get_index();
                let (_, _, class_method_name) = parser.expect_any_id()?;
                parser.expect("(")?;
                let (expected_params, return_type)
                = if let Some(function_data) = parser.has_class_method_with_name(&class_method_name, &struct_data.name) {
                    (function_data.params, function_data.return_type)
                } else {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, index),
                        format!("type '{}' has no classmethod named '{}'", struct_data.name, class_method_name))
                    ))
                };
                parser.params(&expected_params, 0)?;
                parser.expect(")")?;
                let data_type: Option<Type>;
                match return_type.as_ref() {
                    Some(value) => {
                        data_type = Some(Type(value.0.clone()));
                    },
                    None => {
                        data_type = None;
                    }
                }
                is_assignable = false;
                is_function_call = true;
                parser.atom_factor(data_type, is_assignable, is_function_call)
            } else {
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!("expected struct type, got {} '{}'", 
                    symbol_data.get_category_of_identifier(), token_value.clone())))
                )
            }
        },
        CoreToken::LPAREN => {
            parser.expect("(")?;
            let (expected_params, return_type) 
            = if let Some(function_data) = symbol_data.get_function_data() {
                is_function_call = true;
                (function_data.params, function_data.return_type)
            } else if let Some(lambda_data) = parser.has_lambda_type(&symbol_data) {
                is_function_call = true;
                (lambda_data.params, lambda_data.return_type)
            } else if let Some(struct_constructor_data) = symbol_data.get_struct_constructor_data() {
                is_function_call = true;
                (struct_constructor_data.params, struct_constructor_data.return_type)
            } else {
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!("'{}' is not callable", token_value.clone())))
                )
            };
            parser.params(&expected_params, 0)?;
            parser.expect(")")?;
            let data_type: Option<Type>;
            match return_type.as_ref() {
                Some(value) => {
                    data_type = Some(Type(value.0.clone()));
                },
                None => {
                    data_type = None;
                }
            }
            is_assignable = false;
            parser.atom_factor(data_type, is_assignable, is_function_call)
        },
        _ => {
            let data_type;
            let is_init;
            if let Some(response) = symbol_data.get_id_data() {
                (data_type, is_init) = (response.0, response.1);
            } else {
                return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    parser.get_code_line(line_number, index),
                    format!("expected identifier, got {} '{}'", 
                    symbol_data.get_category_of_identifier(), token_value.clone())))
                )
            }
            if !is_init {
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!(
                        "cannot access parts of uninitialized identifier '{}'", token_value.clone())))
                    )
            }
            parser.atom_factor(Some(data_type), is_assignable, is_function_call)
        }
    }
}