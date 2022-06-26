use crate::{parser::packrat::{PackratParser, ParseSuccess}, errors::{ParseError, SyntaxError, SemanticError}};
use std::rc::Rc;
use crate::lexer::token::CoreToken;

#[derive(Debug)]
pub enum CompoundPart {
    INDEX_TYPE(Rc<String>),  // (index type, is_init)
    PROPERTRY_NAME(Rc<String>),  // identifier name
    METHOD_DATA((Rc<String>, Vec<Rc<String>>)),  // (method name, datatype of the params passed)
}

pub fn atom_expr_bexpr_literal(parser: &mut PackratParser) -> Result<(ParseSuccess, Rc<String>), ParseError> {
    // TODO - match expr, bexpr, literal and atom
    todo!()
}

pub fn atom_index_access(parser: &mut PackratParser) -> Result<(ParseSuccess, CompoundPart), ParseError> {
    parser.expect("[")?;
    let (_, index_data_type) = parser.atom_expr_bexpr_literal()?;
    let (response, _) = parser.expect("]")?;
    Ok((response, CompoundPart::INDEX_TYPE(index_data_type)))
}

pub fn atom_propertry_or_method_access(parser: &mut PackratParser) -> Result<(ParseSuccess, CompoundPart), ParseError> {
    parser.expect(".")?;
    let (response, _, token_value) = parser.expect_any_id()?;
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            parser.expect("(")?;
            let (_, _, params_data_type_vec) = parser.params()?;
            let (response, _) = parser.expect(")")?;
            Ok((response, CompoundPart::METHOD_DATA((token_value.0.clone(), params_data_type_vec))))
        },
        _ => {
            Ok((response, CompoundPart::PROPERTRY_NAME(token_value.0.clone())))
        }
    }
}

pub fn atom_index_or_propetry_access(parser: &mut PackratParser) -> Result<(ParseSuccess, Option<CompoundPart>), ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::LSQUARE => {
            match parser.atom_index_access() {
                Ok(response) => {
                    Ok((response.0, Some(response.1)))
                },
                Err(err) => Err(err)
            }
        },
        CoreToken::DOT => {
            // TODO - add parser.atom_propertry_or_method_access()
            match parser.atom_propertry_or_method_access() {
                Ok(response) => {
                    Ok((response.0, Some(response.1)))
                },
                Err(err) => Err(err)
            }
        },
        _ => {
            Ok((ParseSuccess{
                lookahead: parser.get_lookahead(),
                possible_err: None,
            }, None))
        }
    }
}

pub fn atom_factor(parser: &mut PackratParser) -> Result<(ParseSuccess, usize, Vec<CompoundPart>), ParseError> {
    let mut sub_part_access_vec: Vec<CompoundPart> = vec![];
    // let response: ParseSuccess;
    // let compound_part: CompoundPart;
    let (_, compound_part) = parser.atom_index_or_propetry_access()?;
    if let Some(compound_part) = compound_part {
        sub_part_access_vec.push(compound_part);
        let (response, line_number, mut remaining_sub_part_access_vec) = parser.atom_factor()?;
        sub_part_access_vec.append(&mut remaining_sub_part_access_vec);
        Ok((response, line_number, sub_part_access_vec))
    } else {
        return Ok((ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: None,
        }, parser.get_curr_line_number(), vec![]))
    }
    /*
    match parser.atom_index_or_propetry_access() {
        Ok((resp, cmpd_part)) => {
            response = resp;
            compound_part = cmpd_part;
        },
        Err(err) => {
            // handle possible error using FOLLOW(id)
            println!("error coming: {:?}", err);
            return Ok((ParseSuccess{
                lookahead: parser.get_lookahead(),
                possible_err: None,
            }, parser.get_curr_line_number(), vec![]))
        }
    }*/
    /*
    let (response, compound_part) = parser.atom_index_or_propetry_access()?;
    if let Some(possible_err) = response.possible_err {
        // check this error to match empty string - check FOLLOW(id)
        todo!()
    }*/
}

pub fn check_atom_factor(parser: &mut PackratParser, 
    data_type: Option<Rc<String>>, is_init: bool) -> Result<(ParseSuccess, Option<Rc<String>>), ParseError> {
    let (response, line_number, sub_part_access_vec) = parser.atom_factor()?;
    let mut curr_type = data_type;
    let curr_is_init = is_init;
    for entry in &sub_part_access_vec {
        match entry {
            CompoundPart::INDEX_TYPE(key_data_type) => {
                // check whether curr_type is iterable with key having key_data_type.
                // set curr_type to the type of the value of the above iterable (List, Dict)
                // else give error => not iterable on index with type '{}' or not an iterable
                // curr_is_init && is_init -> curr_is_init
                if let Some(curr_type_val) = curr_type {
                    todo!()
                } else {
                    let index = parser.get_index();
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, index),
                       format!("type 'None' is not indexable with key of type '{}'", key_data_type))
                    ))
                }
            },
            CompoundPart::PROPERTRY_NAME(property_name) => {
                // check whether curr_type has a field with name property_name.
                // set curr_type to the type of that field
                // else give error => does not have a propertry named '{}'
                if let Some(curr_type_val) = curr_type {
                    if let Some(field_data_type) = parser.has_field_with_name(&curr_type_val, property_name) {
                        curr_type = Some(field_data_type.clone());
                    } else {
                        let index = parser.get_index();
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                            parser.get_code_line(line_number, index),
                           format!("type '{}' has no propertry named '{}'", curr_type_val, property_name))
                        ))
                    }
                } else {
                    let index = parser.get_index();
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, index),
                       format!("type 'None' has no propertry named '{}'", property_name))
                    ))
                }
            },
            CompoundPart::METHOD_DATA(method_data) => {
                // check whether curr_type has a field with name property_name.
                // set curr_type to the type of that field
                // else give error => does not have a propertry named '{}'
                if let Some(curr_type_val) = curr_type {
                    if let Some(function_data) = parser.has_method_with_name(&curr_type_val, &method_data.0) {
                        // curr_type = Some(field_data_type.clone());
                        let params = function_data.params;
                        let return_type = function_data.return_type;
                        let params_data_type_vec = method_data.1.clone();
                        let params_len = params.len();
                        let params_data_type_vec_len = params_data_type_vec.len();
                        if params_data_type_vec_len != params_len {
                            let index = parser.get_index();
                            return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                                parser.get_code_line(line_number, index), 
                                format!("expected '{}' number of arguments to the method '{}', got '{}'", 
                                params_len, method_data.0, params_data_type_vec_len)))
                            )
                        }
                        for i in 0..params_data_type_vec_len {
                            let curr_data_type = params_data_type_vec[i].clone();
                            let expected_data_type = params[i].1.clone();
                            if !curr_data_type.eq(&expected_data_type) {
                                let index = parser.get_index();
                                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                                    parser.get_code_line(line_number, index),
                                    format!("expected type '{}' for argument '{}' in method '{}', got '{}'", 
                                    expected_data_type, i, method_data.0, curr_data_type)))
                                ) 
                            }
                        }
                        match return_type.as_ref() {
                            Some(value) => {
                                curr_type = Some(value.clone());
                            },
                            None => {
                                curr_type = None;
                            }
                        }
                    } else {
                        let index = parser.get_index();
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                            parser.get_code_line(line_number, index),
                            format!("type '{}' has no method named '{}'", curr_type_val, method_data.0))
                        ))
                    }
                } else {
                    let index = parser.get_index();
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, index),
                       format!("type 'None' has no method named '{}'", method_data.0))
                    ))
                }
            },
        }
    }
    // we can also capture token_value here
    Ok((response, curr_type))
}

pub fn atom(parser: &mut PackratParser) -> Result<(ParseSuccess, Option<Rc<String>>), ParseError> {
    let (_, line_number, 
        token_value, symbol_data) = parser.expect_any_id_in_scope()?;
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            let params: Rc<Vec<(Rc<String>, Rc<String>)>>;
            let return_type: Rc<Option<Rc<String>>>;
            if let Some(response) = symbol_data.get_function_data() {
                (params, return_type) = (response.params, response.return_type);
            } else if let Some(lambda_data) = parser.has_lambda_type(&symbol_data) {
                (params, return_type) = (lambda_data.params, lambda_data.return_type);
            } else {
                let index = parser.get_index();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!("'{}' of type {} is not callable", 
                    token_value.0.clone(), symbol_data.get_type_of_identifier())))
                )
            }
            parser.expect("(")?;
            let (_, line_number, params_data_type_vec) = parser.params()?;
            let params_data_type_vec_len = params_data_type_vec.len();
            let params_len = params.len();
            if params_data_type_vec_len != params_len {
                let index = parser.get_index();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!("expected '{}' number of arguments to the function, got '{}'", 
                    params_len, params_data_type_vec_len)))
                )
            }
            for i in 0..params_data_type_vec_len {
                let curr_data_type = params_data_type_vec[i].clone();
                let expected_data_type = params[i].1.clone();
                let index = parser.get_index();
                if !curr_data_type.eq(&expected_data_type) {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, index), 
                        format!("expected type '{}' for argument '{}', got '{}'", 
                        expected_data_type, i, curr_data_type)))
                    ) 
                }
            }
            parser.expect(")")?;
            let data_type: Option<Rc<String>>;
            match return_type.as_ref() {
                Some(value) => {
                    data_type = Some(value.clone());
                },
                None => {
                    data_type = None;
                }
            }
            parser.check_atom_factor(data_type, true)
        },
        _ => {
            let data_type;
            let is_init;
            if let Some(response) = symbol_data.get_id_data() {
                (data_type, is_init) = (response.0, response.1);
            } else {
                let index = parser.get_index();
                return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    parser.get_code_line(line_number, index),
                    format!("expected an identifier, got a {} '{}'", 
                    symbol_data.get_type_of_identifier(), token_value.0.clone())))
                )
            }
            if !is_init {
                let index = parser.get_index();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!(
                        "cannot access parts of uninitialized identifier '{}'", token_value.0.clone())))
                    )
            }
            parser.check_atom_factor(Some(data_type), is_init)
        }
    }
}