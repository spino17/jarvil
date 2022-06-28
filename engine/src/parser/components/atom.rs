use crate::{parser::packrat::{PackratParser, ParseSuccess}, errors::{ParseError, SyntaxError, SemanticError}};
use std::rc::Rc;
use crate::lexer::token::CoreToken;

#[derive(Debug)]
pub enum CompoundPart {
    INDEX_TYPE((Rc<String>, usize)),  // (index type, index)
    PROPERTRY_NAME((Rc<String>, usize)),  // (identifier name, index)
    METHOD_DATA((Rc<String>, Vec<(Rc<String>, usize)>, usize)),  // (method name, datatype of the params passed, index)
}

pub fn atom_expr_bexpr_literal(parser: &mut PackratParser) -> Result<(ParseSuccess, Rc<String>), ParseError> {
    // TODO - match expr, bexpr, literal and atom
    todo!()
}

pub fn atom_index_access(parser: &mut PackratParser) -> Result<(ParseSuccess, CompoundPart), ParseError> {
    parser.expect("[")?;
    let index = parser.get_index();
    let (_, index_data_type) = parser.atom_expr_bexpr_literal()?;
    let (response, _) = parser.expect("]")?;
    Ok((response, CompoundPart::INDEX_TYPE((index_data_type, index))))
}

pub fn atom_propertry_or_method_access(parser: &mut PackratParser) -> Result<(ParseSuccess, CompoundPart), ParseError> {
    parser.expect(".")?;
    let index = parser.get_index();
    let (response, _, token_value) = parser.expect_any_id()?;
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            parser.expect("(")?;
            let (_, _, params_data_type_vec) = parser.params()?;
            let (response, _) = parser.expect(")")?;
            Ok((response, CompoundPart::METHOD_DATA((token_value.clone(), params_data_type_vec, index))))
        },
        _ => {
            Ok((response, CompoundPart::PROPERTRY_NAME((token_value.clone(), index))))
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
}

pub fn check_atom_factor(parser: &mut PackratParser, 
    data_type: Option<Rc<String>>, mut is_assignable: bool) -> Result<(ParseSuccess, Option<Rc<String>>, bool), ParseError> {
    let (response, line_number, sub_part_access_vec) = parser.atom_factor()?;
    let mut curr_type = data_type;
    for entry in &sub_part_access_vec {
        match entry {
            CompoundPart::INDEX_TYPE((key_data_type, index)) => {
                if let Some(curr_type_val) = curr_type {
                    todo!()
                } else {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, *index),
                       format!("type 'None' is not indexable with key of type '{}'", key_data_type))
                    ))
                }
            },
            CompoundPart::PROPERTRY_NAME((property_name, index)) => {
                if let Some(curr_type_val) = curr_type {
                    if let Some(field_data_type) = parser.has_field_with_name(&curr_type_val, property_name) {
                        curr_type = Some(field_data_type.clone());
                    } else {
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                            parser.get_code_line(line_number, *index),
                           format!("type '{}' has no propertry named '{}'", curr_type_val, property_name))
                        ))
                    }
                } else {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, *index),
                       format!("type 'None' has no propertry named '{}'", property_name))
                    ))
                }
            },
            CompoundPart::METHOD_DATA(method_data) => {
                if let Some(curr_type_val) = curr_type {
                    if let Some(function_data) = 
                    parser.has_method_with_name(&curr_type_val, &method_data.0) {
                        let params = function_data.params;
                        let return_type = function_data.return_type;
                        let params_data_type_vec = method_data.1.clone();
                        let params_len = params.len();
                        let params_data_type_vec_len = params_data_type_vec.len();
                        if params_data_type_vec_len != params_len {
                            return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                                parser.get_code_line(line_number, method_data.2), 
                                format!("expected '{}' number of arguments to the method '{}', got '{}'", 
                                params_len, method_data.0, params_data_type_vec_len)))
                            )
                        }
                        for i in 0..params_data_type_vec_len {
                            let curr_data_type = params_data_type_vec[i].0.clone();
                            let expected_data_type = params[i].1.clone();
                            if !curr_data_type.eq(&expected_data_type) {
                                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                                    parser.get_code_line(line_number, params_data_type_vec[i].1),
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
                        is_assignable = false;
                    } else {
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                            parser.get_code_line(line_number, method_data.2),
                            format!("type '{}' has no method named '{}'", curr_type_val, method_data.0))
                        ))
                    }
                } else {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, method_data.2),
                       format!("type 'None' has no method named '{}'", method_data.0))
                    ))
                }
            },
        }
    }
    Ok((response, curr_type, is_assignable))
}

pub fn atom(parser: &mut PackratParser) -> Result<(ParseSuccess, Option<Rc<String>>, bool), ParseError> {
    let (_, line_number, 
        token_value, symbol_data) = parser.expect_any_id_in_scope()?;
    let mut is_assignable = true;
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            // TODO - check id for type also (constructor in that case)
            let params: Rc<Vec<(Rc<String>, Rc<String>)>>;
            let return_type: Rc<Option<Rc<String>>>;
            if let Some(function_data) = symbol_data.get_function_data() {
                (params, return_type) = (function_data.params, function_data.return_type);
            } else if let Some(lambda_data) = parser.has_lambda_type(&symbol_data) {
                (params, return_type) = (lambda_data.params, lambda_data.return_type);
            } else if let Some(struct_constructor_data) = symbol_data.get_struct_constructor_data() {
                (params, return_type) = (struct_constructor_data.params, struct_constructor_data.return_type);
            } else {
                let index = parser.get_index();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!("'{}' of type {} is not callable", 
                    token_value.clone(), symbol_data.get_type_of_identifier())))
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
                let curr_data_type = params_data_type_vec[i].0.clone();
                let expected_data_type = params[i].1.clone();
                if !curr_data_type.eq(&expected_data_type) {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, params_data_type_vec[i].1), 
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
            is_assignable = false;
            parser.check_atom_factor(data_type, is_assignable)
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
                    symbol_data.get_type_of_identifier(), token_value.clone())))
                )
            }
            if !is_init {
                let index = parser.get_index();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!(
                        "cannot access parts of uninitialized identifier '{}'", token_value.clone())))
                    )
            }
            parser.check_atom_factor(Some(data_type), is_assignable)
        }
    }
}