use crate::{parser::packrat::{PackratParser, ParseSuccess}, errors::{ParseError, SyntaxError}};
use std::rc::Rc;
use crate::lexer::token::CoreToken;

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

pub fn atom_propertry_access(parser: &mut PackratParser) -> Result<(ParseSuccess, CompoundPart), ParseError> {
    parser.expect(".")?;
    let (response, _, token_value) = parser.expect_any_id()?;
    Ok((response, CompoundPart::PROPERTRY_NAME(token_value.0.clone())))
}

pub fn atom_index_or_propetry_access(parser: &mut PackratParser) -> Result<(ParseSuccess, CompoundPart), ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::LSQUARE => {
            return parser.atom_index_access()
        },
        CoreToken::DOT => {
            return parser.atom_propertry_access()
        },
        _ => {
            // possible error
            todo!()
        }
    }
}

pub fn atom_factor(parser: &mut PackratParser) -> Result<(ParseSuccess, Vec<CompoundPart>), ParseError> {
    let mut sub_part_access_vec: Vec<CompoundPart> = vec![];
    let (response, compound_part) = parser.atom_index_or_propetry_access()?;
    if let Some(possible_err) = response.possible_err {
        // check this error to match empty string - check FOLLOW(id)
        todo!()
    }
    sub_part_access_vec.push(compound_part);
    let (response, mut remaining_sub_part_access_vec) = parser.atom_factor()?;
    sub_part_access_vec.append(&mut remaining_sub_part_access_vec);
    Ok((response, sub_part_access_vec))
}

pub fn check_atom_factor(parser: &mut PackratParser, 
    data_type: &Rc<String>, is_init: bool) -> Result<(ParseSuccess, Rc<String>), ParseError> {
    let (response, sub_part_access_vec) = parser.atom_factor()?;
    let mut curr_type = data_type.clone();
    let curr_is_init = is_init;
    for entry in &sub_part_access_vec {
        match entry {
            CompoundPart::INDEX_TYPE(key_data_type) => {
                // check whether curr_type is iterable with key having key_data_type.
                // set curr_type to the type of the value of the above iterable (List, Dict)
                // else give error => not iterable on index with type '{}' or not an iterable
                // curr_is_init && is_init -> curr_is_init
                todo!()
            },
            CompoundPart::PROPERTRY_NAME(property_name) => {
                // check whether curr_type has a field with name property_name.
                // set curr_type to the type of that field
                // else give error => does not have a propertry named '{}'
                todo!()
            },
            CompoundPart::METHOD_DATA(method_data) => {
                // check whether curr_type has a field with name property_name.
                // set curr_type to the type of that field
                // else give error => does not have a propertry named '{}'
                todo!()
            },
        }
    }
    // we can also capture token_value here
    Ok((response, curr_type))
}

pub fn atom(parser: &mut PackratParser) -> Result<(ParseSuccess, Rc<String>), ParseError> {
    let (response, line_number, 
        token_value, symbol_data) = parser.expect_any_id_in_scope()?;
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            // check wther it is function or lambda using symbol_data
            let mut params: Rc<Vec<(Rc<String>, Rc<String>)>>;
            let mut return_type: Rc<Option<Rc<String>>>;
            if let Some(response) = symbol_data.get_function_data() {
                (params, return_type) = (response.params, response.return_type);
            }
            if let Some(lambda_data) = parser.has_lambda_type(&symbol_data) {
                (params, return_type) = (lambda_data.params, lambda_data.return_type);
            } else {
                return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    line_number, 
                    parser.get_code_line(line_number),
                    parser.get_index(), 
                    format!("'{}' of type {} is not callable", 
                    token_value.0.clone(), symbol_data.get_type_of_identifier())))
                )
            }
            let data_type;  // get from return type of id '(' params ')'
            parser.check_atom_factor(&data_type, true);
            // do the semantic check for params passed in the function
            todo!()
        },
        _ => {
            let data_type;
            let is_init;
            if let Some(response) = symbol_data.get_id_data() {
                (data_type, is_init) = (response.0, response.1);
            } else {
                return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    line_number,
                    parser.get_code_line(line_number),
                    parser.get_index(),
                    format!("expected an identifier, got a {} '{}'", 
                    symbol_data.get_type_of_identifier(), token_value.0.clone())))
                )
            }
            if !is_init {
                return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    line_number,
                    parser.get_code_line(line_number),
                    parser.get_index(), format!(
                        "cannot access parts of uninitialized identifier '{}'", token_value.0.clone())))
                    )
            }
            parser.check_atom_factor(&data_type, is_init)
        }
    }
}