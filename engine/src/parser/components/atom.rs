use crate::{parser::packrat::{PackratParser, ParseSuccess}, errors::{ParseError, SyntaxError}};
use std::rc::Rc;
use crate::lexer::token::CoreToken;

pub enum CompoundPart {
    INDEX_TYPE(Rc<String>),  // (index type, is_init)
    PROPERTRY_NAME(Rc<String>),  // identifier name
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

pub fn atom(parser: &mut PackratParser) -> Result<(ParseSuccess, Rc<String>), ParseError> {
    let (response, line_number, 
        token_value, data_type, is_init) = parser.expect_id()?;
    if !is_init {
        return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
            line_number,
            parser.get_code_line(line_number),
            parser.get_index(), format!(
                "cannot access parts of uninitialized identifier '{}'", token_value.0.clone())))
            )
    }
    let (response, sub_part_access_vec) = parser.atom_factor()?;
    let curr_type = data_type.clone();
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
            }
        }
    }
    // we can also capture token_value here
    Ok((response, curr_type))
}