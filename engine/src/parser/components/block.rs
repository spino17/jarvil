use std::rc::Rc;
use std::vec;
use crate::context;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError};
use crate::scope::{Env};
use crate::types::Type;

pub fn check_block_indentation(parser: &mut PackratParser, 
    indent_spaces: i64, err: ParseError, curr_env: &Env, curr_lookahead: usize) -> Result<ParseSuccess, ParseError> {
    let indent_spaces_unit = context::get_indent();
    let indent_factor = indent_spaces / indent_spaces_unit as i64;
    let indent_remainder = indent_spaces - indent_factor * indent_spaces_unit;
    if indent_remainder > 0 {
        return Err(err)
    } else {
        if indent_spaces > indent_spaces_unit * parser.get_indent_level() {
            return Err(err)
        } else {
            // block is over
            parser.reset_env(curr_env);
            parser.reset_indent_level(parser.get_indent_level() - 1);
            parser.reset_lookahead(curr_lookahead);
            return Ok(ParseSuccess{
                lookahead: parser.get_lookahead(),
                possible_err: None,
            })
        }
    }
}

pub fn block(parser: &mut PackratParser, params: Option<&Vec<(Rc<String>, Type)>>) -> Result<ParseSuccess, ParseError> {
    parser.expect("\n")?;
    let curr_env = parser.get_env();
    parser.set_new_env_for_block();
    parser.set_params_to_scope(params);
    let mut curr_lookahead = parser.get_lookahead();
    parser.reset_indent_level(parser.get_indent_level() + 1);
    loop {
        let (response, indent_spaces) = parser.expect_indent_spaces()?;
        if let Some(err) = response.possible_err {
            return parser.check_block_indentation(indent_spaces, err, &curr_env, curr_lookahead)
        }
        match parser.stmt() {
            Ok(_) => {},
            Err(err) => {
                if parser.check_next_token("endmarker") {
                    return Ok(ParseSuccess{
                        lookahead: parser.get_lookahead(),
                        possible_err: Some(err),
                    })
                } else {
                    return Err(err)
                }
            }
        }
        curr_lookahead = parser.get_lookahead();
    }
}

pub fn struct_block(parser: &mut PackratParser) -> Result<(ParseSuccess, Vec<(Rc<String>, Type)>), ParseError> {
    parser.expect("\n")?;
    let indent_spaces_unit = context::get_indent();
    let curr_env = parser.get_env();
    parser.set_new_env_for_block();
    let mut curr_lookahead = parser.get_lookahead();
    parser.reset_indent_level(parser.get_indent_level() + 1);
    // let mut fields_map: FxHashMap<Rc<String>, Rc<String>> = FxHashMap::default();
    let mut fields_vec: Vec<(Rc<String>, Type)> = vec![];
    loop {
        let (response, indent_spaces) = parser.expect_indent_spaces()?;
        if let Some(err) = response.possible_err {
            let response = parser.check_block_indentation(indent_spaces, err, &curr_env, curr_lookahead)?;
            return Ok((response, fields_vec))
        }
        let (_, _, data_type, field_name) = parser.param_decl()?;
        fields_vec.push((field_name, data_type));
        match parser.expect("\n") {
            Ok(_) => {},
            Err(err) => {
                if parser.check_next_token("endmarker") {
                    return Ok((ParseSuccess{
                        lookahead: parser.get_lookahead(),
                        possible_err: Some(err),
                    }, fields_vec))
                } else {
                    return Err(err)
                }
            }
        }
        curr_lookahead = parser.get_lookahead();
    }
}

pub fn impl_for_struct_block(parser: &mut PackratParser, 
    struct_name: &Rc<String>) -> Result<ParseSuccess, ParseError> {
    // TODO - add methods to struct type as soon as a method is parsed
    todo!()
}