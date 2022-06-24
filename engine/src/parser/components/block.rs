use std::rc::Rc;
use crate::context;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError};
use rustc_hash::FxHashMap;

pub fn block(parser: &mut PackratParser, params: Option<&Vec<(Rc<String>, Rc<String>)>>) -> Result<ParseSuccess, ParseError> {
    parser.expect("\n")?;
    let indent_spaces_unit = context::get_indent();
    let curr_env = parser.get_env();
    parser.set_new_env_for_block();
    parser.set_params_to_scope(params);
    let mut curr_lookahead = parser.get_lookahead();
    parser.reset_indent_level(parser.get_indent_level() + 1);
    loop {
        let (response, indent_spaces) = parser.expect_indent_spaces()?;
        if let Some(err) = response.possible_err {
            // check here whether block got over! by comparing the spaces found and expected
            let indent_factor = indent_spaces / indent_spaces_unit as i64;
            let indent_remainder = indent_spaces - indent_factor * indent_spaces_unit;
            if indent_remainder > 0 {
                return Err(err)
            } else {
                if indent_spaces > indent_spaces_unit * parser.get_indent_level() {
                    return Err(err)
                } else {
                    // block is over
                    parser.reset_env(&curr_env);
                    parser.reset_indent_level(parser.get_indent_level() - 1);
                    parser.reset_lookahead(curr_lookahead);
                    return Ok(ParseSuccess{
                        lookahead: parser.get_lookahead(),
                        possible_err: None,
                    })
                }
            }
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

pub fn struct_block(parser: &mut PackratParser) -> Result<(ParseSuccess, FxHashMap<Rc<String>, Rc<String>>), ParseError> {
    parser.expect("\n")?;
    let indent_spaces_unit = context::get_indent();
    let curr_env = parser.get_env();
    parser.set_new_env_for_block();
    let mut curr_lookahead = parser.get_lookahead();
    parser.reset_indent_level(parser.get_indent_level() + 1);
    let mut fields_map: FxHashMap<Rc<String>, Rc<String>> = FxHashMap::default();
    loop {
        let (response, indent_spaces) = parser.expect_indent_spaces()?;
        if let Some(err) = response.possible_err {
            // check here whether block got over! by comparing the spaces found and expected
            let indent_factor = indent_spaces / indent_spaces_unit as i64;
            let indent_remainder = indent_spaces - indent_factor * indent_spaces_unit;
            if indent_remainder > 0 {
                return Err(err)
            } else {
                if indent_spaces > indent_spaces_unit * parser.get_indent_level() {
                    return Err(err)
                } else {
                    // block is over
                    parser.reset_env(&curr_env);
                    parser.reset_indent_level(parser.get_indent_level() - 1);
                    parser.reset_lookahead(curr_lookahead);
                    return Ok((ParseSuccess{
                        lookahead: parser.get_lookahead(),
                        possible_err: None,
                    }, fields_map))
                }
            }
        }
        let (_, _, data_type, token_value) = parser.l_decl()?;
        fields_map.insert(token_value.0, data_type.0);
        match parser.expect("\n") {
            Ok(_) => {},
            Err(err) => {
                if parser.check_next_token("endmarker") {
                    return Ok((ParseSuccess{
                        lookahead: parser.get_lookahead(),
                        possible_err: Some(err),
                    }, fields_map))
                } else {
                    return Err(err)
                }
            }
        }
        curr_lookahead = parser.get_lookahead();
    }
}