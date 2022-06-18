use crate::context;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::Token;
use crate::env::Env;
use crate::errors::{ParseError,aggregate_errors};

// pub fn block<F: FnMut() -> Result<ParseSuccess, ParseError>>(parser: &mut PackratParser, 
//    mut f: F) -> Result<ParseSuccess, ParseError>

pub fn block(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let indent_spaces_unit = context::get_indent();
    // TODO - add a new scope table here
    let curr_env = parser.get_env();
    parser.set_new_env_for_block();
    let mut curr_lookahead = parser.get_lookahead();
    // parser.reset_indent_level(parser.get_indent_level() + 1);
    parser.expect("\n")?;
    loop {
        let (response, indent_spaces) = parser.expect_indent_spaces()?;
        if let Some(err) = response.possible_err {
            // check here whether block got over! by comparing the spaces found and expected
            let indent_factor = (indent_spaces / indent_spaces_unit) as usize;
            let indent_remainder = indent_spaces - indent_factor * indent_spaces_unit;
            if indent_remainder > 0 {
                return Err(err)
            } else {
                if indent_spaces > indent_spaces_unit * parser.get_indent_level() {
                    return Err(err)
                } else {
                    // block is over
                    // TODO - switch back to parent scope
                    parser.reset_indent_level(indent_factor);
                    parser.reset_lookahead(curr_lookahead);
                    parser.reset_env(&curr_env);
                    return Ok(ParseSuccess{
                        lookahead: parser.get_lookahead(),
                        possible_err: None,
                    })
                }
            }
        }
        // f()?;
        // parser.stmt()?;
        match parser.stmt() {
            Ok(_) => {},
            Err(err) => {
                return Ok(ParseSuccess{
                    lookahead: parser.get_lookahead(),
                    possible_err: Some(err),
                })
            }
        }
        curr_lookahead = parser.get_lookahead();
    }
}