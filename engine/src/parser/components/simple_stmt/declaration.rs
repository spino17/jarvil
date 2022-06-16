use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError, SemanticError, aggregate_errors};
use crate::parser::components::simple_stmt::helper::r_asssign_alternatives;

pub fn decl(parser: &mut PackratParser) -> Result<usize, ParseError> {
    let (_, _, data_type) = parser.expect_type()?;
    let (_, line_number, token_value) = parser.expect_and_get_value("id")?;
    let lookahead = parser.get_lookahead();
    let (is_matched, (new_lookahead, rule_index, has_float)) = PackratParser::expect_optionally(|| {
        // TODO - match with expr, bexpr and literal (and new id(optparams))
        let (_, line_number) = parser.expect("=")?;
        let (lookahead, rule_index, has_float) = r_asssign_alternatives(parser)?;
        Ok((lookahead, rule_index, has_float))
    }, (lookahead, 0, false))?;
    parser.reset_lookahead(new_lookahead);
    if is_matched {
        let _ = match rule_index {
            0 => {
                if has_float {
                    if !data_type.as_ref().eq("float") {
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                            parser.get_lookahead(), "mismatched types\nright side of the declaration is a float")))
                    } else {
                        ()
                    }
                } else {
                    if !data_type.as_ref().eq("int") {
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                            parser.get_lookahead(), "mismatched types\nright side of the declaration is an int")))
                    } else {
                        ()
                    }
                }
            },
            1 => {
                if !data_type.as_ref().eq("bool") {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                        parser.get_lookahead(), "mismatched types\nright side of the declaration is a bool")))
                } else {
                    ()
                }
            },
            2 => {
                if !data_type.as_ref().eq("string") {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                        parser.get_lookahead(), "mismatched types\nright side of the declaration is a string")))
                } else {
                    ()
                }
            },
            _ => unreachable!("rule index can only be 0, 1 and 2 as there are three alternatives to declaration")
        };
    }
    parser.set_scope(&token_value, &data_type);
    Ok(parser.get_lookahead())
}