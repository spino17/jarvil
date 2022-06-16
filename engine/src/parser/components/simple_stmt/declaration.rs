use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError, SemanticError, aggregate_errors};
use crate::parser::components::simple_stmt::helper::r_asssign_alternatives;
use crate::parser::packrat::ParseSuccess;

pub fn decl(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let (_, _, data_type) = parser.expect_type()?;
    let (_, line_number, token_value) = parser.expect_and_get_value("identifier")?;
    let curr_lookahead = parser.get_lookahead();
    let (is_matched, (response, rule_index, has_float), err) = 
    PackratParser::expect_optionally(|| {
        // TODO - match with expr, bexpr and literal (and new id(optparams))
        let (_, _) = parser.expect("=")?;
        let (lookahead, rule_index, has_float) = r_asssign_alternatives(parser)?;
        Ok((lookahead, rule_index, has_float))
    }, (ParseSuccess{
        lookahead: curr_lookahead,
        possible_err: None,
    }, 0, false));
    parser.reset_lookahead(response.lookahead);
    if is_matched {
        let _ = match rule_index {
            0 => {
                if has_float {
                    if !data_type.as_ref().eq("float") {
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                            parser.get_lookahead(), 
                            format!(
                                "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                                data_type, "float")))
                            )
                    } else {
                        ()
                    }
                } else {
                    if !data_type.as_ref().eq("int") {
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                            parser.get_lookahead(), 
                            format!(
                                "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                            data_type, "int")))
                        )
                    } else {
                        ()
                    }
                }
            },
            1 => {
                if !data_type.as_ref().eq("bool") {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                        parser.get_lookahead(), 
                        format!(
                            "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                            data_type, "bool"))))
                } else {
                    ()
                }
            },
            2 => {
                if !data_type.as_ref().eq("string") {
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                        parser.get_lookahead(), 
                        format!(
                            "mismatched types\nidentifier declared with type '{}', got assigned with value of type '{}'", 
                            data_type, "string")))
                        )
                } else {
                    ()
                }
            },
            _ => unreachable!("rule index can only be 0, 1 and 2 as there are three alternatives to declaration")
        };
        parser.set_scope(&token_value, &data_type);
        Ok(ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: None,
        })
    } else {
        parser.set_scope(&token_value, &data_type);
        Ok(ParseSuccess{
            lookahead: parser.get_lookahead(),
            possible_err: err,
        })
    }
}