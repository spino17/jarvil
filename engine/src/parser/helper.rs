use crate::ast::ast::SkippedTokenNode;
use crate::constants::common::NEWLINE;

pub enum IndentResultKind {
    CORRECT_INDENTATION,
    INCORRECT_INDENTATION((i64, i64)),
    BLOCK_OVER,
}

pub struct IndentResult {
    pub kind: IndentResultKind,
    pub skipped_tokens: Vec<SkippedTokenNode>,
    pub extra_newlines: Vec<SkippedTokenNode>,
}

/*
pub fn clone_atom_result(
    result: &Result<(ParseSuccess, Option<Type>, bool, bool), JarvilError>,
) -> Result<(ParseSuccess, Option<Type>, bool, bool), JarvilError> {
    match result {
        Ok(response) => {
            let possible_err = match &response.0.possible_err {
                Some(err) => Some(err.clone()),
                None => None,
            };
            let parse_success = ParseSuccess {
                lookahead: response.0.lookahead,
                possible_err,
            };
            let data_type = match &response.1 {
                Some(data_type) => Some(Type(data_type.0.clone())),
                None => None,
            };
            Ok((parse_success, data_type, response.2, response.3))
        }
        Err(err) => Err(err.clone()),
    }
}

pub fn clone_expr_result(
    result: &Result<(ParseSuccess, bool), JarvilError>,
) -> Result<(ParseSuccess, bool), JarvilError> {
    match result {
        Ok(response) => {
            let possible_err = match &response.0.possible_err {
                Some(err) => Some(err.clone()),
                None => None,
            };
            let parse_success = ParseSuccess {
                lookahead: response.0.lookahead,
                possible_err,
            };
            Ok((parse_success, response.1))
        }
        Err(err) => Err(err.clone()),
    }
}
 */

pub fn format_symbol(symbol: &str) -> &str {
    if symbol == "\n" {
        return NEWLINE;
    }
    return symbol;
}
