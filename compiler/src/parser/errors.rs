use super::parser::JarvilParser;
use crate::ast::ast::Node;
use crate::ast::ast::SkippedTokenNode;
use crate::error::diagnostics::{
    Diagnostics, IncorrectlyIndentedBlockError, InvalidLValueError, InvalidTrailingTokensError,
    NoValidStatementFoundInsideBlockBodyError, SingleSubTypeFoundInTupleError,
};
use crate::{error::diagnostics::MissingTokenError, lexer::token::Token};
use text_size::TextRange;

pub fn log_missing_token_error(
    parser: &JarvilParser,
    expected_symbols: &[&'static str],
    received_token: &Token,
) {
    if parser.ignore_all_errors {
        return;
    }
    // -> TODO - check whether error on same line already exists
    let err = MissingTokenError::new(expected_symbols, received_token);
    parser
        .errors
        .borrow_mut()
        .push(Diagnostics::MissingToken(err));
}

pub fn log_trailing_skipped_tokens_error(
    parser: &JarvilParser,
    skipped_tokens: Vec<SkippedTokenNode>,
) {
    if parser.ignore_all_errors {
        return;
    }
    // -> TODO - check whether error on same line already exists
    let err = InvalidTrailingTokensError::new(
        skipped_tokens[0].range().start().into(),
        skipped_tokens[skipped_tokens.len() - 1]
            .range()
            .end()
            .into(),
    );
    parser
        .errors
        .borrow_mut()
        .push(Diagnostics::InvalidTrailingTokens(err));
}

pub fn log_incorrectly_indented_block_error(
    parser: &JarvilParser,
    range: TextRange,
    expected_indent: i64,
    received_indent: i64,
) {
    if parser.ignore_all_errors {
        return;
    }
    // -> TODO - check whether error on same line already exists
    let err = IncorrectlyIndentedBlockError::new(expected_indent, received_indent, range);
    parser
        .errors
        .borrow_mut()
        .push(Diagnostics::IncorrectlyIndentedBlock(err));
}

pub fn log_no_valid_statement_inside_block_error(parser: &JarvilParser, range: TextRange) {
    if parser.ignore_all_errors {
        return;
    }
    let err = NoValidStatementFoundInsideBlockBodyError::new(range);
    parser
        .errors
        .borrow_mut()
        .push(Diagnostics::NoValidStatementFoundInsideBlockBody(err));
}

pub fn log_invalid_l_value_error(parser: &JarvilParser, range: TextRange) {
    if parser.ignore_all_errors {
        return;
    }
    // -> TODO - check whether error on same line already exists
    let err = InvalidLValueError::new(range);
    parser
        .errors
        .borrow_mut()
        .push(Diagnostics::InvalidLValue(err));
}

pub fn log_single_sub_type_in_tuple_error(parser: &JarvilParser, range: TextRange) {
    if parser.ignore_all_errors {
        return;
    }
    let err = SingleSubTypeFoundInTupleError::new(range);
    parser
        .errors
        .borrow_mut()
        .push(Diagnostics::SingleSubTypeFoundInTuple(err));
}
