use text_size::TextRange;

use crate::ast::ast::SkippedTokenNode;
use crate::constants::common::NEWLINE;
use crate::error::diagnostics::Diagnostics;
use crate::scope::errors::GenericTypeArgsCheckError;

pub enum IndentResultKind {
    CorrectIndentation,
    IncorrectIndentation((i64, i64)),
    BlockOver,
}

pub struct IndentResult {
    pub kind: IndentResultKind,
    pub skipped_tokens: Vec<SkippedTokenNode>,
    pub extra_newlines: Vec<SkippedTokenNode>,
}

pub fn format_symbol(symbol: &str) -> &str {
    if symbol == "\n" {
        return NEWLINE;
    }
    return symbol;
}

pub fn err_for_generic_type_args(
    err: &GenericTypeArgsCheckError,
    identifier_decl_range: TextRange,
) -> Diagnostics {
    match err {
        GenericTypeArgsCheckError::GenericTypeArgsNotExpected => todo!(),
        GenericTypeArgsCheckError::GenericTypeArgsExpected => todo!(),
        GenericTypeArgsCheckError::GenericTypeArgsCountMismatched(
            received_count,
            expected_count,
        ) => todo!(),
        GenericTypeArgsCheckError::GenericTypeArgsIncorrectlyBounded(incorrectly_bounded_types) => {
            todo!()
        }
    }
}
