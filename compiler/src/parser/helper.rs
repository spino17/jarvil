use crate::ast::ast::SkippedTokenNode;
use crate::constants::common::NEWLINE;
use crate::error::diagnostics::{
    Diagnostics, GenericTypeArgsCountMismatchedError, GenericTypeArgsExpectedError,
    GenericTypeArgsIncorrectlyBoundedError, GenericTypeArgsNotExpectedError,
};
use crate::error::helper::IdentifierKind;
use crate::scope::errors::GenericTypeArgsCheckError;
use text_size::TextRange;

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
    symbol
}

pub fn err_for_generic_ty_args(
    err: &GenericTypeArgsCheckError,
    identifier_decl_range: TextRange,
    identifier_kind: IdentifierKind,
) -> Diagnostics {
    match err {
        GenericTypeArgsCheckError::GenericTypeArgsNotExpected => {
            let err = GenericTypeArgsNotExpectedError::new(identifier_kind, identifier_decl_range);
            Diagnostics::GenericTypeArgsNotExpected(err)
        }
        GenericTypeArgsCheckError::GenericTypeArgsExpected => {
            let err = GenericTypeArgsExpectedError::new(identifier_kind, identifier_decl_range);
            Diagnostics::GenericTypeArgsExpected(err)
        }
        GenericTypeArgsCheckError::GenericTypeArgsCountMismatched(
            received_count,
            expected_count,
        ) => {
            let err = GenericTypeArgsCountMismatchedError::new(
                *received_count,
                *expected_count,
                identifier_decl_range,
            );
            Diagnostics::GenericTypeArgsCountMismatched(err)
        }
        GenericTypeArgsCheckError::GenericTypeArgsIncorrectlyBounded(incorrectly_bounded_types) => {
            let err = GenericTypeArgsIncorrectlyBoundedError::new(incorrectly_bounded_types);
            Diagnostics::GenericTypeArgsIncorrectlyBounded(err)
        }
    }
}
