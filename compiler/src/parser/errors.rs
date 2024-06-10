use super::parser::JarvilParser;
use crate::ast::ast::SkippedTokenNode;
use crate::ast::traits::Node;
use crate::error::diagnostics::{
    Diagnostics, IncorrectlyIndentedBlockError, InvalidLValueError, InvalidTrailingTokensError,
    NoValidStatementFoundInsideBlockBodyError, SingleSubTypeFoundInTupleError,
};
use crate::{error::diagnostics::MissingTokenError, lexer::token::Token};
use text_size::TextRange;

impl<'ctx> JarvilParser<'ctx> {
    pub fn log_missing_token_error(
        &self,
        expected_symbols: &[&'static str],
        received_token: &Token,
    ) {
        if self.ignore_all_errors() {
            return;
        }

        let err = MissingTokenError::new(expected_symbols, received_token);
        self.errors.log_error(Diagnostics::MissingToken(err));
    }

    pub fn log_trailing_skipped_tokens_error(&self, skipped_tokens: Vec<SkippedTokenNode>) {
        if self.ignore_all_errors() {
            return;
        }

        let err = InvalidTrailingTokensError::new(
            skipped_tokens[0].range().start().into(),
            skipped_tokens[skipped_tokens.len() - 1]
                .range()
                .end()
                .into(),
        );
        self.errors
            .log_error(Diagnostics::InvalidTrailingTokens(err));
    }

    pub fn log_incorrectly_indented_block_error(
        &self,
        range: TextRange,
        expected_indent: i64,
        received_indent: i64,
    ) {
        if self.ignore_all_errors() {
            return;
        }

        let err = IncorrectlyIndentedBlockError::new(expected_indent, received_indent, range);
        self.errors
            .log_error(Diagnostics::IncorrectlyIndentedBlock(err));
    }

    pub fn log_no_valid_statement_inside_block_error(&self, range: TextRange) {
        if self.ignore_all_errors() {
            return;
        }

        let err = NoValidStatementFoundInsideBlockBodyError::new(range);
        self.errors
            .log_error(Diagnostics::NoValidStatementFoundInsideBlockBody(err));
    }

    pub fn log_invalid_l_value_error(&self, range: TextRange) {
        if self.ignore_all_errors() {
            return;
        }

        let err = InvalidLValueError::new(range);
        self.errors.log_error(Diagnostics::InvalidLValue(err));
    }

    pub fn log_single_sub_ty_in_tuple_error(&self, range: TextRange) {
        if self.ignore_all_errors() {
            return;
        }

        let err = SingleSubTypeFoundInTupleError::new(range);
        self.errors
            .log_error(Diagnostics::SingleSubTypeFoundInTuple(err));
    }
}
