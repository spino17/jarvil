use miette::{Diagnostic, Report, SourceSpan};
use std::fmt;
use thiserror::Error;

#[derive(Clone, Debug)]
pub enum Diagnostics {
    InvalidChar(InvalidCharError),
    NoClosingSymbol(NoClosingSymbolError),
    MissingToken,
    InvalidTrailingTokens,
    IncorrectlyIndentedBlock,
    InvalidLValue,
    IdentifierAlreadyDeclared,
    IdentifierNotDeclared,
    IncorrectParamsCount, // more or less than expected
    MismatchedParamType,
    IdentifierNotCallable,
    PropertyDoesNotExist,
    PropertyNotSupported,
    ExpressionNotCallable,
    ExpressionIndexingNotValid,
    UnaryOperatorInvalidUse,
    BinaryOperatorInvalidOperands,
    MismatchedTypesOnLeftRight,
    NoReturnStatementInFunction,
    InvalidReturnStatement,
    MismatchedReturnType,
}

impl Diagnostics {
    pub fn report(&self) -> Report {
        todo!()
    }
}

impl fmt::Display for Diagnostics {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:?}", self.report())
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid character found during lexing")]
#[diagnostic(code("lexical error"))]
pub struct InvalidCharError {
    #[label = "invalid char"]
    pub char: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("closing symbol not found")]
#[diagnostic(code("lexical error"))]
pub struct NoClosingSymbolError {
    pub expected_symbol: char,
    #[label("closing `{}` not found", self.expected_symbol)]
    pub unclosed_span: SourceSpan,
    #[help]
    pub help: Option<String>,
}
