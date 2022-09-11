use super::helper::{IdentifierKind, PropertyKind};
use crate::{lexer::token::Token, parser::helper::format_symbol, types::core::Type};
use miette::{Diagnostic, LabeledSpan, Report, SourceSpan};
use std::fmt::{self, Display};
use text_size::TextRange;
use thiserror::Error;

#[derive(Clone, Debug)]
pub enum Diagnostics<'a> {
    InvalidChar(InvalidCharError),
    NoClosingSymbol(NoClosingSymbolError),
    MissingToken(MissingTokenError<'a>),
    InvalidTrailingTokens(InvalidTrailingTokensError),
    IncorrectlyIndentedBlock(IncorrectlyIndentedBlockError),
    InvalidLValue(InvalidLValueError),
    IdentifierAlreadyDeclared(IdentifierAlreadyDeclaredError),
    IdentifierNotDeclared(IdentifierNotDeclaredError),
    MoreParamsCount(MoreParamsCountError),
    LessParamsCount(LessParamsCountError),
    MismatchedParamType(MismatchedParamTypeError),
    IdentifierNotCallable(IdentifierNotCallableError),
    PropertyDoesNotExist(PropertyDoesNotExistError),
    PropertyNotSupported(PropertyNotSupportedError),
    ExpressionNotCallable(ExpressionNotCallableError),
    ExpressionIndexingNotValid(ExpressionIndexingNotValidError),
    UnaryOperatorInvalidUse(UnaryOperatorInvalidUseError),
    BinaryOperatorInvalidOperands(BinaryOperatorInvalidOperandsError),
    MismatchedTypesOnLeftRight(MismatchedTypesOnLeftRightError),
    NoReturnStatementInFunction(NoReturnStatementInFunctionError),
    InvalidReturnStatement(InvalidReturnStatementError),
    MismatchedReturnType(MismatchedReturnTypeError),
}

impl<'a> Diagnostics<'a> {
    pub fn report(&self) -> Report {
        todo!()
    }
}

impl<'a> fmt::Display for Diagnostics<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:?}", self.report())
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid character `{}` found during lexing", self.invalid_char)]
#[diagnostic(code("lexical error"))]
pub struct InvalidCharError {
    pub invalid_char: char,
    #[label("invalid char")]
    pub span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error(
    r#"opening symbols `/*`, `'` and `"` should have closing parts `*/`, `'` and `"` respectively"#
)]
#[diagnostic(code("lexical error"))]
pub struct NoClosingSymbolError {
    pub expected_symbol: char,
    #[label("closing `{}` not found", self.expected_symbol)]
    pub unclosed_span: SourceSpan,
}

#[derive(Debug, Error, Clone)]
#[error("expected token missing")]
pub struct MissingTokenError<'a> {
    pub expected_symbols: &'a [&'static str],
    pub received_token: Token,
    pub message: String,
}
impl<'a> Diagnostic for MissingTokenError<'a> {
    fn code<'b>(&'b self) -> Option<Box<dyn Display + 'b>> {
        return Some(Box::new("syntax error"));
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let err_message = if self.expected_symbols.len() == 1 {
            format!(
                "expected `{}`, got `{}`",
                format_symbol(self.expected_symbols[0]),
                self.received_token.name()
            )
        } else {
            let mut err_str = String::from("expected ");
            let mut flag = false;
            let symbols_len = self.expected_symbols.len();
            for index in 0..symbols_len - 1 {
                if flag {
                    err_str.push_str(", ");
                }
                err_str.push_str(&format!(
                    "`{}`",
                    format_symbol(self.expected_symbols[index])
                ));
                flag = true;
            }
            err_str.push_str(&format!(
                " or `{}`, got `{}`",
                format_symbol(self.expected_symbols[symbols_len - 1]),
                self.received_token.name()
            ));
            err_str
        };
        let start_index = self.received_token.start_index();
        let end_index = self.received_token.end_index();
        let len = end_index - start_index;
        let span_vec = vec![LabeledSpan::new(Some(err_message), start_index, len)];
        return Some(Box::new(span_vec.into_iter()));
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid trailing sequence of tokens")]
#[diagnostic(code("syntax error"))]
pub struct InvalidTrailingTokensError {
    #[label("tokens will be skipped for any further analysis")]
    pub span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("incorrectly indented block")]
#[diagnostic(code("syntax error"))]
pub struct IncorrectlyIndentedBlockError {
    pub expected_indent: i64,
    pub received_indent: i64,
    #[label("expected an indented block with `{}` spaces, got `{}` spaces", self.expected_indent, self.received_indent)]
    pub span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid l-value")]
#[diagnostic(code("syntax error"))]
pub struct InvalidLValueError {
    #[label("expression cannot be assigned a value")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // any value derived from a function call is not assignable
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("{} is redeclared", self.identifier_kind)]
#[diagnostic(code("semantic error (resolving phase)"))]
pub struct IdentifierAlreadyDeclaredError {
    pub identifier_kind: IdentifierKind,
    pub name: String,
    #[label("previous declaration of `{}` is here", self.name)]
    pub previous_def_span: SourceSpan,
    #[label("`{}` is redeclared here", self.name)]
    pub redef_span: SourceSpan,
    #[help]
    pub help: Option<String>, // variables and functions are not allowed to be redeclared inside the same block
                              // whereas types are not allowed to be redeclared inside the complete scope
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("{} is not declared in the scope", self.identifier_kind)]
#[diagnostic(code("semantic error (resolving phase)"))]
pub struct IdentifierNotDeclaredError {
    pub identifier_kind: IdentifierKind,
    pub name: String,
    #[label("`{}` not found in the scope", self.name)]
    pub span: SourceSpan,
    #[help]
    help: Option<String>, // identifiers are declared into one of the three namespaces: variables, functions and types
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched expected and passed number of parameters")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct MoreParamsCountError {
    expected_params_count: usize,
    #[label("expected `{}` arguments, got more than that", self.expected_params_count)]
    pub span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched expected and passed number of parameters")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct LessParamsCountError {
    expected_params_count: usize,
    received_params_count: usize,
    #[label("expected {} arguments, got {}", self.expected_params_count, self.received_params_count)]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Clone)]
#[error("mismatched types")]
pub struct MismatchedParamTypeError {
    params_vec: Vec<(Type, Type, usize, TextRange)>, // (expected_type, received_type, index_of_param, span)
}
impl Diagnostic for MismatchedParamTypeError {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        let mut span_vec: Vec<LabeledSpan> = vec![];
        for factor in &self.params_vec {
            let err_message = format!("expected type `{}`, got `{}`", factor.0, factor.1);
            let start_index: usize = factor.3.start().into();
            let len: usize = (factor.3.end() - factor.3.start()).into();
            span_vec.push(miette::LabeledSpan::new(
                Some(err_message),
                start_index,
                len,
            ));
        }
        return Some(Box::new(span_vec.into_iter()));
    }

    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        return Some(Box::new("semantic error (type-checking phase)"));
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("calling an uncallable")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct IdentifierNotCallableError {
    pub name: String,
    pub ty: Type,
    #[label("variable `{}` with type `{}` is not callable", self.name, self.ty)]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // only variables with lambda types are callable
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("property does not exist")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct PropertyDoesNotExistError {
    pub property_kind: PropertyKind,
    pub property_name: String,
    pub ty: Type,
    #[label("no {} named `{}` exist for expression with type `{}`", self.property_kind, self.property_name, self.ty)]
    pub property_span: SourceSpan,
    #[label("expression has type `{}`", self.ty)]
    pub expr_span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("property not supported")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct PropertyNotSupportedError {
    pub property_name: String,
    #[label("{} are only supported for `struct` types", self.property_name)]
    pub span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("calling an uncallable")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct ExpressionNotCallableError {
    #[label("expression is not callable")]
    pub span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid indexing")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct ExpressionIndexingNotValidError {
    pub expr_type: Type,
    pub index_type: Type,
    #[label("expression has type `{}`", self.expr_type)]
    pub expr_span: SourceSpan,
    #[label("expression with type `{}` is not indexable with value of type `{}`", self.expr_type, self.index_type)]
    pub index_span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid unary operand")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct UnaryOperatorInvalidUseError {
    pub ty: Type,
    pub valid_operand_type: String,
    #[label("operand has type `{}`", self.ty)]
    pub operand_span: SourceSpan,
    #[label("operator cannot be applied on operand")]
    pub operator_span: SourceSpan,
    #[help]
    pub help: Option<String>, // unary operator {} is valid only for {} operands
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid binary operands")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct BinaryOperatorInvalidOperandsError {
    pub left_type: Type,
    pub right_type: Type,
    #[label("operand has type `{}`", self.left_type)]
    pub left_expr_span: SourceSpan,
    #[label("operand has type `{}`", self.right_type)]
    pub right_expr_span: SourceSpan,
    #[label("binary operator cannot be applied on operands")]
    pub operator_span: SourceSpan,
    #[help]
    pub help: Option<String>, // binary operator {} is valid only for {} operands
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched types")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct MismatchedTypesOnLeftRightError {
    pub left_type: Type,
    pub right_type: Type,
    #[label("has type `{}`", self.left_type)]
    pub left_span: SourceSpan,
    #[label("has type `{}`", self.right_type)]
    pub right_span: SourceSpan,
    #[help]
    pub help: Option<String>, // types on both sides should be same
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("no return statement found")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct NoReturnStatementInFunctionError {
    #[label("function body has no `return` statement")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // function with a return value should have atleast one `return` statement inside the top-level block
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid return statement")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct InvalidReturnStatementError {
    #[label("invalid `return` statement")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // `return` statement should be used inside function body
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched types")]
#[diagnostic(code("semantic error (type-checking phase)"))]
pub struct MismatchedReturnTypeError {
    pub expected_type: Type,
    pub received_type: Type,
    #[label("expected return value type `{}`, got `{}`", self.expected_type, self.received_type)]
    pub span: SourceSpan,
}
