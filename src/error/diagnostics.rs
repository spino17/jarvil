use super::helper::{range_to_span, IdentifierKind, PropertyKind};
use crate::{lexer::token::Token, parser::helper::format_symbol, types::core::Type};
use miette::{Diagnostic, LabeledSpan, Report, SourceSpan};
use owo_colors::{OwoColorize, Style};
use std::fmt::{self, Display};
use text_size::TextRange;
use thiserror::Error;

#[derive(Clone, Debug)]
pub enum Diagnostics {
    InvalidChar(InvalidCharError),
    NoClosingSymbol(NoClosingSymbolError),
    MissingToken(MissingTokenError),
    InvalidTrailingTokens(InvalidTrailingTokensError),
    IncorrectlyIndentedBlock(IncorrectlyIndentedBlockError),
    InvalidLValue(InvalidLValueError),
    IdentifierAlreadyDeclared(IdentifierAlreadyDeclaredError),
    ConstructorNotFoundInsideStructDeclaration(ConstructorNotFoundInsideStructDeclarationError),
    IdentifierFoundInNonLocals(IdentifierFoundInNonLocalsError),
    IdentifierNotFoundInAnyNamespace(IdentifierNotFoundInAnyNamespaceError),
    IdentifierNotDeclared(IdentifierNotDeclaredError),
    VoidConstructorReturnType(VoidConstructorReturnTypeError),
    NonStructConstructorReturnType(NonStructConstructorReturnTypeError),
    MismatchedConstructorReturnType(MismatchedConstructorReturnTypeError),
    SelfNotFound(SelfNotFoundError),
    VariableReferencedBeforeAssignment(VariableReferencedBeforeAssignmentError),
    RightSideWithVoidTypeNotAllowed(RightSideWithVoidTypeNotAllowedError),
    MoreParamsCount(MoreParamsCountError),
    LessParamsCount(LessParamsCountError),
    MoreThanMaxLimitParamsPassed(MoreThanMaxLimitParamsPassedError),
    MismatchedParamType(MismatchedParamTypeError),
    IdentifierNotCallable(IdentifierNotCallableError),
    StructFieldNotCallable(StructFieldNotCallableError),
    ConstructorNotFoundForType(ConstructorNotFoundForTypeError),
    ClassmethodDoesNotExist(ClassmethodDoesNotExistError),
    PropertyDoesNotExist(PropertyDoesNotExistError),
    PropertyNotSupported(PropertyNotSupportedError),
    ExpressionNotCallable(ExpressionNotCallableError),
    ExpressionIndexingNotValid(ExpressionIndexingNotValidError),
    UnaryOperatorInvalidUse(UnaryOperatorInvalidUseError),
    BinaryOperatorInvalidOperands(BinaryOperatorInvalidOperandsError),
    MismatchedTypesOnLeftRight(MismatchedTypesOnLeftRightError),
    NoValidStatementFoundInsideBlockBody(NoValidStatementFoundInsideBlockBodyError),
    NoReturnStatementInFunction(NoReturnStatementInFunctionError),
    InvalidReturnStatement(InvalidReturnStatementError),
    MismatchedReturnType(MismatchedReturnTypeError),
    NonHashableTypeInIndex(NonHashableTypeInIndexError),
    FieldsNotInitializedInConstructor(FieldsNotInitializedInConstructorError),
    TupleIndexOutOfBound(TupleIndexOutOfBoundError),
    UnresolvedIndexExpressionInTuple(UnresolvedIndexExpressionInTupleError),
    InvalidIndexExpressionForTuple(InvalidIndexExpressionForTupleError),
    ImmutableTypeNotAssignable(ImmutableTypeNotAssignableError),
    SingleSubTypeFoundInTuple(SingleSubTypeFoundInTupleError),
    BuiltinFunctionNameOverlap(BuiltinFunctionNameOverlapError),
    MainFunctionNotFound(MainFunctionNotFoundError),
    MainFunctionWrongType(MainFunctionWrongTypeError),
    ExplicitReturnStatementFoundInConstructorBody(
        ExplicitReturnStatementFoundInConstructorBodyError,
    ),
}

impl Diagnostics {
    pub fn report(&self) -> Report {
        match self {
            Diagnostics::InvalidChar(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::NoClosingSymbol(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::MissingToken(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::InvalidTrailingTokens(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::IncorrectlyIndentedBlock(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::InvalidLValue(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::IdentifierAlreadyDeclared(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::ConstructorNotFoundInsideStructDeclaration(diagonstic) => {
                Report::new(diagonstic.clone())
            }
            Diagnostics::IdentifierNotFoundInAnyNamespace(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::IdentifierFoundInNonLocals(diagonstic) => Report::new(diagonstic.clone()),
            Diagnostics::RightSideWithVoidTypeNotAllowed(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::IdentifierNotDeclared(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::VoidConstructorReturnType(diagonstic) => Report::new(diagonstic.clone()),
            Diagnostics::NonStructConstructorReturnType(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::MismatchedConstructorReturnType(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::SelfNotFound(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::VariableReferencedBeforeAssignment(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::MoreParamsCount(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::LessParamsCount(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::MoreThanMaxLimitParamsPassed(diagonstic) => {
                Report::new(diagonstic.clone())
            }
            Diagnostics::MismatchedParamType(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::IdentifierNotCallable(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::StructFieldNotCallable(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::ConstructorNotFoundForType(diagonstic) => Report::new(diagonstic.clone()),
            Diagnostics::ClassmethodDoesNotExist(diagonstic) => Report::new(diagonstic.clone()),
            Diagnostics::PropertyDoesNotExist(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::PropertyNotSupported(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::ExpressionNotCallable(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::ExpressionIndexingNotValid(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::UnaryOperatorInvalidUse(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::BinaryOperatorInvalidOperands(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::MismatchedTypesOnLeftRight(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::NoReturnStatementInFunction(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::NoValidStatementFoundInsideBlockBody(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::InvalidReturnStatement(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::MismatchedReturnType(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::NonHashableTypeInIndex(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::FieldsNotInitializedInConstructor(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::TupleIndexOutOfBound(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::UnresolvedIndexExpressionInTuple(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::InvalidIndexExpressionForTuple(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::ImmutableTypeNotAssignable(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::SingleSubTypeFoundInTuple(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::BuiltinFunctionNameOverlap(diagnotic) => Report::new(diagnotic.clone()),
            Diagnostics::MainFunctionNotFound(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::MainFunctionWrongType(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::ExplicitReturnStatementFoundInConstructorBody(diagnostic) => {
                Report::new(diagnostic.clone())
            }
        }
    }
}

impl fmt::Display for Diagnostics {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:?}", self.report())
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid character found")]
#[diagnostic(code("LexicalError"))]
pub struct InvalidCharError {
    #[label("invalid char")]
    pub span: SourceSpan,
}

impl InvalidCharError {
    pub fn new(invalid_token: &Token) -> Self {
        InvalidCharError {
            span: (invalid_token.start_index(), invalid_token.len()).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error(
    r#"opening symbols `/*`, `'` and `"` should have closing parts `*/`, `'` and `"` respectively"#
)]
#[diagnostic(code("LexicalError"))]
pub struct NoClosingSymbolError {
    pub expected_symbol: String,
    #[label("no closing `{}` found", self.expected_symbol)]
    pub unclosed_span: SourceSpan,
}

impl NoClosingSymbolError {
    pub fn new(expected_symbol: String, token: &Token) -> Self {
        NoClosingSymbolError {
            expected_symbol,
            unclosed_span: (token.start_index(), token.len()).into(),
        }
    }
}

#[derive(Debug, Error, Clone)]
#[error("expected token missing")]
pub struct MissingTokenError {
    pub expected_symbols: Vec<&'static str>,
    pub received_token: String,
    pub start_index: usize,
    pub len: usize,
}

impl MissingTokenError {
    pub fn new(expected_symbols: &[&'static str], received_token: &Token) -> Self {
        let len = if received_token.is_eq("\n") {
            received_token.len() - 1
        } else {
            received_token.len()
        };
        MissingTokenError {
            expected_symbols: expected_symbols.to_vec(),
            received_token: received_token.name(),
            start_index: received_token.start_index(),
            len,
        }
    }
}

impl Diagnostic for MissingTokenError {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        return Some(Box::new("SyntaxError"));
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let err_message = if self.expected_symbols.len() == 1 {
            format!(
                "expected `{}`, got `{}`",
                format_symbol(self.expected_symbols[0]),
                self.received_token
            )
        } else {
            let mut err_str = "expected ".to_string();
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
                self.received_token
            ));
            err_str
        };
        let span_vec = vec![LabeledSpan::new(
            Some(err_message),
            self.start_index,
            self.len,
        )];
        return Some(Box::new(span_vec.into_iter()));
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid trailing sequence of tokens")]
#[diagnostic(code("SyntaxError"))]
pub struct InvalidTrailingTokensError {
    #[label("tokens will be skipped for any further analysis")]
    pub span: SourceSpan,
}

impl InvalidTrailingTokensError {
    pub fn new(start_index: usize, end_index: usize) -> Self {
        InvalidTrailingTokensError {
            span: (start_index, end_index - start_index).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("incorrectly indented statement")]
#[diagnostic(code("SyntaxError"))]
pub struct IncorrectlyIndentedBlockError {
    pub expected_indent: i64,
    pub received_indent: i64,
    #[label("expected an indented statement with `{}` spaces, got `{}` spaces", self.expected_indent, self.received_indent)]
    pub span: SourceSpan,
}

impl IncorrectlyIndentedBlockError {
    pub fn new(expected_indent: i64, received_indent: i64, range: TextRange) -> Self {
        IncorrectlyIndentedBlockError {
            expected_indent,
            received_indent,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid l-value")]
#[diagnostic(code("SyntaxError"))]
pub struct InvalidLValueError {
    #[label("expression cannot be assigned a value")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // any value derived from a function call is not assignable
}

impl InvalidLValueError {
    pub fn new(range: TextRange) -> Self {
        InvalidLValueError {
            span: range_to_span(range).into(),
            help: Some(
                "any value derived from the output of a function call is not assignable"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("void constructor return-type")]
#[diagnostic(code("SemanticError"))]
pub struct VoidConstructorReturnTypeError {
    #[label("constructor cannot have a void return-type")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // any value derived from a function call is not assignable
}

impl VoidConstructorReturnTypeError {
    pub fn new(range: TextRange) -> Self {
        VoidConstructorReturnTypeError {
            span: range_to_span(range).into(),
            help: Some(
                "constructor should have return-type same as the struct it is defined in"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched constructor return-type")]
#[diagnostic(code("SemanticError"))]
pub struct MismatchedConstructorReturnTypeError {
    pub struct_name: String,
    #[label("constructor return-type should be `{}`", self.struct_name)]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // any value derived from a function call is not assignable
}

impl MismatchedConstructorReturnTypeError {
    pub fn new(struct_name: String, range: TextRange) -> Self {
        MismatchedConstructorReturnTypeError {
            struct_name,
            span: range_to_span(range).into(),
            help: Some(
                "constructor should have return-type same as the struct it is defined in"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("non-struct constructor return-type")]
#[diagnostic(code("SemanticError"))]
pub struct NonStructConstructorReturnTypeError {
    #[label("constructor return-type should be a struct")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // any value derived from a function call is not assignable
}

impl NonStructConstructorReturnTypeError {
    pub fn new(range: TextRange) -> Self {
        NonStructConstructorReturnTypeError {
            span: range_to_span(range).into(),
            help: Some(
                "constructor should have return-type same as the struct it is defined in"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("{} is redeclared", self.identifier_kind)]
#[diagnostic(code("SemanticError"))]
pub struct IdentifierAlreadyDeclaredError {
    pub identifier_kind: IdentifierKind,
    pub name: String,
    #[label("previous declaration of `{}` is here", self.name)]
    pub previous_decl_span: SourceSpan,
    #[label("`{}` is redeclared here", self.name)]
    pub redecl_span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl IdentifierAlreadyDeclaredError {
    pub fn new(
        identifier_kind: IdentifierKind,
        name: String,
        previous_decl_range: TextRange,
        redecl_range: TextRange,
    ) -> Self {
        let help_str = match identifier_kind {
            IdentifierKind::Variable | IdentifierKind::Function => {
                format!(
                    "{}s are not allowed to be redeclared inside the same scope",
                    identifier_kind
                )
            }
            IdentifierKind::Argument => {
                format!(
                    "{}s are not allowed to be redeclared in the same function defintion",
                    identifier_kind
                )
            }
            IdentifierKind::Field => {
                format!("all fields of struct should have distinct names")
            }
            IdentifierKind::Type => {
                format!(
                    "{}s are not allowed to be redeclared inside the complete scope chain",
                    identifier_kind
                )
            }
            IdentifierKind::Method => {
                format!("all methods of struct should have distinct names")
            }
            IdentifierKind::Constructor => {
                format!("constructor is not allowed to be redeclared")
            }
        };
        IdentifierAlreadyDeclaredError {
            identifier_kind,
            name,
            previous_decl_span: range_to_span(previous_decl_range).into(),
            redecl_span: range_to_span(redecl_range).into(),
            help: Some(help_str.style(Style::new().yellow()).to_string()),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("builtin function name overlap")]
#[diagnostic(code("SemanticError"))]
pub struct BuiltinFunctionNameOverlapError {
    #[label("there is a builtin function with same name")]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl BuiltinFunctionNameOverlapError {
    pub fn new(range: TextRange) -> Self {
        BuiltinFunctionNameOverlapError {
            span: range_to_span(range).into(),
            help: Some(
                "functions with same name as builtin functions cannot be declared in global scope"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("{} is not declared in the scope", self.identifier_kind)]
#[diagnostic(code("SemanticError"))]
pub struct IdentifierNotDeclaredError {
    pub identifier_kind: IdentifierKind,
    #[label("not found in the scope")]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl IdentifierNotDeclaredError {
    pub fn new(identifier_kind: IdentifierKind, range: TextRange) -> Self {
        IdentifierNotDeclaredError {
            identifier_kind,
            span: range_to_span(range).into(),
            help: Some(
                "identifiers are declared in one of the three namespaces: variables, functions and types"
                .to_string()
                .style(Style::new().yellow())
                .to_string()
            )
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("constructor not found")]
#[diagnostic(code("SemanticError"))]
pub struct ConstructorNotFoundInsideStructDeclarationError {
    #[label("constructor definition not found inside struct declaration")]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl ConstructorNotFoundInsideStructDeclarationError {
    pub fn new(range: TextRange) -> Self {
        ConstructorNotFoundInsideStructDeclarationError {
            span: range_to_span(range).into(),
            help: Some(
                "struct declaration should always have constructor definition with signature: `def __init__([<params>]): <block>"
                .to_string()
                .style(Style::new().yellow())
                .to_string()
            )
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("fields not initialized in constructor")]
#[diagnostic(code("SemanticError"))]
pub struct FieldsNotInitializedInConstructorError {
    pub message: String,
    #[label("fields {} not initialized inside the constructor", self.message)]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl FieldsNotInitializedInConstructorError {
    pub fn new(message: String, range: TextRange) -> Self {
        FieldsNotInitializedInConstructorError {
            message,
            span: range_to_span(range).into(),
            help: Some(
                "all fields of struct should be initialized through assignment inside the constructor"
                .to_string()
                .style(Style::new().yellow())
                .to_string()
            )
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("`self` is not declared in the scope")]
#[diagnostic(code("SemanticError"))]
pub struct SelfNotFoundError {
    #[label("not found in the scope")]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl SelfNotFoundError {
    pub fn new(range: TextRange) -> Self {
        SelfNotFoundError {
            span: range_to_span(range).into(),
            help: Some(
                "`self` should only be used inside a class scope"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("{} found in non-locals", self.identifier_kind)]
#[diagnostic(code("SemanticError"))]
pub struct IdentifierFoundInNonLocalsError {
    pub identifier_kind: IdentifierKind,
    #[label("identifier with same name is resolved in non-local scope")]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl IdentifierFoundInNonLocalsError {
    pub fn new(identifier_kind: IdentifierKind, range: TextRange) -> Self {
        IdentifierFoundInNonLocalsError {
            identifier_kind,
            span: range_to_span(range).into(),
            help: Some(
                "variables and functions are not allowed to be declared in the scope where there exist a reference with the same name to a non-local declaration"
                .to_string()
                .style(Style::new().yellow())
                .to_string()
            )
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("variable `{}` referenced before assignment", self.variable_name)]
#[diagnostic(code("SemanticError"))]
pub struct VariableReferencedBeforeAssignmentError {
    pub variable_name: String,
    #[label("variable declared here")]
    pub decl_span: SourceSpan,
    #[label("same variable used within the declaration statement")]
    pub usage_span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl VariableReferencedBeforeAssignmentError {
    pub fn new(variable_name: String, decl_range: TextRange, usage_range: TextRange) -> Self {
        VariableReferencedBeforeAssignmentError {
            variable_name,
            decl_span: range_to_span(decl_range).into(),
            usage_span: range_to_span(usage_range).into(),
            help: Some(
                "variables are not allowed to be referenced inside the their own declaration statement"
                .to_string()
                .style(Style::new().yellow())
                .to_string()
            )
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("callable is not declared in any namespace")]
#[diagnostic(code("SemanticError"))]
pub struct IdentifierNotFoundInAnyNamespaceError {
    #[label("not found in the scope")]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl IdentifierNotFoundInAnyNamespaceError {
    pub fn new(range: TextRange) -> Self {
        IdentifierNotFoundInAnyNamespaceError {
            span: range_to_span(range).into(),
            help: Some(
                "identifiers are declared in one of the three namespaces: variables, functions and types\ncallable identifier are resolved in the following order of namespace:\nfunction => type => variable"
                .to_string()
                .style(Style::new().yellow())
                .to_string()
            )
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("right side with `()` type is not allowed")]
#[diagnostic(code("SemanticError"))]
pub struct RightSideWithVoidTypeNotAllowedError {
    #[label("has type `()`")]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl RightSideWithVoidTypeNotAllowedError {
    pub fn new(range: TextRange) -> Self {
        RightSideWithVoidTypeNotAllowedError {
            span: range_to_span(range).into(),
            help: Some(
                "variable declaration or assignment with type `void` is not allowed"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("assignment to immutable type")]
#[diagnostic(code("SemanticError"))]
pub struct ImmutableTypeNotAssignableError {
    pub ty: String,
    #[label("type `{}` is not assignable", self.ty)]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl ImmutableTypeNotAssignableError {
    pub fn new(ty: &Type, range: TextRange) -> Self {
        ImmutableTypeNotAssignableError {
            ty: ty.to_string(),
            span: range_to_span(range).into(),
            help: Some(
                "`str` and `tuple` are immutable types which are not assignable"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched expected and passed number of parameters")]
#[diagnostic(code("TypeCheckError"))]
pub struct MoreParamsCountError {
    expected_params_count: usize,
    #[label("expected {} parameters, got more than that", self.expected_params_count)]
    pub span: SourceSpan,
}

impl MoreParamsCountError {
    pub fn new(expected_params_count: usize, range: TextRange) -> Self {
        MoreParamsCountError {
            expected_params_count,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched expected and passed number of parameters")]
#[diagnostic(code("TypeCheckError"))]
pub struct LessParamsCountError {
    expected_params_count: usize,
    received_params_count: usize,
    #[label("expected {} parameters, got {}", self.expected_params_count, self.received_params_count)]
    pub span: SourceSpan,
}

impl LessParamsCountError {
    pub fn new(
        expected_params_count: usize,
        received_params_count: usize,
        range: TextRange,
    ) -> Self {
        LessParamsCountError {
            expected_params_count,
            received_params_count,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("more than {} parameters passed in the function", self.max_limit)]
#[diagnostic(code("SemanticError"))]
pub struct MoreThanMaxLimitParamsPassedError {
    params_count: usize,
    max_limit: usize,
    #[label("max. limit for parameters to a function is {}, got {}", self.max_limit, self.params_count)]
    pub span: SourceSpan,
}

impl MoreThanMaxLimitParamsPassedError {
    pub fn new(params_count: usize, max_limit: usize, range: TextRange) -> Self {
        MoreThanMaxLimitParamsPassedError {
            params_count,
            max_limit,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Debug, Error, Clone)]
#[error("mismatched types")]
pub struct MismatchedParamTypeError {
    params_vec: Vec<(String, String, usize, TextRange)>, // (expected_type, received_type, index_of_param, span)
}
impl MismatchedParamTypeError {
    pub fn new(params_vec: Vec<(String, String, usize, TextRange)>) -> Self {
        MismatchedParamTypeError { params_vec }
    }
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
        return Some(Box::new("TypeCheckError"));
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("calling an uncallable")]
#[diagnostic(code("TypeCheckError"))]
pub struct IdentifierNotCallableError {
    pub ty: String,
    #[label("variable with type `{}` is not callable", self.ty)]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl IdentifierNotCallableError {
    pub fn new(ty: String, range: TextRange) -> Self {
        IdentifierNotCallableError {
            ty,
            span: range_to_span(range).into(),
            help: Some(
                "only variables with `lambda` types are callable"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("constructor not found")]
#[diagnostic(code("TypeCheckError"))]
pub struct ConstructorNotFoundForTypeError {
    pub ty: String,
    #[label("type `{}` does not have a constructor", self.ty)]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl ConstructorNotFoundForTypeError {
    pub fn new(ty_str: String, range: TextRange) -> Self {
        ConstructorNotFoundForTypeError {
            ty: ty_str,
            span: range_to_span(range).into(),
            help: Some(
                "only struct type is allowed to call constructor via self name"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("property does not exist")]
#[diagnostic(code("TypeCheckError"))]
pub struct ClassmethodDoesNotExistError {
    pub struct_name: String,
    #[label("no classmethod with this name exist for struct `{}`", self.struct_name)]
    pub span: SourceSpan,
}

impl ClassmethodDoesNotExistError {
    pub fn new(struct_name: String, range: TextRange) -> Self {
        ClassmethodDoesNotExistError {
            struct_name,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("property does not exist")]
#[diagnostic(code("TypeCheckError"))]
pub struct PropertyDoesNotExistError {
    pub property_kind: PropertyKind,
    pub ty: String,
    #[label("no {} with this name exist for the expression", self.property_kind)]
    pub property_span: SourceSpan,
    #[label("expression has type `{}`", self.ty)]
    pub expr_span: SourceSpan,
}

impl PropertyDoesNotExistError {
    pub fn new(
        property_kind: PropertyKind,
        ty: String,
        property_range: TextRange,
        expr_range: TextRange,
    ) -> Self {
        PropertyDoesNotExistError {
            property_kind,
            ty,
            property_span: range_to_span(property_range).into(),
            expr_span: range_to_span(expr_range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("struct field not callable")]
#[diagnostic(code("TypeCheckError"))]
pub struct StructFieldNotCallableError {
    pub ty: String,
    #[label("field with type `{}` is not callable", self.ty)]
    pub field_span: SourceSpan,
}

impl StructFieldNotCallableError {
    pub fn new(ty: String, field_span: TextRange) -> Self {
        StructFieldNotCallableError {
            ty,
            field_span: range_to_span(field_span).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("property not supported")]
#[diagnostic(code("TypeCheckError"))]
pub struct PropertyNotSupportedError {
    pub property_name: String,
    #[label("{} are only supported for `struct` types", self.property_name)]
    pub span: SourceSpan,
}

impl PropertyNotSupportedError {
    pub fn new(property_name: String, range: TextRange) -> Self {
        PropertyNotSupportedError {
            property_name,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("calling an uncallable")]
#[diagnostic(code("TypeCheckError"))]
pub struct ExpressionNotCallableError {
    #[label("expression is not callable")]
    pub span: SourceSpan,
}

impl ExpressionNotCallableError {
    pub fn new(range: TextRange) -> Self {
        ExpressionNotCallableError {
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid indexing")]
#[diagnostic(code("TypeCheckError"))]
pub struct ExpressionIndexingNotValidError {
    pub expr_type: String,
    pub index_type: String,
    #[label("expression has type `{}`", self.expr_type)]
    pub expr_span: SourceSpan,
    #[label("expression is not indexable with value of type `{}`", self.index_type)]
    pub index_span: SourceSpan,
}

impl ExpressionIndexingNotValidError {
    pub fn new(
        expr_type: String,
        index_type: String,
        expr_range: TextRange,
        index_range: TextRange,
    ) -> Self {
        ExpressionIndexingNotValidError {
            expr_type,
            index_type,
            expr_span: range_to_span(expr_range).into(),
            index_span: range_to_span(index_range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("tuple index out of bound")]
#[diagnostic(code("TypeCheckError"))]
pub struct TupleIndexOutOfBoundError {
    pub tuple_len: usize,
    #[label("index out of bounds `(0, {})` or `(-1, -{})`", self.tuple_len - 1, self.tuple_len)]
    pub index_span: SourceSpan,
}

impl TupleIndexOutOfBoundError {
    pub fn new(tuple_len: usize, index_span: TextRange) -> Self {
        TupleIndexOutOfBoundError {
            tuple_len,
            index_span: range_to_span(index_span).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid indexing")]
#[diagnostic(code("TypeCheckError"))]
pub struct InvalidIndexExpressionForTupleError {
    #[label("invalid expression for indexing tuple")]
    pub index_span: SourceSpan,
}

impl InvalidIndexExpressionForTupleError {
    pub fn new(index_span: TextRange) -> Self {
        InvalidIndexExpressionForTupleError {
            index_span: range_to_span(index_span).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("unresolved index expression in tuple")]
#[diagnostic(code("TypeCheckError"))]
pub struct UnresolvedIndexExpressionInTupleError {
    #[label("expression does not resolve to a valid integer value for indexing tuple")]
    pub index_span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl UnresolvedIndexExpressionInTupleError {
    pub fn new(index_span: TextRange) -> Self {
        UnresolvedIndexExpressionInTupleError {
            index_span: range_to_span(index_span).into(),
            help: Some(
                "tuple index should be a fixed integer value"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid unary operand")]
#[diagnostic(code("TypeCheckError"))]
pub struct UnaryOperatorInvalidUseError {
    pub ty: String,
    pub valid_operand_type: String,
    pub operator: String,
    #[label("operand has type `{}`", self.ty)]
    pub operand_span: SourceSpan,
    #[label("operator cannot be applied on operand")]
    pub operator_span: SourceSpan,
    #[help]
    pub help: Option<String>, // unary operator {} is valid only for {} operands
}

impl UnaryOperatorInvalidUseError {
    pub fn new(
        ty: String,
        valid_operand_type: &'static str,
        operator: &'static str,
        operand_range: TextRange,
        operator_range: TextRange,
    ) -> Self {
        let help_str = format!(
            "unary operator {} is valid only for {} operands",
            operator, valid_operand_type
        );
        UnaryOperatorInvalidUseError {
            ty,
            valid_operand_type: valid_operand_type.to_string(),
            operator: operator.to_string(),
            operand_span: range_to_span(operand_range).into(),
            operator_span: range_to_span(operator_range).into(),
            help: Some(help_str.style(Style::new().yellow()).to_string()),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid binary operands")]
#[diagnostic(code("TypeCheckError"))]
pub struct BinaryOperatorInvalidOperandsError {
    pub left_type: String,
    pub right_type: String,
    #[label("operand has type `{}`", self.left_type)]
    pub left_expr_span: SourceSpan,
    #[label("operand has type `{}`", self.right_type)]
    pub right_expr_span: SourceSpan,
    #[label("binary operator cannot be applied on operands")]
    pub operator_span: SourceSpan,
    #[help]
    pub help: Option<String>, // binary operator {} is valid only for {} operands
}

impl BinaryOperatorInvalidOperandsError {
    pub fn new(
        left_type: String,
        right_type: String,
        left_range: TextRange,
        right_range: TextRange,
        operator_range: TextRange,
    ) -> Self {
        // TODO - construct dynamic help message
        BinaryOperatorInvalidOperandsError {
            left_type,
            right_type,
            left_expr_span: range_to_span(left_range).into(),
            right_expr_span: range_to_span(right_range).into(),
            operator_span: range_to_span(operator_range).into(),
            help: None,
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched types")]
#[diagnostic(code("TypeCheckError"))]
pub struct MismatchedTypesOnLeftRightError {
    pub left_type: String,
    pub right_type: String,
    #[label("has type `{}`", self.left_type)]
    pub left_span: SourceSpan,
    #[label("has type `{}`", self.right_type)]
    pub right_span: SourceSpan,
    #[help]
    pub help: Option<String>, // types on both sides should be same
}

impl MismatchedTypesOnLeftRightError {
    pub fn new(
        left_type: String,
        right_type: String,
        left_range: TextRange,
        right_range: TextRange,
    ) -> Self {
        MismatchedTypesOnLeftRightError {
            left_type,
            right_type,
            left_span: range_to_span(left_range).into(),
            right_span: range_to_span(right_range).into(),
            help: Some(
                "types on both sides should be same"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("no return statement found")]
#[diagnostic(code("TypeCheckError"))]
pub struct NoReturnStatementInFunctionError {
    #[label("function body has no `return` statement")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl NoReturnStatementInFunctionError {
    pub fn new(range: TextRange) -> Self {
        NoReturnStatementInFunctionError {
            span: range_to_span(range).into(),
            help: Some(
                "function with a return value should have atleast one `return` statement inside the top-level block"
                .to_string()
                .style(Style::new().yellow())
                .to_string()
            )
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("explicit return statement found in constructor body")]
#[diagnostic(code("TypeCheckError"))]
pub struct ExplicitReturnStatementFoundInConstructorBodyError {
    #[label("explicit return statement")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl ExplicitReturnStatementFoundInConstructorBodyError {
    pub fn new(range: TextRange) -> Self {
        ExplicitReturnStatementFoundInConstructorBodyError {
            span: range_to_span(range).into(),
            help: Some(
                "constructor body should have no explicit return statements"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("no valid statement found inside the block")]
#[diagnostic(code("SyntaxError"))]
pub struct NoValidStatementFoundInsideBlockBodyError {
    #[label("expected atleast one statement inside the block")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl NoValidStatementFoundInsideBlockBodyError {
    pub fn new(range: TextRange) -> Self {
        NoValidStatementFoundInsideBlockBodyError {
            span: range_to_span(range).into(),
            help: Some(
                "block body should have atleast one valid statement"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid return statement")]
#[diagnostic(code("TypeCheckError"))]
pub struct InvalidReturnStatementError {
    #[label("invalid `return` statement")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl InvalidReturnStatementError {
    pub fn new(range: TextRange) -> Self {
        InvalidReturnStatementError {
            span: range_to_span(range).into(),
            help: Some(
                "`return` statement should be used inside function body"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched types")]
#[diagnostic(code("TypeCheckError"))]
pub struct MismatchedReturnTypeError {
    pub expected_type: String,
    pub received_type: String,
    #[label("expected return value with type `{}`, got `{}`", self.expected_type, self.received_type)]
    pub span: SourceSpan,
}

impl MismatchedReturnTypeError {
    pub fn new(expected_type: String, received_type: String, range: TextRange) -> Self {
        MismatchedReturnTypeError {
            expected_type: expected_type,
            received_type: received_type,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("non-hashable type found in hashmap index")]
#[diagnostic(code("SemanticError"))]
pub struct NonHashableTypeInIndexError {
    #[label("non-hashable type")]
    pub index_span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl NonHashableTypeInIndexError {
    pub fn new(index_span: TextRange) -> Self {
        NonHashableTypeInIndexError {
            index_span: range_to_span(index_span).into(),
            help: Some(
                "only `int`, `float`, `str` and `tuple` with hashable sub-types are hashable types"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("single sub-type in tuple")]
#[diagnostic(code("SemanticError"))]
pub struct SingleSubTypeFoundInTupleError {
    #[label("only one sub-type in tuple")]
    pub index_span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl SingleSubTypeFoundInTupleError {
    pub fn new(index_span: TextRange) -> Self {
        SingleSubTypeFoundInTupleError {
            index_span: range_to_span(index_span).into(),
            help: Some(
                "tuple should have more than one sub-type"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("`main` function not found")]
#[diagnostic(code("SemanticError"))]
pub struct MainFunctionNotFoundError {
    #[help]
    help: Option<String>,
}

impl MainFunctionNotFoundError {
    pub fn new() -> Self {
        MainFunctionNotFoundError {
            help: Some(
                "the entry point to the code is through `main` function"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("`main` function has wrong type")]
#[diagnostic(code("SemanticError"))]
pub struct MainFunctionWrongTypeError {
    #[label("wrong structure of params and return type")]
    pub index_span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl MainFunctionWrongTypeError {
    pub fn new(span: TextRange) -> Self {
        MainFunctionWrongTypeError {
            index_span: range_to_span(span).into(),
            help: Some(
                "`main` function should have no params and no return type"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}
