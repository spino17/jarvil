use super::helper::{range_to_span, IdentifierKind, PropertyKind};
use crate::types::core::AbstractType;
use crate::{
    core::string_interner::{Interner, StrId},
    lexer::token::Token,
    parser::helper::format_symbol,
    scope::symbol::interfaces::PartialConcreteInterfaceMethodsCheckError,
    types::core::Type,
};
use miette::{Diagnostic, LabeledSpan, Report, SourceSpan};
use owo_colors::{OwoColorize, Style};
use std::{
    fmt::{self, Display},
    vec,
};
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
    IdentifierNotFoundInAnyNamespace(IdentifierNotFoundInAnyNamespaceError),
    IdentifierNotDeclared(IdentifierNotDeclaredError),
    InvalidLoopControlFlowStatementFound(InvalidLoopControlFlowStatementFoundError),
    NonVoidConstructorReturnType(NonVoidConstructorReturnTypeError),
    NonStructConstructorReturnType(NonStructConstructorReturnTypeError),
    MismatchedConstructorReturnType(MismatchedConstructorReturnTypeError),
    SelfNotFound(SelfNotFoundError),
    GenericTypeResolvedToOutsideScope(GenericTypeResolvedToOutsideScopeError),
    IdentifierUsedBeforeInitialized(IdentifierUsedBeforeInitializedError),
    RightSideWithVoidTypeNotAllowed(RightSideWithVoidTypeNotAllowedError),
    MoreParamsCount(MoreParamsCountError),
    LessParamsCount(LessParamsCountError),
    MismatchedParamType(MismatchedParamTypeError),
    NotAllConcreteTypesInferred(NotAllConcreteTypesInferredError),
    TypeInferenceFailed(TypeInferenceFailedError),
    InferredTypesNotBoundedByInterfaces(InferredTypesNotBoundedByInterfacesError),
    IdentifierNotCallable(IdentifierNotCallableError),
    FieldNotCallable(FieldNotCallableError),
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
    MainFunctionNotFound(MainFunctionNotFoundError),
    MainFunctionWrongType(MainFunctionWrongTypeError),
    ExplicitReturnStatementFoundInConstructorBody(
        ExplicitReturnStatementFoundInConstructorBodyError,
    ),
    InterfaceAlreadyExistInBoundsDeclaration(InterfaceAlreadyExistInBoundsDeclarationError),
    GenericTypesDeclarationInsideConstructorFound(
        GenericTypesDeclarationInsideConstructorFoundError,
    ),
    GenericTypeArgsNotExpected(GenericTypeArgsNotExpectedError),
    GenericTypeArgsExpected(GenericTypeArgsExpectedError),
    GenericTypeArgsCountMismatched(GenericTypeArgsCountMismatchedError),
    GenericTypeArgsIncorrectlyBounded(GenericTypeArgsIncorrectlyBoundedError),
    InterfaceMethodsInStructCheck(InterfaceMethodsInStructCheckError),
    InitMethodNotAllowedInsideConstructor(InitMethodNotAllowedInsideConstructorError),
    InferredLambdaVariableTypeMismatchedWithTypeFromAnnotation(
        InferredLambdaVariableTypeMismatchedWithTypeFromAnnotationError,
    ),
    RightSideExpressionTypeMismatchedWithTypeFromAnnotation(
        RightSideExpressionTypeMismatchedWithTypeFromAnnotationError,
    ),
    IncorrectExpressionType(IncorrectExpressionTypeError),
    ExpressionTypeCannotBeInferred(ExpressionTypeCannotBeInferredError),
    ClassMethodExpectedParenthesis(ClassMethodExpectedParenthesisError),
    EnumVariantDoesNotExist(EnumVariantDoesNotExistError),
    UnexpectedValueProvidedToEnumVariant(UnexpectedValueProvidedToEnumVariantError),
    ExpectedValueForEnumVariant(ExpectedValueForEnumVariantError),
    EnumVariantsMissingFromMatchCaseStatement(EnumVariantsMissingFromMatchCaseStatementError),
    IncorrectEnumName(IncorrectEnumNameError),
    NonIterableExpression(NonIterableExpressionError),
    PropertyResolvedToMultipleInterfaceObjects(PropertyResolvedToMultipleInterfaceObjectsError),
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
            Diagnostics::RightSideWithVoidTypeNotAllowed(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::IdentifierNotDeclared(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::NonVoidConstructorReturnType(diagonstic) => {
                Report::new(diagonstic.clone())
            }
            Diagnostics::NonStructConstructorReturnType(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::MismatchedConstructorReturnType(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::SelfNotFound(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::IdentifierUsedBeforeInitialized(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::MoreParamsCount(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::LessParamsCount(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::MismatchedParamType(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::IdentifierNotCallable(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::FieldNotCallable(diagnostic) => Report::new(diagnostic.clone()),
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
            Diagnostics::MainFunctionNotFound(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::MainFunctionWrongType(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::ExplicitReturnStatementFoundInConstructorBody(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::GenericTypeResolvedToOutsideScope(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::InterfaceAlreadyExistInBoundsDeclaration(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::GenericTypesDeclarationInsideConstructorFound(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::GenericTypeArgsNotExpected(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::GenericTypeArgsExpected(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::GenericTypeArgsCountMismatched(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::GenericTypeArgsIncorrectlyBounded(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::NotAllConcreteTypesInferred(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::TypeInferenceFailed(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::InferredTypesNotBoundedByInterfaces(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::InterfaceMethodsInStructCheck(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::InitMethodNotAllowedInsideConstructor(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::InferredLambdaVariableTypeMismatchedWithTypeFromAnnotation(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::RightSideExpressionTypeMismatchedWithTypeFromAnnotation(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::IncorrectExpressionType(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::ExpressionTypeCannotBeInferred(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::InvalidLoopControlFlowStatementFound(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::ClassMethodExpectedParenthesis(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::EnumVariantDoesNotExist(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::UnexpectedValueProvidedToEnumVariant(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::ExpectedValueForEnumVariant(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::EnumVariantsMissingFromMatchCaseStatement(diagnostic) => {
                Report::new(diagnostic.clone())
            }
            Diagnostics::IncorrectEnumName(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::NonIterableExpression(diagnostic) => Report::new(diagnostic.clone()),
            Diagnostics::PropertyResolvedToMultipleInterfaceObjects(diagnostic) => {
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
    pub fn new(range: TextRange) -> Self {
        InvalidCharError {
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error(
    r#"opening symbols `/*`, `'` and `"` should have closing parts `*/`, `'` and `"` respectively"#
)]
#[diagnostic(code("LexicalError"))]
pub struct NoClosingSymbolError {
    pub expected_symbol: &'static str,
    #[label("no closing `{}` found", self.expected_symbol)]
    pub unclosed_span: SourceSpan,
}

impl NoClosingSymbolError {
    pub fn new(expected_symbol: &'static str, range: TextRange) -> Self {
        NoClosingSymbolError {
            expected_symbol,
            unclosed_span: range_to_span(range).into(),
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
        Some(Box::new("SyntaxError"))
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
        Some(Box::new(span_vec.into_iter()))
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
                "constants and output of function call is not assignable"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("expected token missing")]
#[diagnostic(code("SyntaxError"))]
pub struct ClassMethodExpectedParenthesisError {
    #[label("expected `(` for classmethod call expression")]
    pub span: SourceSpan,
}

impl ClassMethodExpectedParenthesisError {
    pub fn new(end_index: usize) -> Self {
        let start_index = end_index - 1;
        ClassMethodExpectedParenthesisError {
            span: (start_index, end_index - start_index).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("return-type in constructor declaration found")]
#[diagnostic(code("SemanticError"))]
pub struct NonVoidConstructorReturnTypeError {
    #[label("constructor cannot have a return-type")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>, // any value derived from a function call is not assignable
}

impl NonVoidConstructorReturnTypeError {
    pub fn new(range: TextRange) -> Self {
        NonVoidConstructorReturnTypeError {
            span: range_to_span(range).into(),
            help: Some(
                "constructor should not have any return-type"
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
        name: &str,
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
            IdentifierKind::UserDefinedType => {
                "types are not allowed to be redeclared inside the complete scope chain".to_string()
            }
            IdentifierKind::Interface => {
                "interfaces are not allowed to be redeclared inside the complete scope chain"
                    .to_string()
            }
            IdentifierKind::Argument => {
                "parameters are not allowed to be redeclared in the same function defintion"
                    .to_string()
            }
            IdentifierKind::Field | IdentifierKind::Method => {
                format!(
                    "all {}s of struct and interfaces should have distinct names",
                    identifier_kind
                )
            }
            IdentifierKind::Variant => {
                "all variants of enum should have distinct names".to_string()
            }
            IdentifierKind::Constructor => {
                "constructor is not allowed to be redeclared".to_string()
            }
        };
        IdentifierAlreadyDeclaredError {
            identifier_kind,
            name: name.to_string(),
            previous_decl_span: range_to_span(previous_decl_range).into(),
            redecl_span: range_to_span(redecl_range).into(),
            help: Some(help_str.style(Style::new().yellow()).to_string()),
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
    pub err_msg: String,
    #[label("fields {} not initialized inside the constructor", err_msg)]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl FieldsNotInitializedInConstructorError {
    pub fn new(missing_fields_vec: Vec<&StrId>, range: TextRange, interner: &Interner) -> Self {
        let len = missing_fields_vec.len();
        let mut message = format!("`{}`", interner.lookup(*missing_fields_vec[0]));
        if len > 1 {
            for i in 1..(len - 1) {
                message.push_str(&format!(", `{}`", interner.lookup(*missing_fields_vec[i])));
            }
            message.push_str(&format!(
                " and `{}`",
                interner.lookup(*missing_fields_vec[len - 1])
            ));
        }
        FieldsNotInitializedInConstructorError {
            err_msg: message,
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
#[error("enum variants missing from match-case statement")]
#[diagnostic(code("SemanticError"))]
pub struct EnumVariantsMissingFromMatchCaseStatementError {
    pub enum_name: String,
    pub err_msg: String,
    #[label(
        "variants {} not handled inside the match-case statement for the expression with type `{}`",
        err_msg,
        self.enum_name
    )]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl EnumVariantsMissingFromMatchCaseStatementError {
    pub fn new(
        enum_name: String,
        missing_variants: Vec<StrId>,
        range: TextRange,
        interner: &Interner,
    ) -> Self {
        let len = missing_variants.len();
        let mut message = format!("`{}`", interner.lookup(missing_variants[0]));
        if len > 1 {
            for i in 1..(len - 1) {
                message.push_str(&format!(", `{}`", interner.lookup(missing_variants[i])));
            }
            message.push_str(&format!(
                " and `{}`",
                interner.lookup(missing_variants[len - 1])
            ));
        }
        EnumVariantsMissingFromMatchCaseStatementError {
            enum_name,
            err_msg: message,
            span: range_to_span(range).into(),
            help: Some(
                "all variants should be handled inside the match-case statement for the expression with enum type"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("`self` is not declared in the scope")]
#[diagnostic(code("SemanticError"))]
pub struct IncorrectEnumNameError {
    pub expected_enum_name: String,
    pub received_enum_name: String,
    #[label("expected enum `{}`, got `{}`", self.expected_enum_name, self.received_enum_name)]
    pub span: SourceSpan,
}

impl IncorrectEnumNameError {
    pub fn new(expected_enum_name: String, received_enum_name: String, range: TextRange) -> Self {
        IncorrectEnumNameError {
            expected_enum_name,
            received_enum_name,
            span: range_to_span(range).into(),
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
#[error("inferred type of lambda variable does not match with provided type annotation")]
#[diagnostic(code("SemanticError"))]
pub struct InferredLambdaVariableTypeMismatchedWithTypeFromAnnotationError {
    pub ty_from_annotation: String,
    pub inferred_lambda_ty: String,
    #[label("inferred lambda type of right side expression does not match this")]
    pub span: SourceSpan,
    help: Option<String>,
}

impl InferredLambdaVariableTypeMismatchedWithTypeFromAnnotationError {
    pub fn new(ty_from_annotation: String, inferred_lambda_ty: String, range: TextRange) -> Self {
        InferredLambdaVariableTypeMismatchedWithTypeFromAnnotationError {
            ty_from_annotation,
            inferred_lambda_ty,
            span: range_to_span(range).into(),
            help: Some(
                "use lambda type in annotation matching the prototype with the lambda on the right side"
                .to_string()
                .style(Style::new().yellow())
                .to_string()
            )
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("identifier `{}` referenced before initialization", self.identifier_name)]
#[diagnostic(code("SemanticError"))]
pub struct IdentifierUsedBeforeInitializedError {
    pub identifier_name: String,
    pub identifier_kind: IdentifierKind,
    #[label("{} declared here", identifier_kind)]
    pub decl_span: SourceSpan,
    #[label("same {} used within the declaration", identifier_kind)]
    pub usage_span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl IdentifierUsedBeforeInitializedError {
    pub fn new(
        identifier_name: &str,
        identifier_kind: IdentifierKind,
        decl_range: TextRange,
        usage_range: TextRange,
    ) -> Self {
        let help_str = match identifier_kind {
            IdentifierKind::Variable => "variables are not allowed to be referenced inside their own declaration statement",
            IdentifierKind::UserDefinedType => "struct types are not allowed to be referenced inside their own generic types declaration and implementing interfaces",
            IdentifierKind::Interface => "interfaces are not allowed to be referenced inside their own generic types declaration",
            _ => unreachable!()
        };
        IdentifierUsedBeforeInitializedError {
            identifier_name: identifier_name.to_string(),
            identifier_kind,
            decl_span: range_to_span(decl_range).into(),
            usage_span: range_to_span(usage_range).into(),
            help: Some(
                help_str
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("interface reincluded in the bounds")]
#[diagnostic(code("SemanticError"))]
pub struct InterfaceAlreadyExistInBoundsDeclarationError {
    name: String,
    #[label("interface `{}` is included here", self.name)]
    pub previous_decl_span: SourceSpan,
    #[label("interface `{}` reincluded here", self.name)]
    pub decl_span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl InterfaceAlreadyExistInBoundsDeclarationError {
    pub fn new(name: &str, previous_decl_span: TextRange, decl_span: TextRange) -> Self {
        InterfaceAlreadyExistInBoundsDeclarationError {
            name: name.to_string(),
            previous_decl_span: range_to_span(previous_decl_span).into(),
            decl_span: range_to_span(decl_span).into(),
            help: Some(
                "interface can only be included once inside the bounds declaration"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("init method not allowed inside interface")]
#[diagnostic(code("SemanticError"))]
pub struct InitMethodNotAllowedInsideConstructorError {
    #[label("prototype with name `__init__` is not allowed")]
    pub span: SourceSpan,
}

impl InitMethodNotAllowedInsideConstructorError {
    pub fn new(span: TextRange) -> Self {
        InitMethodNotAllowedInsideConstructorError {
            span: range_to_span(span).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("invalid {} statement found", self.kind)]
#[diagnostic(code("SemanticError"))]
pub struct InvalidLoopControlFlowStatementFoundError {
    pub kind: &'static str,
    #[label("no enclosing loop found for the statmement")]
    pub span: SourceSpan,
}

impl InvalidLoopControlFlowStatementFoundError {
    pub fn new(span: TextRange, kind: &'static str) -> Self {
        InvalidLoopControlFlowStatementFoundError {
            span: range_to_span(span).into(),
            kind,
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("generic types declaration inside constructor found")]
#[diagnostic(code("SemanticError"))]
pub struct GenericTypesDeclarationInsideConstructorFoundError {
    #[label("generic types declaration found")]
    pub span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl GenericTypesDeclarationInsideConstructorFoundError {
    pub fn new(span: TextRange) -> Self {
        GenericTypesDeclarationInsideConstructorFoundError {
            span: range_to_span(span).into(),
            help: Some(
                "generic types declaration is not allowed inside constructor"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("generic type resolved to outside scope")]
#[diagnostic(code("SemanticError"))]
pub struct GenericTypeResolvedToOutsideScopeError {
    #[label("generic type resolved to the outside declaration")]
    pub usage_span: SourceSpan,
    #[label("generic type is declared here")]
    pub decl_span: SourceSpan,
    #[help]
    help: Option<String>,
}

impl GenericTypeResolvedToOutsideScopeError {
    pub fn new(usage_span: TextRange, decl_span: TextRange) -> Self {
        GenericTypeResolvedToOutsideScopeError {
            usage_span: range_to_span(usage_span).into(),
            decl_span: range_to_span(decl_span).into(),
            help: Some(
                "generic types are not allowed to be resolved to declarations outside the enclosing generics declarative constructs like functions, methods, structs and interfaces."
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
                "callable identifier are resolved in the following order of namespace: function => type => variable"
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
    pub fn new(ty: String, range: TextRange) -> Self {
        ImmutableTypeNotAssignableError {
            ty,
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
#[error("generic type arguments not provided")]
#[diagnostic(code("SemanticError"))]
pub struct GenericTypeArgsExpectedError {
    pub identifier_kind: IdentifierKind,
    #[label("generic type arguments expected by the {}", identifier_kind)]
    pub span: SourceSpan,
}

impl GenericTypeArgsExpectedError {
    pub fn new(identifier_kind: IdentifierKind, range: TextRange) -> Self {
        GenericTypeArgsExpectedError {
            identifier_kind,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("unexpected generic type arguments provided")]
#[diagnostic(code("SemanticError"))]
pub struct GenericTypeArgsNotExpectedError {
    pub identifier_kind: IdentifierKind,
    #[label("generic type arguments not expected by the {}", identifier_kind)]
    pub span: SourceSpan,
}

impl GenericTypeArgsNotExpectedError {
    pub fn new(identifier_kind: IdentifierKind, range: TextRange) -> Self {
        GenericTypeArgsNotExpectedError {
            identifier_kind,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("mismatched expected and passed generic type arguments")]
#[diagnostic(code("SemanticError"))]
pub struct GenericTypeArgsCountMismatchedError {
    pub received_count: usize,
    pub expected_count: usize,
    #[label(
        "expected {} generic type arguments, got {}",
        expected_count,
        received_count
    )]
    pub span: SourceSpan,
}

impl GenericTypeArgsCountMismatchedError {
    pub fn new(received_count: usize, expected_count: usize, range: TextRange) -> Self {
        GenericTypeArgsCountMismatchedError {
            received_count,
            expected_count,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Debug, Error, Clone)]
#[error("incorrectly bounded generic type arguments found")]
pub struct GenericTypeArgsIncorrectlyBoundedError {
    incorrectly_bounded_types: Vec<(TextRange, String)>, // (ty_span, interface_bounds_str)
}
impl GenericTypeArgsIncorrectlyBoundedError {
    pub fn new(incorrectly_bounded_types: &Vec<(TextRange, String)>) -> Self {
        GenericTypeArgsIncorrectlyBoundedError {
            incorrectly_bounded_types: incorrectly_bounded_types.clone(),
        }
    }
}

impl Diagnostic for GenericTypeArgsIncorrectlyBoundedError {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        let mut span_vec: Vec<LabeledSpan> = vec![];
        for factor in &self.incorrectly_bounded_types {
            let err_message = format!("type is not bounded by interfaces `{}`", factor.1);
            let start_index: usize = factor.0.start().into();
            let len: usize = (factor.0.end() - factor.0.start()).into();
            span_vec.push(miette::LabeledSpan::new(
                Some(err_message),
                start_index,
                len,
            ));
        }
        Some(Box::new(span_vec.into_iter()))
    }

    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new("TypeCheckError"))
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
        Some(Box::new(span_vec.into_iter()))
    }

    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new("TypeCheckError"))
    }
}

#[derive(Debug, Error, Clone)]
#[error("struct type not implementing interface methods correctly")]
pub struct InterfaceMethodsInStructCheckError {
    missing_interface_method_names: Option<String>,
    errors: Option<Vec<(String, TextRange)>>,
    interface_range: TextRange,
}

impl InterfaceMethodsInStructCheckError {
    pub fn new(
        missing_interface_method_names: Option<Vec<&StrId>>,
        errors: Option<Vec<(&StrId, PartialConcreteInterfaceMethodsCheckError)>>,
        interface_name: String,
        interface_range: TextRange,
        interner: &Interner,
    ) -> Self {
        let missing_interface_method_names: Option<String> =
            if let Some(missing_interface_method_names) = missing_interface_method_names {
                let mut s = "".to_string();
                if !missing_interface_method_names.is_empty() {
                    s.push_str(interner.lookup(*missing_interface_method_names[0]));
                }
                for i in 1..missing_interface_method_names.len() {
                    s.push_str(&format!(
                        ", {}",
                        interner.lookup(*missing_interface_method_names[i])
                    ));
                }
                Some(s)
            } else {
                None
            };
        let errors: Option<Vec<(String, TextRange)>> = if let Some(errors) = errors {
            let mut v = vec![];
            for (_, err) in errors {
                let s: (String, TextRange) = match err {
                    PartialConcreteInterfaceMethodsCheckError::GenericTypesDeclarationCheckFailed(range) => {
                        (format!("generic types declaration does not match with interface `{}` specification for this method", interface_name), range)
                    },
                    PartialConcreteInterfaceMethodsCheckError::GenericTypesDeclarationNotExpected(range) => {
                        (format!("generic types declaration is not expected by interface `{}` specification for this method", interface_name), range)
                    },
                    PartialConcreteInterfaceMethodsCheckError::GenericTypesDeclarationExpected(range) => {
                        (format!("generic types declaration expected by interface `{}` specification for this method", interface_name), range)
                    },
                    PartialConcreteInterfaceMethodsCheckError::PrototypeEquivalenceCheckFailed(range) => {
                        (format!("prototype does not match with the interface `{}` specification for this method", interface_name), range)
                    },
                };
                v.push(s);
            }
            Some(v)
        } else {
            None
        };
        InterfaceMethodsInStructCheckError {
            missing_interface_method_names,
            errors,
            interface_range,
        }
    }
}

impl Diagnostic for InterfaceMethodsInStructCheckError {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        let mut span_vec: Vec<LabeledSpan> = vec![];
        let s = match &self.missing_interface_method_names {
            Some(s) => {
                format!("missing interface methods: {}", s)
            }
            None => "interface not correctly implemented by the struct type".to_string(),
        };
        let start_index: usize = self.interface_range.start().into();
        let len: usize = (self.interface_range.end() - self.interface_range.start()).into();
        span_vec.push(miette::LabeledSpan::new(Some(s), start_index, len));
        if let Some(errors) = &self.errors {
            for (err_message, method_range) in errors {
                let start_index: usize = method_range.start().into();
                let len: usize = (method_range.end() - method_range.start()).into();
                span_vec.push(miette::LabeledSpan::new(
                    Some(err_message.to_string()),
                    start_index,
                    len,
                ));
            }
        }
        Some(Box::new(span_vec.into_iter()))
    }

    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new("TypeCheckError"))
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("concrete types inference failed")]
#[diagnostic(code("TypeCheckError"))]
pub struct NotAllConcreteTypesInferredError {
    #[label("complete set of concrete types cannot be inferred")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl NotAllConcreteTypesInferredError {
    pub fn new(range: TextRange) -> Self {
        NotAllConcreteTypesInferredError {
            span: range_to_span(range).into(),
            help: Some(
                "explicitly specify the generic type arguments using <...>"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("concrete types inference failed")]
#[diagnostic(code("TypeCheckError"))]
pub struct TypeInferenceFailedError {
    #[label("cannot infer concrete types")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl TypeInferenceFailedError {
    pub fn new(range: TextRange) -> Self {
        TypeInferenceFailedError {
            span: range_to_span(range).into(),
            help: Some(
                "explicitly specify the generic type arguments using <...>"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("concrete types inference failed")]
#[diagnostic(code("TypeCheckError"))]
pub struct InferredTypesNotBoundedByInterfacesError {
    pub msg: String,
    #[label("{}", msg)]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl InferredTypesNotBoundedByInterfacesError {
    pub fn new(
        range: TextRange,
        err_strs: Vec<(String, String)>,
        concrete_types: Vec<Type>,
        interner: &Interner,
    ) -> Self {
        let mut concrete_types_str = "<".to_string();
        let concrete_types_len = concrete_types.len();
        concrete_types_str.push_str(&concrete_types[0].to_string(interner));
        for i in 1..concrete_types_len {
            concrete_types_str.push_str(&format!(", {}", concrete_types[i].to_string(interner)));
        }
        concrete_types_str.push('>');
        let mut err_msg = format!(
            "type `{}` is not bounded by interfaces `{}`",
            err_strs[0].0, err_strs[0].1
        );
        for (ty_str, interface_bounds_str) in &err_strs[1..] {
            err_msg.push_str(&format!(
                "\ntype `{}` is not bounded by interfaces `{}`",
                ty_str, interface_bounds_str
            ));
        }
        InferredTypesNotBoundedByInterfacesError {
            span: range_to_span(range).into(),
            msg: format!(
                "inferred types `{}` are not bounded:\n{}",
                concrete_types_str, err_msg
            ),
            help: Some(
                "explicitly specify the generic type arguments using <...>"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("ambigious {} resolution", self.property_kind)]
#[diagnostic(code("TypeCheckError"))]
pub struct PropertyResolvedToMultipleInterfaceObjectsError {
    pub property_kind: PropertyKind,
    pub msg: String,
    #[label("{}", msg)]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl PropertyResolvedToMultipleInterfaceObjectsError {
    pub fn new(range: TextRange, interface_objs: Vec<String>, property_kind: PropertyKind) -> Self {
        let mut err_msg = interface_objs[0].to_string();
        for i in 1..interface_objs.len() {
            err_msg.push_str(&format!(", {}", interface_objs[i]));
        }
        PropertyResolvedToMultipleInterfaceObjectsError {
            property_kind,
            span: range_to_span(range).into(),
            msg: format!(
                "{} resolved to following multiple interface objects: {}",
                property_kind, err_msg
            ),
            help: Some(
                "there should not be name collision for fields and methods between interfaces inside bounds of any generic type"
                    .to_string()
                    .style(Style::new().yellow())
                    .to_string(),
            ),
        }
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
#[error("expression not iterable")]
#[diagnostic(code("TypeCheckError"))]
pub struct NonIterableExpressionError {
    pub ty: String,
    #[label("expression with type `{}` is not iterable", self.ty)]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl NonIterableExpressionError {
    pub fn new(ty: String, range: TextRange) -> Self {
        NonIterableExpressionError {
            ty,
            span: range_to_span(range).into(),
            help: Some(
                "only expressions with iterable type like `array`, `hashmap`, `str` should be used inside `for` loop"
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
    pub fn new(ty_str: &str, range: TextRange) -> Self {
        ConstructorNotFoundForTypeError {
            ty: ty_str.to_string(),
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
    pub fn new(struct_name: &str, range: TextRange) -> Self {
        ClassmethodDoesNotExistError {
            struct_name: struct_name.to_string(),
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
#[error("field not callable")]
#[diagnostic(code("TypeCheckError"))]
pub struct FieldNotCallableError {
    pub ty: String,
    #[label("field with type `{}` is not callable", self.ty)]
    pub field_span: SourceSpan,
}

impl FieldNotCallableError {
    pub fn new(ty: String, field_span: TextRange) -> Self {
        FieldNotCallableError {
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
#[error("incorrect expression type")]
#[diagnostic(code("TypeCheckError"))]
pub struct IncorrectExpressionTypeError {
    pub expected_ty: String,
    pub received_ty: String,
    #[label(
        "expected type `{}`, got expression with type `{}`",
        expected_ty,
        received_ty
    )]
    pub span: SourceSpan,
}

impl IncorrectExpressionTypeError {
    pub fn new(expected_ty: String, received_ty: String, range: TextRange) -> Self {
        IncorrectExpressionTypeError {
            expected_ty,
            received_ty,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("expression type cannot be inferred")]
#[diagnostic(code("TypeCheckError"))]
pub struct ExpressionTypeCannotBeInferredError {
    #[label("expression type is not clear")]
    pub span: SourceSpan,
    #[help]
    pub help: Option<String>,
}

impl ExpressionTypeCannotBeInferredError {
    pub fn new(range: TextRange) -> Self {
        ExpressionTypeCannotBeInferredError {
            span: range_to_span(range).into(),
            help: Some(
                "while declaring the variable, use explicit type annotation to remove ambiguity associated with empty collections like lists and dicts"
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
#[error("right side expression type does not match the type annotated on the variable")]
#[diagnostic(code("TypeCheckError"))]
pub struct RightSideExpressionTypeMismatchedWithTypeFromAnnotationError {
    pub ty_from_annotation: String,
    pub right_expr_ty: String,
    #[label("variable has type `{}`", ty_from_annotation)]
    pub variable_span: SourceSpan,
    #[label("expression has type `{}`", right_expr_ty)]
    pub right_expr_span: SourceSpan,
}

impl RightSideExpressionTypeMismatchedWithTypeFromAnnotationError {
    pub fn new(
        ty_from_annotation: String,
        right_expr_ty: String,
        variable_range: TextRange,
        right_expr_range: TextRange,
    ) -> Self {
        RightSideExpressionTypeMismatchedWithTypeFromAnnotationError {
            ty_from_annotation,
            right_expr_ty,
            variable_span: range_to_span(variable_range).into(),
            right_expr_span: range_to_span(right_expr_range).into(),
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
    #[label("explicit return statement found")]
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
            expected_type,
            received_type,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("enum variant not found")]
#[diagnostic(code("TypeCheckError"))]
pub struct EnumVariantDoesNotExistError {
    pub enum_name: String,
    #[label("no variant with this name found for enum `{}`", self.enum_name)]
    pub span: SourceSpan,
}

impl EnumVariantDoesNotExistError {
    pub fn new(enum_name: String, range: TextRange) -> Self {
        EnumVariantDoesNotExistError {
            enum_name,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("unexpected value provided to enum variant")]
#[diagnostic(code("TypeCheckError"))]
pub struct UnexpectedValueProvidedToEnumVariantError {
    pub variant_name: String,
    #[label("enum variant `{}` does not expect a value", self.variant_name)]
    pub span: SourceSpan,
}

impl UnexpectedValueProvidedToEnumVariantError {
    pub fn new(variant_name: String, range: TextRange) -> Self {
        UnexpectedValueProvidedToEnumVariantError {
            variant_name,
            span: range_to_span(range).into(),
        }
    }
}

#[derive(Diagnostic, Debug, Error, Clone)]
#[error("enum variant expected a value")]
#[diagnostic(code("TypeCheckError"))]
pub struct ExpectedValueForEnumVariantError {
    pub variant_ty: String,
    #[label("enum variant expected a value with type `{}`", self.variant_ty)]
    pub span: SourceSpan,
}

impl ExpectedValueForEnumVariantError {
    pub fn new(variant_ty: String, range: TextRange) -> Self {
        ExpectedValueForEnumVariantError {
            variant_ty,
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
#[diagnostic(code("SyntaxError"))]
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
