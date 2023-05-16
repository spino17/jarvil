// This module contains green tree nodes. Green Tree is top to down immutable typed structure with no parent information.
// See the following for more information on green and red tree, immutability and cheap mutations.
// 1. `https://github.com/apple/swift/tree/5e2c815edfd758f9b1309ce07bfc01c4bc20ec23/lib/Syntax`
// 2. `https://github.com/rust-analyzer/rowan`

// NOTE: This file only contains structure of the AST nodes and not their bounded methods. The methods for respective `ASTNode::<...>`
// is declared in the file `impl_ast.rs`

#[macro_use]
use jarvil_macros::Nodify;
#[macro_use]
use jarvil_macros::Node;

use crate::lexer::token::BinaryOperatorKind;
use crate::lexer::token::UnaryOperatorKind;
use crate::scope::core::IdentifierKind;
use crate::{lexer::token::Token, scope::core::Namespace, types::core::Type};
use std::sync::Weak;
use std::{cell::RefCell, rc::Rc};
use text_size::{TextRange, TextSize};

pub trait Node {
    fn range(&self) -> TextRange;
    fn start_line_number(&self) -> usize;
}

pub trait ErrornousNode {
    fn new_with_missing_tokens(
        expected_symbols: &Rc<Vec<&'static str>>,
        received_token: &Token,
    ) -> Self;
}

#[derive(Debug, Clone, Nodify)]
pub enum ASTNode {
    BLOCK(BlockNode),

    // Statements
    STATEMENT_INDENT_WRAPPER(StatemenIndentWrapperNode),
    SKIPPED_TOKENS(SkippedTokensNode),
    INCORRECTLY_INDENTED_STATEMENT(IncorrectlyIndentedStatementNode),
    STATEMENT(StatementNode),
    RETURN(ReturnStatementNode),

    // Declaration `n` Assignment
    VARIABLE_DECLARATION(VariableDeclarationNode),
    ASSIGNMENT(AssignmentNode),
    OK_ASSIGNMENT(OkAssignmentNode),
    INVALID_L_VALUE(InvalidLValueNode),
    INVALID_R_LAMBDA(InvalidRLambdaNode),
    R_ASSIGNMENT(RAssignmentNode),
    R_VARIABLE_DECLARATION(RVariableDeclarationNode),

    // Types
    TYPE_DECLARATION(TypeDeclarationNode),
    STRUCT_DECLARATION(StructDeclarationNode),
    STRUCT_STATEMENT(StructStatementNode),
    LAMBDA_TYPE_DECLARATION(LambdaTypeDeclarationNode),
    OK_LAMBDA_TYPE_DECLARATION(OkLambdaTypeDeclarationNode),
    TYPE_EXPRESSION(TypeExpressionNode),
    ATOMIC_TYPE(AtomicTypeNode),
    ARRAY_TYPE(ArrayTypeNode),
    USER_DEFINED_TYPE(UserDefinedTypeNode),

    // Callable
    CALLABLE_PROTOTYPE(CallablePrototypeNode),
    CALLABLE_BODY(CallableBodyNode),
    OK_CALLABLE_BODY(OkCallableBodyNode),
    FUNCTION_DECLARATION(FunctionDeclarationNode),
    LAMBDA_DECLARATION(LambdaDeclarationNode),

    // Expression
    EXPRESSION_STATEMENT(ExpressionStatementNode),
    EXPRESSION(ExpressionNode),
    ATOMIC_EXPRESSION(AtomicExpressionNode),
    PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode),
    UNARY_EXPRESSION(UnaryExpressionNode),
    ONLY_UNARY_EXPRESSION(OnlyUnaryExpressionNode),
    BINARY_EXPRESSION(BinaryExpressionNode),
    COMPARISON(ComparisonNode),
    CALL_EXPRESSION(CallExpressionNode),

    // Atom
    ATOM(AtomNode),
    ATOM_START(AtomStartNode),
    PROPERTY_ACCESS(PropertyAccessNode),
    METHOD_ACCESS(MethodAccessNode),
    INDEX_ACCESS(IndexAccessNode),
    CALL(CallNode),
    CLASS_METHOD_CALL(ClassMethodCallNode),

    // General
    NAME_TYPE_SPECS(NameTypeSpecsNode),
    OK_NAME_TYPE_SPECS(OkNameTypeSpecsNode),
    NAME_TYPE_SPEC(NameTypeSpecNode),
    TYPE_TUPLE(TypeTupleNode),
    OK_TYPE_TUPLE(OkTypeTupleNode),
    PARAMS(ParamsNode),
    OK_PARAMS(OkParamsNode),

    // Basic
    IDENTIFIER(IdentifierNode),
    OK_IDENTIFIER(OkIdentifierNode),
    TOKEN(TokenNode),
    OK_TOKEN(OkTokenNode),
    MISSING_TOKEN(MissingTokenNode),
    SKIPPED_TOKEN(SkippedTokenNode),
}

// core nodes containing the structure for storing concrete syntax
// BLOCK
// `\n` <stmts>
#[derive(Debug, Clone)]
pub struct CoreBlockNode {
    pub newline: TokenNode,
    pub stmts: Vec<StatemenIndentWrapperNode>,
    pub scope: Option<Namespace>,
    pub kind: BlockKind,
}

// STATEMENT_INDENT_WRAPPER
#[derive(Debug, Clone, Node)]
pub enum CoreStatemenIndentWrapperNode {
    CORRECTLY_INDENTED(StatementNode),
    INCORRECTLY_INDENTED(IncorrectlyIndentedStatementNode),
    LEADING_SKIPPED_TOKENS(SkippedTokensNode), // skipped tokens leading to the next stmt in block
    TRAILING_SKIPPED_TOKENS(SkippedTokensNode), // skipped tokens trailing to the previous stmt in block
    EXTRA_NEWLINES(SkippedTokensNode),
}

// SKIPPED_TOKENS
#[derive(Debug, Clone)]
pub struct CoreSkippedTokensNode {
    pub skipped_tokens: Vec<SkippedTokenNode>,
}

// INCORRECTLY_INDENTED_STATEMENT
#[derive(Debug, Clone)]
pub struct CoreIncorrectlyIndentedStatementNode {
    pub stmt: StatementNode,
    pub expected_indent: i64,
    pub received_indent: i64,
}

// STATEMENT
#[derive(Debug, Clone, Node)]
pub enum CoreStatementNode {
    EXPRESSION(ExpressionStatementNode),
    ASSIGNMENT(AssignmentNode),
    VARIABLE_DECLARATION(VariableDeclarationNode),
    RETURN(ReturnStatementNode),
    FUNCTION_DECLARATION(FunctionDeclarationNode),
    TYPE_DECLARATION(TypeDeclarationNode),
    STRUCT_STATEMENT(StructStatementNode),
    MISSING_TOKENS(MissingTokenNode),
}

// RETURN
// `return` <expr> `\n`
#[derive(Debug, Clone)]
pub struct CoreReturnStatementNode {
    pub return_keyword: TokenNode,
    pub expr: Option<ExpressionNode>,
    pub newline: TokenNode,
}

// VARIABLE_DECLARATION
// `let` <name> `=` <r_assign>
#[derive(Debug, Clone)]
pub struct CoreVariableDeclarationNode {
    pub let_keyword: TokenNode,
    pub equal: TokenNode,
    pub name: IdentifierNode,
    pub r_node: RVariableDeclarationNode,
}

// ASSIGNMENT
#[derive(Debug, Clone, Node)]
pub enum CoreAssignmentNode {
    OK(OkAssignmentNode),
    INVALID_L_VALUE(InvalidLValueNode),
    INVALID_R_LAMBDA(InvalidRLambdaNode),
}

// OK_ASSIGNMENT
// <l_atom> `=` <r_assign>
#[derive(Debug, Clone)]
pub struct CoreOkAssignmentNode {
    pub equal: TokenNode,
    pub l_atom: AtomNode,
    pub r_assign: RAssignmentNode,
}

// INVALID_L_VALUE
// <l_expr> `=` <r_assign>
#[derive(Debug, Clone)]
pub struct CoreInvalidLValueNode {
    pub l_expr: ExpressionNode,
    pub equal: TokenNode,
    pub r_assign: RAssignmentNode,
}

// INVALID_L_VALUE
// <l_expr> `=` <r_assign>
#[derive(Debug, Clone)]
pub struct CoreInvalidRLambdaNode {
    pub l_expr: ExpressionNode,
    pub equal: TokenNode,
    pub r_assign: RAssignmentNode,
}

// R_ASSIGNMENT
#[derive(Debug, Clone, Node)]
pub enum CoreRAssignmentNode {
    EXPRESSION(ExpressionStatementNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone, Node)]
pub enum CoreRVariableDeclarationNode {
    LAMBDA(LambdaDeclarationNode),
    EXPRESSION(ExpressionStatementNode),
    MISSING_TOKENS(MissingTokenNode),
}

// TYPE_DECLARATION
#[derive(Debug, Clone, Node)]
pub enum CoreTypeDeclarationNode {
    STRUCT(StructDeclarationNode),
    LAMBDA(LambdaTypeDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

// STRUCT_DECLARATION
// `type` <name> struct `:` <block>
#[derive(Debug, Clone)]
pub struct CoreStructDeclarationNode {
    pub type_keyword: TokenNode,
    pub colon: TokenNode,
    pub struct_keyword: TokenNode,
    pub name: IdentifierNode,
    pub block: BlockNode,
}

// STRUCT_STATEMENT
// <name_type_spec> `\n`
#[derive(Debug, Clone)]
pub struct CoreStructStatementNode {
    pub newline: TokenNode,
    pub name_type_spec: NameTypeSpecNode,
}

// LAMBDA_TYPE_DECLARATION
#[derive(Debug, Clone, Node)]
pub enum CoreLambdaTypeDeclarationNode {
    OK(OkLambdaTypeDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

// OK_LAMBDA_TYPE_DECLARATION
// `type` <name> `:` <prototype> `\n`
#[derive(Debug, Clone)]
pub struct CoreOkLambdaTypeDeclarationNode {
    pub type_keyword: TokenNode,
    pub lambda_keyword: TokenNode,
    pub name: IdentifierNode,
    pub equal: TokenNode,
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub type_tuple: Option<TypeTupleNode>,
    pub right_arrow: Option<TokenNode>,
    pub return_type: Option<TypeExpressionNode>,
    pub newline: TokenNode,
}

// TYPE_EXPRESSION
#[derive(Debug, Clone, Node)]
pub enum CoreTypeExpressionNode {
    ATOMIC(AtomicTypeNode),
    USER_DEFINED(UserDefinedTypeNode),
    ARRAY(ArrayTypeNode),
    MISSING_TOKENS(MissingTokenNode),
}

// ATOMIC_TYPE
// `int` | `float` | `string` | `bool` ...
#[derive(Debug, Clone)]
pub struct CoreAtomicTypeNode {
    pub kind: TokenNode,
}

// ARRAY_TYPE
// `[` <sub_type> `]`
#[derive(Debug, Clone)]
pub struct CoreArrayTypeNode {
    pub lsquare: TokenNode,
    pub rsquare: TokenNode,
    pub sub_type: TypeExpressionNode,
}

// USER_DEFINED_TYPE
#[derive(Debug, Clone)]
pub struct CoreUserDefinedTypeNode {
    pub name: IdentifierNode,
}

// CALLABLE_PROTOTYPE
// `(` [<params>] `)` [`->`] [<return_type>]
#[derive(Debug, Clone)]
pub struct CoreCallablePrototypeNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub right_arrow: Option<TokenNode>,
    pub params: Option<NameTypeSpecsNode>,
    pub return_type: Option<TypeExpressionNode>,
}

// CALLABLE_BODY
#[derive(Debug, Clone, Node)]
pub enum CoreCallableBodyNode {
    OK(OkCallableBodyNode),
    MISSING_TOKENS(MissingTokenNode),
}

// OK_CALLABLE_BODY
// `(` [<params>] `)` [`->`] [<return_type>] `:` <block>
#[derive(Debug, Clone)]
pub struct CoreOkCallableBodyNode {
    pub colon: TokenNode,
    pub block: BlockNode,
    pub prototype: CallablePrototypeNode,
}

// FUNCTION_DECLARATION
// `def` <name> <body>
#[derive(Debug, Clone)]
pub struct CoreFunctionDeclarationNode {
    pub def_keyword: TokenNode,
    pub name: IdentifierNode,
    pub kind: CallableKind,
    pub body: CallableBodyNode,
}

// LAMBDA_DECLARATION
// `lambda` `(` [<params>] `)` [`->`] [<return_type>] `:` <block>
#[derive(Debug, Clone)]
pub struct CoreLambdaDeclarationNode {
    pub lambda_keyword: TokenNode,
    pub name: IdentifierNode,
    pub body: CallableBodyNode,
}

// EXPRESSION_STATEMENT
// <expr> `\n`
#[derive(Debug, Clone)]
pub struct CoreExpressionStatementNode {
    pub expr: ExpressionNode,
    pub newline: TokenNode,
}

// EXPRESSION
#[derive(Debug, Clone, Node)]
pub enum CoreExpressionNode {
    UNARY(UnaryExpressionNode),
    BINARY(BinaryExpressionNode),
    COMPARISON(ComparisonNode),
    MISSING_TOKENS(MissingTokenNode),
}

// ATOMIC_EXPRESSION
#[derive(Debug, Clone, Node)]
pub enum CoreAtomicExpressionNode {
    BOOL_VALUE(TokenNode),
    INTEGER(TokenNode),
    FLOATING_POINT_NUMBER(TokenNode),
    LITERAL(TokenNode),
    PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode),
    ATOM(AtomNode),
    MISSING_TOKENS(MissingTokenNode),
}

// PARENTHESISED_EXPRESSION
// `(` <expr> `)`
#[derive(Debug, Clone)]
pub struct CoreParenthesisedExpressionNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub expr: ExpressionNode,
}

// UNARY_EXPRESSION
#[derive(Debug, Clone, Node)]
pub enum CoreUnaryExpressionNode {
    ATOMIC(AtomicExpressionNode),
    UNARY(OnlyUnaryExpressionNode),
    MISSING_TOKENS(MissingTokenNode),
}

// ONLY_UNARY_EXPRESSION
// <operator> <unary_expr>
#[derive(Debug, Clone)]
pub struct CoreOnlyUnaryExpressionNode {
    pub operator: TokenNode,
    pub unary_expr: UnaryExpressionNode,
    pub operator_kind: UnaryOperatorKind,
}

// BINARY_EXPRESSION
// <left_expr> <operator> <right_expr>
#[derive(Debug, Clone)]
pub struct CoreBinaryExpressionNode {
    pub operator_kind: BinaryOperatorKind,
    pub operator: TokenNode,
    pub left_expr: ExpressionNode,
    pub right_expr: ExpressionNode,
}

// COMPARISON
#[derive(Debug, Clone)]
pub struct CoreComparisonNode {
    pub operands: Vec<ExpressionNode>,
    pub operators: Vec<TokenNode>,
}

// CALL_EXPRESSION
// <function_name> `(` [<params>] `)`
#[derive(Debug, Clone)]
pub struct CoreCallExpressionNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub function_name: IdentifierNode,
    pub params: Option<ParamsNode>,
}

// ATOM
#[derive(Debug, Clone, Node)]
pub enum CoreAtomNode {
    ATOM_START(AtomStartNode),            // <id>, id(...), id::id(...)
    CALL(CallNode),                       // A(...)
    PROPERTRY_ACCESS(PropertyAccessNode), // A.id
    METHOD_ACCESS(MethodAccessNode),      // A.id(...)
    INDEX_ACCESS(IndexAccessNode),        // A[<expr>]
}

// ATOM_START
#[derive(Debug, Clone, Node)]
pub enum CoreAtomStartNode {
    IDENTIFIER(IdentifierNode),             // id
    CALL(CallExpressionNode),               // id(...)
    CLASS_METHOD_CALL(ClassMethodCallNode), // id::id(...)
}

// PROPERTY_ACCESS
// <atom> `.` <property>
#[derive(Debug, Clone)]
pub struct CorePropertyAccessNode {
    pub dot: TokenNode,
    pub atom: AtomNode,
    pub propertry: IdentifierNode,
}

// METHOD_ACCESS
// <atom> `.` <method_name> `(` [<params>] `)`
#[derive(Debug, Clone)]
pub struct CoreMethodAccessNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub dot: TokenNode,
    pub atom: AtomNode,
    pub method_name: IdentifierNode,
    pub params: Option<ParamsNode>,
}

// INDEX_ACCESS
// <atom> `[` <index> `]`
#[derive(Debug, Clone)]
pub struct CoreIndexAccessNode {
    pub lsquare: TokenNode,
    pub rsquare: TokenNode,
    pub atom: AtomNode,
    pub index: ExpressionNode,
}

// CALL
// <atom> `(` [<params>] `)`
#[derive(Debug, Clone)]
pub struct CoreCallNode {
    pub atom: AtomNode,
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub params: Option<ParamsNode>,
}

// CLASS_METHOD_CALL
// <class_name> `::` <class_method_name> `(` [<params>] `)`
#[derive(Debug, Clone)]
pub struct CoreClassMethodCallNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub double_colon: TokenNode,
    pub class_name: IdentifierNode,
    pub class_method_name: IdentifierNode,
    pub params: Option<ParamsNode>,
}

// NAME_TYPE_SPECS
#[derive(Debug, Clone, Node)]
pub enum CoreNameTypeSpecsNode {
    OK(OkNameTypeSpecsNode),
    MISSING_TOKENS(MissingTokenNode),
}

// OK_NAME_TYPE_SPECS
// <arg> `,` <remaining_args>
#[derive(Debug, Clone)]
pub struct CoreOkNameTypeSpecsNode {
    pub comma: Option<TokenNode>,
    pub arg: NameTypeSpecNode,
    pub remaining_args: Option<NameTypeSpecsNode>,
}

// NAME_TYPE_SPEC
// <name> `:` <data_type>
#[derive(Debug, Clone)]
pub struct CoreNameTypeSpecNode {
    pub colon: TokenNode,
    pub name: IdentifierNode,
    pub data_type: TypeExpressionNode,
}

// TYPE_TUPLE
#[derive(Debug, Clone, Node)]
pub enum CoreTypeTupleNode {
    OK(OkTypeTupleNode),
    MISSING_TOKENS(MissingTokenNode),
}

// OK_TYPE_TUPLE
// <data_type> `,` <remaining_types>
#[derive(Debug, Clone)]
pub struct CoreOkTypeTupleNode {
    pub comma: Option<TokenNode>,
    pub data_type: TypeExpressionNode,
    pub remaining_types: Option<TypeTupleNode>,
}

// PARAMS
#[derive(Debug, Clone, Node)]
pub enum CoreParamsNode {
    OK(OkParamsNode),
    MISSING_TOKENS(MissingTokenNode),
}

// OK_PARAMS
// <param> `,` <remaining_params>
#[derive(Debug, Clone)]
pub struct CoreOkParamsNode {
    pub comma: Option<TokenNode>,
    pub param: ExpressionNode,
    pub remaining_params: Option<ParamsNode>,
}

// IDENTIFIER
#[derive(Debug, Clone, Node)]
pub enum CoreIdentifierNode {
    OK(OkIdentifierNode),
    MISSING_TOKENS(MissingTokenNode),
    SKIPPED(SkippedTokenNode),
}

// OK_IDENTIFIER
#[derive(Debug, Clone)]
pub struct CoreOkIdentifierNode {
    pub token: TokenNode,
    pub decl: Option<(IdentifierKind, usize)>, // (symbol data reference, depth)
}

// TOKEN
#[derive(Debug, Clone, Node)]
pub enum CoreTokenNode {
    OK(OkTokenNode),
    MISSING_TOKENS(MissingTokenNode),
    SKIPPED(SkippedTokenNode),
}

// OK_TOKEN
#[derive(Debug, Clone)]
pub struct CoreOkTokenNode {
    pub token: Token,
}

// MISSING_TOKEN
#[derive(Debug, Clone)]
pub struct CoreMissingTokenNode {
    pub expected_symbols: Rc<Vec<&'static str>>,
    pub received_token: Token,
}

// SKIPPED_TOKEN
#[derive(Debug, Clone)]
pub struct CoreSkippedTokenNode {
    pub skipped_token: Token,
}

// core node wrapper
#[derive(Debug, Clone)]
pub struct BlockNode(pub Rc<RefCell<CoreBlockNode>>);
#[derive(Debug, Clone)]
pub struct StatemenIndentWrapperNode(pub Rc<CoreStatemenIndentWrapperNode>);
#[derive(Debug, Clone)]
pub struct SkippedTokensNode(pub Rc<CoreSkippedTokensNode>);
#[derive(Debug, Clone)]
pub struct IncorrectlyIndentedStatementNode(pub Rc<CoreIncorrectlyIndentedStatementNode>);
#[derive(Debug, Clone)]
pub struct StatementNode(pub Rc<CoreStatementNode>);
#[derive(Debug, Clone)]
pub struct ReturnStatementNode(pub Rc<CoreReturnStatementNode>);
#[derive(Debug, Clone)]
pub struct VariableDeclarationNode(pub Rc<CoreVariableDeclarationNode>);
#[derive(Debug, Clone)]
pub struct AssignmentNode(pub Rc<CoreAssignmentNode>);
#[derive(Debug, Clone)]
pub struct OkAssignmentNode(pub Rc<CoreOkAssignmentNode>);
#[derive(Debug, Clone)]
pub struct InvalidLValueNode(pub Rc<CoreInvalidLValueNode>);
#[derive(Debug, Clone)]
pub struct InvalidRLambdaNode(pub Rc<CoreInvalidRLambdaNode>);
#[derive(Debug, Clone)]
pub struct RAssignmentNode(pub Rc<CoreRAssignmentNode>);
#[derive(Debug, Clone)]
pub struct RVariableDeclarationNode(pub Rc<CoreRVariableDeclarationNode>);
#[derive(Debug, Clone)]
pub struct TypeDeclarationNode(pub Rc<CoreTypeDeclarationNode>);
#[derive(Debug, Clone)]
pub struct StructDeclarationNode(pub Rc<CoreStructDeclarationNode>);
#[derive(Debug, Clone)]
pub struct StructStatementNode(pub Rc<CoreStructStatementNode>);
#[derive(Debug, Clone)]
pub struct LambdaTypeDeclarationNode(pub Rc<CoreLambdaTypeDeclarationNode>);
#[derive(Debug, Clone)]
pub struct OkLambdaTypeDeclarationNode(pub Rc<CoreOkLambdaTypeDeclarationNode>);
#[derive(Debug, Clone)]
pub struct TypeExpressionNode(pub Rc<CoreTypeExpressionNode>);
#[derive(Debug, Clone)]
pub struct AtomicTypeNode(pub Rc<CoreAtomicTypeNode>);
#[derive(Debug, Clone)]
pub struct ArrayTypeNode(pub Rc<CoreArrayTypeNode>);
#[derive(Debug, Clone)]
pub struct UserDefinedTypeNode(pub Rc<CoreUserDefinedTypeNode>);
#[derive(Debug, Clone)]
pub struct CallablePrototypeNode(pub Rc<CoreCallablePrototypeNode>);
#[derive(Debug, Clone)]
pub struct CallableBodyNode(pub Rc<CoreCallableBodyNode>);
#[derive(Debug, Clone)]
pub struct OkCallableBodyNode(pub Rc<CoreOkCallableBodyNode>);
#[derive(Debug, Clone)]
pub struct FunctionDeclarationNode(pub Rc<CoreFunctionDeclarationNode>);
#[derive(Debug, Clone)]
pub struct LambdaDeclarationNode(pub Rc<CoreLambdaDeclarationNode>);
#[derive(Debug, Clone)]
pub struct ExpressionStatementNode(pub Rc<CoreExpressionStatementNode>);
#[derive(Debug, Clone)]
pub struct ExpressionNode(pub Rc<CoreExpressionNode>);
#[derive(Debug, Clone)]
pub struct AtomicExpressionNode(pub Rc<CoreAtomicExpressionNode>);
#[derive(Debug, Clone)]
pub struct ParenthesisedExpressionNode(pub Rc<CoreParenthesisedExpressionNode>);
#[derive(Debug, Clone)]
pub struct UnaryExpressionNode(pub Rc<CoreUnaryExpressionNode>);
#[derive(Debug, Clone)]
pub struct OnlyUnaryExpressionNode(pub Rc<CoreOnlyUnaryExpressionNode>);
#[derive(Debug, Clone)]
pub struct BinaryExpressionNode(pub Rc<CoreBinaryExpressionNode>);
#[derive(Debug, Clone)]
pub struct ComparisonNode(pub Rc<CoreComparisonNode>);
#[derive(Debug, Clone)]
pub struct CallExpressionNode(pub Rc<CoreCallExpressionNode>);
#[derive(Debug, Clone)]
pub struct AtomNode(pub Rc<CoreAtomNode>);
#[derive(Debug, Clone)]
pub struct AtomStartNode(pub Rc<CoreAtomStartNode>);
#[derive(Debug, Clone)]
pub struct PropertyAccessNode(pub Rc<CorePropertyAccessNode>);
#[derive(Debug, Clone)]
pub struct MethodAccessNode(pub Rc<CoreMethodAccessNode>);
#[derive(Debug, Clone)]
pub struct IndexAccessNode(pub Rc<CoreIndexAccessNode>);
#[derive(Debug, Clone)]
pub struct CallNode(pub Rc<CoreCallNode>);
#[derive(Debug, Clone)]
pub struct ClassMethodCallNode(pub Rc<CoreClassMethodCallNode>);
#[derive(Debug, Clone)]
pub struct NameTypeSpecsNode(pub Rc<CoreNameTypeSpecsNode>);
#[derive(Debug, Clone)]
pub struct OkNameTypeSpecsNode(pub Rc<CoreOkNameTypeSpecsNode>);
#[derive(Debug, Clone)]
pub struct NameTypeSpecNode(pub Rc<CoreNameTypeSpecNode>);
#[derive(Debug, Clone)]
pub struct TypeTupleNode(pub Rc<CoreTypeTupleNode>);
#[derive(Debug, Clone)]
pub struct OkTypeTupleNode(pub Rc<CoreOkTypeTupleNode>);
#[derive(Debug, Clone)]
pub struct ParamsNode(pub Rc<CoreParamsNode>);
#[derive(Debug, Clone)]
pub struct OkParamsNode(pub Rc<CoreOkParamsNode>);
#[derive(Debug, Clone)]
pub struct IdentifierNode(pub Rc<CoreIdentifierNode>);
#[derive(Debug, Clone)]
pub struct OkIdentifierNode(pub Rc<RefCell<CoreOkIdentifierNode>>);
#[derive(Debug, Clone)]
pub struct TokenNode(pub Rc<CoreTokenNode>);
#[derive(Debug, Clone)]
pub struct OkTokenNode(pub Rc<CoreOkTokenNode>);
#[derive(Debug, Clone)]
pub struct MissingTokenNode(pub Rc<CoreMissingTokenNode>);
#[derive(Debug, Clone)]
pub struct SkippedTokenNode(pub Rc<CoreSkippedTokenNode>);

// misc "kind" enums
#[derive(Debug, Clone, PartialEq)]
pub enum BlockKind {
    TOP,
    FUNC,
    STRUCT,
}
pub enum TypeResolveKind {
    RESOLVED(Type),
    UNRESOLVED(OkIdentifierNode),
    INVALID,
}
#[derive(Debug, Clone, PartialEq)]
pub enum CallableKind {
    FUNC,
    METHOD, // Add the symbol entry of the struct for which this is a method
    CLASSMETHOD,
    CONSTRUCTOR,
}
