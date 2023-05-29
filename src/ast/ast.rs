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
use crate::{lexer::token::Token, types::core::Type};
use std::rc::Rc;
use text_size::{TextRange, TextSize};

pub trait Node {
    fn range(&self) -> TextRange;
    fn start_line_number(&self) -> usize;
}

pub trait ErrornousNode {
    fn new_with_missing_tokens(
        expected_symbols: &Vec<&'static str>,
        received_token: &Token,
    ) -> Self;
}

#[derive(Debug, Clone, Nodify)]
pub enum ASTNode {
    Block(BlockNode),

    // Statements
    StatementIndentWrapper(StatemenIndentWrapperNode),
    SkippedTokens(SkippedTokensNode),
    IncorrectlyIndentedStatement(IncorrectlyIndentedStatementNode),
    Statement(StatementNode),
    Return(ReturnStatementNode),

    // Declaration `n` Assignment
    VariableDeclaration(VariableDeclarationNode),
    Assignment(AssignmentNode),
    OkAssignment(OkAssignmentNode),
    InvalidLValue(InvalidLValueNode),
    RAssignment(RAssignmentNode),
    RVariableDeclaration(RVariableDeclarationNode),

    // Types
    TypeDeclaration(TypeDeclarationNode),
    StructDeclaration(StructDeclarationNode),
    StructPropertyDeclaration(StructPropertyDeclarationNode),
    LambdaTypeDeclaration(LambdaTypeDeclarationNode),
    OkLambdaTypeDeclaration(OkLambdaTypeDeclarationNode),
    TypeExpression(TypeExpressionNode),
    AtomicType(AtomicTypeNode),
    ArrayType(ArrayTypeNode),
    TupleType(TupleTypeNode),
    HashmapType(HashMapTypeNode),
    UserDefinedType(UserDefinedTypeNode),

    // Callable
    CallablePrototype(CallablePrototypeNode),
    CallableBody(CallableBodyNode),
    OkCallableBody(OkCallableBodyNode),
    FunctionDeclaration(FunctionDeclarationNode),
    FunctionWrapper(FunctionWrapperNode),
    BoundedMethodWrapper(BoundedMethodWrapperNode),
    LambdaDeclaration(LambdaDeclarationNode),

    // Expression
    ExpressionStatement(ExpressionStatementNode),
    Expression(ExpressionNode),
    AtomicExpression(AtomicExpressionNode),
    ParenthesisedExpression(ParenthesisedExpressionNode),
    UnaryExpression(UnaryExpressionNode),
    OnlyUnaryExpression(OnlyUnaryExpressionNode),
    BinaryExpression(BinaryExpressionNode),
    Comparison(ComparisonNode),
    CallExpression(CallExpressionNode),

    // Atom
    Atom(AtomNode),
    AtomStart(AtomStartNode),
    PropertyAccess(PropertyAccessNode),
    MethodAccess(MethodAccessNode),
    IndexAccess(IndexAccessNode),
    Call(CallNode),
    ClassMethodCall(ClassMethodCallNode),

    // General
    NameTypeSpecs(NameTypeSpecsNode),
    OkNameTypeSpecs(OkNameTypeSpecsNode),
    NameTypeSpec(NameTypeSpecNode),
    TypeTuple(TypeTupleNode),
    OkTypeTuple(OkTypeTupleNode),
    Params(ParamsNode),
    OkParams(OkParamsNode),

    // Basic
    Identifier(IdentifierNode),
    OkIdentifier(OkIdentifierNode),
    SelfKeyword(SelfKeywordNode),
    OkSelfKeyword(OkSelfKeywordNode),
    Token(TokenNode),
    OkToken(OkTokenNode),
    MissingToken(MissingTokenNode),
    SkippedToken(SkippedTokenNode),
}

// core nodes containing the structure for storing concrete syntax
// BLOCK
// `\n` <stmts>
#[derive(Debug, Clone)]
pub struct CoreBlockNode {
    pub newline: TokenNode,
    pub stmts: Rc<Vec<StatemenIndentWrapperNode>>,
}

// STATEMENT_INDENT_WRAPPER
#[derive(Debug, Clone, Node)]
pub enum CoreStatemenIndentWrapperNode {
    CorrectlyIndented(StatementNode),
    IncorrectlyIndented(IncorrectlyIndentedStatementNode),
    LeadingSkippedTokens(SkippedTokensNode), // skipped tokens leading to the next stmt in block
    TrailingSkippedTokens(SkippedTokensNode), // skipped tokens trailing to the previous stmt in block
    ExtraNewlines(SkippedTokensNode),
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
    Expression(ExpressionStatementNode),
    Assignment(AssignmentNode),
    VariableDeclaration(VariableDeclarationNode),
    Return(ReturnStatementNode),
    FunctionWrapper(FunctionWrapperNode),
    BoundedMethodWrapper(BoundedMethodWrapperNode),
    TypeDeclaration(TypeDeclarationNode),
    StructPropertyDeclaration(StructPropertyDeclarationNode),
    MissingTokens(MissingTokenNode),
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
    Ok(OkAssignmentNode),
    InvalidLValue(InvalidLValueNode),
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

// R_ASSIGNMENT
#[derive(Debug, Clone, Node)]
pub enum CoreRAssignmentNode {
    Expression(ExpressionStatementNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Clone, Node)]
pub enum CoreRVariableDeclarationNode {
    Lambda(LambdaDeclarationNode),
    Expression(ExpressionStatementNode),
    MissingTokens(MissingTokenNode),
}

// TYPE_DECLARATION
#[derive(Debug, Clone, Node)]
pub enum CoreTypeDeclarationNode {
    Struct(StructDeclarationNode),
    Lambda(LambdaTypeDeclarationNode),
    MissingTokens(MissingTokenNode),
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
pub struct CoreStructPropertyDeclarationNode {
    pub newline: TokenNode,
    pub name_type_spec: NameTypeSpecNode,
}

// LAMBDA_TYPE_DECLARATION
#[derive(Debug, Clone, Node)]
pub enum CoreLambdaTypeDeclarationNode {
    Ok(OkLambdaTypeDeclarationNode),
    MissingTokens(MissingTokenNode),
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
    Atomic(AtomicTypeNode),
    UserDefined(UserDefinedTypeNode),
    Array(ArrayTypeNode),
    Tuple(TupleTypeNode),
    HashMap(HashMapTypeNode),
    MissingTokens(MissingTokenNode),
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

// TUPLE_TYPE
// `(` <types> `)`
#[derive(Debug, Clone)]
pub struct CoreTupleTypeNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub types: TypeTupleNode,
}

// DICTIONARY_TYPE
// `{` <key_type> `:` <value_type> `}`
#[derive(Debug, Clone)]
pub struct CoreHashMapTypeNode {
    pub lcurly: TokenNode,
    pub rcurly: TokenNode,
    pub colon: TokenNode,
    pub key_type: TypeExpressionNode,
    pub value_type: TypeExpressionNode,
}

// USER_DEFINED_TYPE
#[derive(Debug, Clone)]
pub struct CoreUserDefinedTypeNode {
    pub name: IdentifierNode,
}

// CALLABLE_PROTOTYPE
// `(` [<params>] `)` [`->` <return_type>]
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
    Ok(OkCallableBodyNode),
    MissingTokens(MissingTokenNode),
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
    pub body: CallableBodyNode,
}

#[derive(Debug, Clone)]
pub struct CoreFunctionWrapperNode {
    pub func_decl: FunctionDeclarationNode,
}

#[derive(Debug, Clone)]
pub struct CoreBoundedMethodWrapperNode {
    pub func_decl: FunctionDeclarationNode,
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
    Unary(UnaryExpressionNode),
    Binary(BinaryExpressionNode),
    Comparison(ComparisonNode),
    MissingTokens(MissingTokenNode),
}

// ATOMIC_EXPRESSION
#[derive(Debug, Clone, Node)]
pub enum CoreAtomicExpressionNode {
    BoolValue(TokenNode),
    Integer(TokenNode),
    FloatingPointNumber(TokenNode),
    Literal(TokenNode),
    ParenthesisedExpression(ParenthesisedExpressionNode),
    Atom(AtomNode),
    MissingTokens(MissingTokenNode),
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
    Atomic(AtomicExpressionNode),
    Unary(OnlyUnaryExpressionNode),
    MissingTokens(MissingTokenNode),
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
    AtomStart(AtomStartNode),           // <id>, id(...), id::id(...)
    Call(CallNode),                     // A(...)
    PropertyAccess(PropertyAccessNode), // A.id
    MethodAccess(MethodAccessNode),     // A.id(...)
    IndexAccess(IndexAccessNode),       // A[<expr>]
}

// ATOM_START
#[derive(Debug, Clone, Node)]
pub enum CoreAtomStartNode {
    Identifier(IdentifierNode),           // id
    SelfKeyword(SelfKeywordNode),         // self
    Call(CallExpressionNode),             // id(...)
    ClassMethodCall(ClassMethodCallNode), // id::id(...)
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
    Ok(OkNameTypeSpecsNode),
    MissingTokens(MissingTokenNode),
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
    Ok(OkTypeTupleNode),
    MissingTokens(MissingTokenNode),
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
    Ok(OkParamsNode),
    MissingTokens(MissingTokenNode),
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
    Ok(OkIdentifierNode),
    MissingTokens(MissingTokenNode),
}

// OK_IDENTIFIER
#[derive(Debug, Clone)]
pub struct CoreOkIdentifierNode {
    pub token: OkTokenNode,
}

#[derive(Debug, Clone, Node)]
pub enum CoreSelfKeywordNode {
    Ok(OkSelfKeywordNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct CoreOkSelfKeywordNode {
    pub token: OkTokenNode,
}

// TOKEN
#[derive(Debug, Clone, Node)]
pub enum CoreTokenNode {
    Ok(OkTokenNode),
    MissingTokens(MissingTokenNode),
}

// OK_TOKEN
#[derive(Debug, Clone)]
pub struct CoreOkTokenNode {
    pub token: Token,
}

// MISSING_TOKEN
#[derive(Debug, Clone)]
pub struct CoreMissingTokenNode {
    pub expected_symbols: Vec<&'static str>,
    pub received_token: Token,
}

// SKIPPED_TOKEN
#[derive(Debug, Clone)]
pub struct CoreSkippedTokenNode {
    pub skipped_token: Token,
}

// core node wrapper
#[derive(Debug, Clone)]
pub struct BlockNode(pub Rc<CoreBlockNode>);
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
pub struct RAssignmentNode(pub Rc<CoreRAssignmentNode>);
#[derive(Debug, Clone)]
pub struct RVariableDeclarationNode(pub Rc<CoreRVariableDeclarationNode>);
#[derive(Debug, Clone)]
pub struct TypeDeclarationNode(pub Rc<CoreTypeDeclarationNode>);
#[derive(Debug, Clone)]
pub struct StructDeclarationNode(pub Rc<CoreStructDeclarationNode>);
#[derive(Debug, Clone)]
pub struct StructPropertyDeclarationNode(pub Rc<CoreStructPropertyDeclarationNode>);
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
pub struct TupleTypeNode(pub Rc<CoreTupleTypeNode>);
#[derive(Debug, Clone)]
pub struct HashMapTypeNode(pub Rc<CoreHashMapTypeNode>);
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
pub struct FunctionWrapperNode(pub Rc<CoreFunctionWrapperNode>);
#[derive(Debug, Clone)]
pub struct BoundedMethodWrapperNode(pub Rc<CoreBoundedMethodWrapperNode>);
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
pub struct OkIdentifierNode(pub Rc<CoreOkIdentifierNode>);
#[derive(Debug, Clone)]
pub struct SelfKeywordNode(pub Rc<CoreSelfKeywordNode>);
#[derive(Debug, Clone)]
pub struct OkSelfKeywordNode(pub Rc<CoreOkSelfKeywordNode>);
#[derive(Debug, Clone)]
pub struct TokenNode(pub Rc<CoreTokenNode>);
#[derive(Debug, Clone)]
pub struct OkTokenNode(pub Rc<CoreOkTokenNode>);
#[derive(Debug, Clone)]
pub struct MissingTokenNode(pub Rc<CoreMissingTokenNode>);
#[derive(Debug, Clone)]
pub struct SkippedTokenNode(pub Rc<CoreSkippedTokenNode>);

// misc "kind" enums

pub enum TypeResolveKind {
    Resolved(Type),
    Unresolved(Vec<OkIdentifierNode>),
    Invalid,
}

#[derive(Debug, Clone)]
pub enum BoundedMethodKind {
    Constructor,
    Method,
    ClassMethod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallableKind {
    Function,
    Method,
}
