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
        expected_symbols: Vec<&'static str>,
        received_token: &Token,
    ) -> Self;
}

#[derive(Debug, Clone, Nodify)]
pub enum ASTNode {
    Block(BlockNode),
    StatementIndentWrapper(StatemenIndentWrapperNode),
    SkippedTokens(SkippedTokensNode),
    IncorrectlyIndentedStatement(IncorrectlyIndentedStatementNode),
    Statement(StatementNode),
    Return(ReturnStatementNode),
    VariableDeclaration(VariableDeclarationNode),
    Assignment(AssignmentNode),
    OkAssignment(OkAssignmentNode),
    InvalidLValue(InvalidLValueNode),
    RAssignment(RAssignmentNode),
    RVariableDeclaration(RVariableDeclarationNode),
    TypeDeclaration(TypeDeclarationNode),
    StructDeclaration(StructDeclarationNode),
    StructPropertyDeclaration(StructPropertyDeclarationNode),
    LambdaTypeDeclaration(LambdaTypeDeclarationNode),
    TypeExpression(TypeExpressionNode),
    AtomicType(AtomicTypeNode),
    ArrayType(ArrayTypeNode),
    TupleType(TupleTypeNode),
    HashmapType(HashMapTypeNode),
    UserDefinedType(UserDefinedTypeNode),
    CallablePrototype(CallablePrototypeNode),
    CallableBody(CallableBodyNode),
    FunctionDeclaration(FunctionDeclarationNode),
    FunctionWrapper(FunctionWrapperNode),
    BoundedMethodWrapper(BoundedMethodWrapperNode),
    LambdaDeclaration(LambdaDeclarationNode),
    ExpressionStatement(ExpressionStatementNode),
    TypeTuple(SymbolSeparatedSequenceNode<TypeExpressionNode>),
    Expression(ExpressionNode),
    AtomicExpression(AtomicExpressionNode),
    ParenthesisedExpression(ParenthesisedExpressionNode),
    UnaryExpression(UnaryExpressionNode),
    OnlyUnaryExpression(OnlyUnaryExpressionNode),
    BinaryExpression(BinaryExpressionNode),
    Comparison(ComparisonNode),
    CallExpression(CallExpressionNode),
    Atom(AtomNode),
    AtomStart(AtomStartNode),
    PropertyAccess(PropertyAccessNode),
    MethodAccess(MethodAccessNode),
    IndexAccess(IndexAccessNode),
    Call(CallNode),
    ClassMethodCall(ClassMethodCallNode),
    NameTypeSpecs(SymbolSeparatedSequenceNode<NameTypeSpecNode>),
    NameTypeSpec(NameTypeSpecNode),
    Params(SymbolSeparatedSequenceNode<ExpressionNode>),
    Identifier(IdentifierNode),
    IdentifierInUse(IdentifierInUseNode),
    IdentifierInDecl(IdentifierInDeclNode),
    OkIdentifierInUse(OkIdentifierInUseNode),
    OkIdentifierInDecl(OkIdentifierInDeclNode),
    GenericTypeDecl(GenericTypeDeclNode),
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
// newline <stmts>
#[derive(Debug)]
pub struct CoreBlockNode {
    pub newline: TokenNode,
    pub stmts: Rc<Vec<StatemenIndentWrapperNode>>,
}

// STATEMENT_INDENT_WRAPPER
#[derive(Debug, Node)]
pub enum CoreStatemenIndentWrapperNode {
    CorrectlyIndented(StatementNode),
    IncorrectlyIndented(IncorrectlyIndentedStatementNode),
    LeadingSkippedTokens(SkippedTokensNode), // skipped tokens leading to the next stmt in block
    TrailingSkippedTokens(SkippedTokensNode), // skipped tokens trailing to the previous stmt in block
    ExtraNewlines(SkippedTokensNode),
}

// SKIPPED_TOKENS
#[derive(Debug)]
pub struct CoreSkippedTokensNode {
    pub skipped_tokens: Vec<SkippedTokenNode>,
}

// INCORRECTLY_INDENTED_STATEMENT
#[derive(Debug)]
pub struct CoreIncorrectlyIndentedStatementNode {
    pub stmt: StatementNode,
    pub expected_indent: i64,
    pub received_indent: i64,
}

// STATEMENT
#[derive(Debug, Node)]
pub enum CoreStatementNode {
    Expression(ExpressionStatementNode),
    Assignment(AssignmentNode),
    VariableDeclaration(VariableDeclarationNode),
    Return(ReturnStatementNode),
    FunctionWrapper(FunctionWrapperNode),
    BoundedMethodWrapper(BoundedMethodWrapperNode),
    TypeDeclaration(TypeDeclarationNode),
    StructPropertyDeclaration(StructPropertyDeclarationNode),
}

// RETURN
// `return` <expr>? newline
#[derive(Debug)]
pub struct CoreReturnStatementNode {
    pub return_keyword: TokenNode,
    pub expr: Option<ExpressionNode>,
    pub newline: TokenNode,
}

#[derive(Debug)]
pub struct CoreInterfaceMethodDeclaration {
    pub name: IdentifierInDeclNode,
    pub prototype: CallablePrototypeNode,
    pub default_block: Option<(TokenNode, BlockNode)>, // (`:`, block for default implementation)
}

// VARIABLE_DECLARATION
// `let` <name> `=` <r_node>
#[derive(Debug)]
pub struct CoreVariableDeclarationNode {
    pub let_keyword: TokenNode,
    pub equal: TokenNode,
    pub name: IdentifierNode,
    pub r_node: RVariableDeclarationNode,
}

// ASSIGNMENT
#[derive(Debug, Node)]
pub enum CoreAssignmentNode {
    Ok(OkAssignmentNode),
    InvalidLValue(InvalidLValueNode),
}

// OK_ASSIGNMENT
// <l_atom> `=` <r_assign>
#[derive(Debug)]
pub struct CoreOkAssignmentNode {
    pub equal: TokenNode,
    pub l_atom: AtomNode,
    pub r_assign: RAssignmentNode,
}

// INVALID_L_VALUE
// <l_expr> `=` <r_assign>
#[derive(Debug)]
pub struct CoreInvalidLValueNode {
    pub l_expr: ExpressionNode,
    pub equal: TokenNode,
    pub r_assign: RAssignmentNode,
}

// R_ASSIGNMENT
#[derive(Debug)]
pub struct CoreRAssignmentNode {
    pub expr: ExpressionStatementNode,
}

#[derive(Debug, Node)]
pub enum CoreRVariableDeclarationNode {
    Lambda(LambdaDeclarationNode),
    Expression(ExpressionStatementNode),
}

// TYPE_DECLARATION
#[derive(Debug, Node)]
pub enum CoreTypeDeclarationNode {
    Struct(StructDeclarationNode),
    Lambda(LambdaTypeDeclarationNode),
    MissingTokens(MissingTokenNode),
}

// STRUCT_DECLARATION
// `type` <name> struct `:` <block>
#[derive(Debug)]
pub struct CoreStructDeclarationNode {
    pub type_keyword: TokenNode,
    pub colon: TokenNode,
    pub struct_keyword: TokenNode,
    pub name: IdentifierNode,
    pub block: BlockNode,
}

// STRUCT_STATEMENT
// <name_type_spec> newline
#[derive(Debug)]
pub struct CoreStructPropertyDeclarationNode {
    pub newline: TokenNode,
    pub name_type_spec: NameTypeSpecNode,
}

// LAMBDA_TYPE_DECLARATION
// `type` <name> lambda `=` `(` <type_tuple>? `)` { `->` <return_type> }? newline
#[derive(Debug)]
pub struct CoreLambdaTypeDeclarationNode {
    pub type_keyword: TokenNode,
    pub lambda_keyword: TokenNode,
    pub name: IdentifierNode,
    pub equal: TokenNode,
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub type_tuple: Option<SymbolSeparatedSequenceNode<TypeExpressionNode>>,
    pub right_arrow: Option<TokenNode>,
    pub return_type: Option<TypeExpressionNode>,
    pub newline: TokenNode,
}

// TYPE_EXPRESSION
#[derive(Debug, Node)]
pub enum CoreTypeExpressionNode {
    Atomic(AtomicTypeNode),
    UserDefined(UserDefinedTypeNode),
    Array(ArrayTypeNode),
    Tuple(TupleTypeNode),
    HashMap(HashMapTypeNode),
    MissingTokens(MissingTokenNode),
}

// ATOMIC_TYPE
// `int` | `float` | `string` | `bool`
#[derive(Debug)]
pub struct CoreAtomicTypeNode {
    pub kind: TokenNode,
}

// ARRAY_TYPE
// `[` <sub_type> `]`
#[derive(Debug)]
pub struct CoreArrayTypeNode {
    pub lsquare: TokenNode,
    pub rsquare: TokenNode,
    pub sub_type: TypeExpressionNode,
}

// TUPLE_TYPE
// `(` <types> `)`
#[derive(Debug)]
pub struct CoreTupleTypeNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub types: SymbolSeparatedSequenceNode<TypeExpressionNode>,
}

// DICTIONARY_TYPE
// `{` <key_type> `:` <value_type> `}`
#[derive(Debug)]
pub struct CoreHashMapTypeNode {
    pub lcurly: TokenNode,
    pub rcurly: TokenNode,
    pub colon: TokenNode,
    pub key_type: TypeExpressionNode,
    pub value_type: TypeExpressionNode,
}

// USER_DEFINED_TYPE
#[derive(Debug)]
pub struct CoreUserDefinedTypeNode {
    pub name: IdentifierNode,
}

// CALLABLE_PROTOTYPE
// `(` <params>? `)` { `->` <return_type> }?
#[derive(Debug)]
pub struct CoreCallablePrototypeNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub params: Option<SymbolSeparatedSequenceNode<NameTypeSpecNode>>,
    pub return_type: Option<(TokenNode, TypeExpressionNode)>, // (`->`, <type_expr>)
}

// CALLABLE_BODY
// <prototype> `:` <block>
#[derive(Debug)]
pub struct CoreCallableBodyNode {
    pub colon: TokenNode,
    pub block: BlockNode,
    pub prototype: CallablePrototypeNode,
}

// FUNCTION_DECLARATION
// `def` <name> <body>
#[derive(Debug)]
pub struct CoreFunctionDeclarationNode {
    pub def_keyword: TokenNode,
    pub name: IdentifierNode,
    pub body: CallableBodyNode,
}

#[derive(Debug)]
pub struct CoreFunctionWrapperNode {
    pub func_decl: FunctionDeclarationNode,
}

#[derive(Debug)]
pub struct CoreBoundedMethodWrapperNode {
    pub func_decl: FunctionDeclarationNode,
}

// LAMBDA_DECLARATION
// `lambda` `(` <params>? `)` { `->` <return_type> }? `:` <block>
#[derive(Debug)]
pub struct CoreLambdaDeclarationNode {
    pub lambda_keyword: TokenNode,
    pub name: IdentifierNode,
    pub body: CallableBodyNode,
}

// EXPRESSION_STATEMENT
// <expr> newline
#[derive(Debug)]
pub struct CoreExpressionStatementNode {
    pub expr: ExpressionNode,
    pub newline: TokenNode,
}

// EXPRESSION
#[derive(Debug, Node)]
pub enum CoreExpressionNode {
    Unary(UnaryExpressionNode),
    Binary(BinaryExpressionNode),
    Comparison(ComparisonNode),
}

// ATOMIC_EXPRESSION
#[derive(Debug, Node)]
pub enum CoreAtomicExpressionNode {
    Bool(TokenNode),
    Integer(TokenNode),
    FloatingPointNumber(TokenNode),
    Literal(TokenNode),
    ParenthesisedExpression(ParenthesisedExpressionNode),
    Atom(AtomNode),
    MissingTokens(MissingTokenNode),
}

// PARENTHESISED_EXPRESSION
// `(` <expr> `)`
#[derive(Debug)]
pub struct CoreParenthesisedExpressionNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub expr: ExpressionNode,
}

// UNARY_EXPRESSION
#[derive(Debug, Node)]
pub enum CoreUnaryExpressionNode {
    Atomic(AtomicExpressionNode),
    Unary(OnlyUnaryExpressionNode),
}

// ONLY_UNARY_EXPRESSION
// <operator> <unary_expr>
#[derive(Debug)]
pub struct CoreOnlyUnaryExpressionNode {
    pub operator: TokenNode,
    pub unary_expr: UnaryExpressionNode,
    pub operator_kind: UnaryOperatorKind,
}

// BINARY_EXPRESSION
// <left_expr> <operator> <right_expr>
#[derive(Debug)]
pub struct CoreBinaryExpressionNode {
    pub operator_kind: BinaryOperatorKind,
    pub operator: TokenNode,
    pub left_expr: ExpressionNode,
    pub right_expr: ExpressionNode,
}

// COMPARISON
#[derive(Debug)]
pub struct CoreComparisonNode {
    pub operands: Vec<ExpressionNode>,
    pub operators: Vec<TokenNode>,
}

// CALL_EXPRESSION
// <function_name> `(` <params>? `)`
#[derive(Debug)]
pub struct CoreCallExpressionNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub function_name: IdentifierNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
}

// ATOM
#[derive(Debug, Node)]
pub enum CoreAtomNode {
    AtomStart(AtomStartNode),           // id, id(...), id::id(...), `self`
    Call(CallNode),                     // A(...)
    PropertyAccess(PropertyAccessNode), // A.id
    MethodAccess(MethodAccessNode),     // A.id(...)
    IndexAccess(IndexAccessNode),       // A[<expr>]
}

// ATOM_START
#[derive(Debug, Node)]
pub enum CoreAtomStartNode {
    Identifier(IdentifierNode),           // id
    SelfKeyword(SelfKeywordNode),         // self
    Call(CallExpressionNode),             // id(...)
    ClassMethodCall(ClassMethodCallNode), // id::id(...)
}

// PROPERTY_ACCESS
// <atom> `.` <property>
#[derive(Debug)]
pub struct CorePropertyAccessNode {
    pub dot: TokenNode,
    pub atom: AtomNode,
    pub propertry: IdentifierNode,
}

// METHOD_ACCESS
// <atom> `.` <method_name> `(` <params>? `)`
#[derive(Debug)]
pub struct CoreMethodAccessNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub dot: TokenNode,
    pub atom: AtomNode,
    pub method_name: IdentifierNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
}

// INDEX_ACCESS
// <atom> `[` <index> `]`
#[derive(Debug)]
pub struct CoreIndexAccessNode {
    pub lsquare: TokenNode,
    pub rsquare: TokenNode,
    pub atom: AtomNode,
    pub index: ExpressionNode,
}

// CALL
// <atom> `(` <params>? `)`
#[derive(Debug)]
pub struct CoreCallNode {
    pub atom: AtomNode,
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
}

// CLASS_METHOD_CALL
// <class_name> `::` <class_method_name> `(` <params>? `)`
#[derive(Debug)]
pub struct CoreClassMethodCallNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub double_colon: TokenNode,
    pub class_name: IdentifierNode,
    pub class_method_name: IdentifierNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
}

// NAME_TYPE_SPEC
// <name> `:` <data_type>
#[derive(Debug)]
pub struct CoreNameTypeSpecNode {
    pub colon: TokenNode,
    pub name: IdentifierNode,
    pub data_type: TypeExpressionNode,
}

// IDENTIFIER
#[derive(Debug, Node)]
pub enum CoreIdentifierNode {
    Ok(OkIdentifierNode),
    MissingTokens(MissingTokenNode),
}

// OK_IDENTIFIER
#[derive(Debug)]
pub struct CoreOkIdentifierNode {
    pub token: OkTokenNode,
}

#[derive(Debug, Node)]
pub enum CoreSelfKeywordNode {
    Ok(OkSelfKeywordNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug)]
pub struct CoreOkSelfKeywordNode {
    pub token: OkTokenNode,
}

// TOKEN
#[derive(Debug, Node)]
pub enum CoreTokenNode {
    Ok(OkTokenNode),
    MissingTokens(MissingTokenNode),
}

// OK_TOKEN
#[derive(Debug)]
pub struct CoreOkTokenNode {
    pub token: Token,
}

// MISSING_TOKEN
#[derive(Debug)]
pub struct CoreMissingTokenNode {
    pub expected_symbols: Vec<&'static str>,
    pub received_token: Token,
}

// SKIPPED_TOKEN
#[derive(Debug)]
pub struct CoreSkippedTokenNode {
    pub skipped_token: Token,
}

#[derive(Debug)]
pub struct CoreSymbolSeparatedSequenceNode<T: Clone> {
    pub entity: T,
    pub remaining_entities: Option<(TokenNode, SymbolSeparatedSequenceNode<T>)>,
}

#[derive(Debug, Node)]
pub enum CoreIdentifierInUseNode {
    Ok(OkIdentifierInUseNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Node)]
pub enum CoreIdentifierInDeclNode {
    Ok(OkIdentifierInDeclNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug)]
pub struct CoreOkIdentifierInUseNode {
    pub name: OkTokenNode,
    pub generic_type_args: Option<(
        TokenNode,
        SymbolSeparatedSequenceNode<TypeExpressionNode>,
        TokenNode,
    )>,
}

#[derive(Debug)]
pub struct CoreOkIdentifierInDeclNode {
    pub name: OkTokenNode,
    pub generic_type_decls: Option<(
        TokenNode,
        SymbolSeparatedSequenceNode<GenericTypeDeclNode>,
        TokenNode,
    )>, // (langle, ..., rangle)
}

#[derive(Debug)]
pub struct CoreGenericTypeDeclNode {
    pub generic_type_name: IdentifierInDeclNode,
    pub interface_bounds: Option<(TokenNode, SymbolSeparatedSequenceNode<IdentifierInUseNode>)>, // (colon, ...)
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
pub struct NameTypeSpecNode(pub Rc<CoreNameTypeSpecNode>);
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
#[derive(Debug, Clone)]
pub struct SymbolSeparatedSequenceNode<T: Clone>(pub Rc<CoreSymbolSeparatedSequenceNode<T>>);
#[derive(Debug, Clone)]
pub struct GenericTypeDeclNode(pub Rc<CoreGenericTypeDeclNode>);
#[derive(Debug, Clone)]
pub struct IdentifierInUseNode(pub Rc<CoreIdentifierInUseNode>);
#[derive(Debug, Clone)]
pub struct IdentifierInDeclNode(pub Rc<CoreIdentifierInDeclNode>);
#[derive(Debug, Clone)]
pub struct OkIdentifierInUseNode(pub Rc<CoreOkIdentifierInUseNode>);
#[derive(Debug, Clone)]
pub struct OkIdentifierInDeclNode(pub Rc<CoreOkIdentifierInDeclNode>);

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
