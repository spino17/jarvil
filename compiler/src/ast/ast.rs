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
use crate::parser::resolver::BlockKind;
use crate::{lexer::token::Token, types::core::Type};
use std::rc::Rc;
use text_size::{TextRange, TextSize};

pub trait Node {
    fn range(&self) -> TextRange;
    fn start_line_number(&self) -> usize;
}

pub trait ErrornousNode {
    fn new_with_missing_tokens(expected_symbols: Vec<&'static str>, received_token: &Token)
        -> Self;
}

#[derive(Debug, Clone, Nodify)]
pub enum ASTNode {
    Block(BlockNode),
    StatementIndentWrapper(StatemenIndentWrapperNode),
    SkippedTokens(SkippedTokensNode),
    IncorrectlyIndentedStatement(IncorrectlyIndentedStatementNode),
    Statement(StatementNode),
    Return(ReturnStatementNode),
    InterfaceDeclaration(InterfaceDeclarationNode),
    InterfaceMethodPrototypeWrapper(InterfaceMethodPrototypeWrapperNode),
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
    IdentifierInUse(IdentifierInUseNode),
    IdentifierInDecl(IdentifierInDeclNode),
    OkIdentifierInUse(OkIdentifierInUseNode),
    OkIdentifierInDecl(OkIdentifierInDeclNode),
    GenericTypeDecl(GenericTypeDeclNode),
    SelfKeyword(SelfKeywordNode),
    OkSelfKeyword(OkSelfKeywordNode),
    Token(TokenNode),
    OkToken(OkTokenNode),
    MissingToken(MissingTokenNode),
    SkippedToken(SkippedTokenNode),
}

#[derive(Debug)]
pub struct CoreBlockNode {
    pub newline: TokenNode,
    pub stmts: Rc<Vec<StatemenIndentWrapperNode>>,
    pub kind: BlockKind,
}

#[derive(Debug, Node)]
pub enum CoreStatemenIndentWrapperNode {
    CorrectlyIndented(StatementNode),
    IncorrectlyIndented(IncorrectlyIndentedStatementNode),
    LeadingSkippedTokens(SkippedTokensNode), // skipped tokens leading to the next stmt in block
    TrailingSkippedTokens(SkippedTokensNode), // skipped tokens trailing to the previous stmt in block
    ExtraNewlines(SkippedTokensNode),
}

#[derive(Debug)]
pub struct CoreSkippedTokensNode {
    pub skipped_tokens: Vec<SkippedTokenNode>,
}

#[derive(Debug)]
pub struct CoreIncorrectlyIndentedStatementNode {
    pub stmt: StatementNode,
    pub expected_indent: i64,
    pub received_indent: i64,
}

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
    InterfaceDeclaration(InterfaceDeclarationNode),
    InterfaceMethodPrototypeWrapper(InterfaceMethodPrototypeWrapperNode),
}

#[derive(Debug)]
pub struct CoreInterfaceDeclarationNode {
    pub interface_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug)]
pub enum InterfaceMethodTerminalNode {
    NoDefaultBody(TokenNode), // newline
    HasDefaultBody(TokenNode, BlockNode),
}

#[derive(Debug)]
pub struct CoreInterfaceMethodPrototypeWrapperNode {
    pub def_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub prototype: CallablePrototypeNode,
    pub terminal: InterfaceMethodTerminalNode,
}

#[derive(Debug)]
pub struct CoreReturnStatementNode {
    pub return_keyword: TokenNode,
    pub expr: Option<ExpressionNode>,
    pub newline: TokenNode,
}

#[derive(Debug)]
pub struct CoreVariableDeclarationNode {
    pub let_keyword: TokenNode,
    pub equal: TokenNode,
    pub name: IdentifierInDeclNode,
    pub r_node: RVariableDeclarationNode,
}

#[derive(Debug, Node)]
pub enum CoreAssignmentNode {
    Ok(OkAssignmentNode),
    InvalidLValue(InvalidLValueNode),
}

#[derive(Debug)]
pub struct CoreOkAssignmentNode {
    pub equal: TokenNode,
    pub l_atom: AtomNode,
    pub r_assign: RAssignmentNode,
}

#[derive(Debug)]
pub struct CoreInvalidLValueNode {
    pub l_expr: ExpressionNode,
    pub equal: TokenNode,
    pub r_assign: RAssignmentNode,
}

#[derive(Debug)]
pub struct CoreRAssignmentNode {
    pub expr: ExpressionStatementNode,
}

#[derive(Debug, Node)]
pub enum CoreRVariableDeclarationNode {
    Lambda(LambdaDeclarationNode),
    Expression(ExpressionStatementNode),
}

#[derive(Debug, Node)]
pub enum CoreTypeDeclarationNode {
    Struct(StructDeclarationNode),
    Lambda(LambdaTypeDeclarationNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug)]
pub struct CoreStructDeclarationNode {
    pub type_keyword: TokenNode,
    pub colon: TokenNode,
    pub struct_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub block: BlockNode,
}

#[derive(Debug)]
pub struct CoreStructPropertyDeclarationNode {
    pub newline: TokenNode,
    pub name_type_spec: NameTypeSpecNode,
}

#[derive(Debug)]
pub struct CoreLambdaTypeDeclarationNode {
    pub type_keyword: TokenNode,
    pub lambda_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub equal: TokenNode,
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub type_tuple: Option<SymbolSeparatedSequenceNode<TypeExpressionNode>>,
    pub right_arrow: Option<TokenNode>,
    pub return_type: Option<TypeExpressionNode>,
    pub newline: TokenNode,
}

#[derive(Debug, Node)]
pub enum CoreTypeExpressionNode {
    Atomic(AtomicTypeNode),
    UserDefined(UserDefinedTypeNode),
    Array(ArrayTypeNode),
    Tuple(TupleTypeNode),
    HashMap(HashMapTypeNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug)]
pub struct CoreAtomicTypeNode {
    pub kind: TokenNode,
}

#[derive(Debug)]
pub struct CoreArrayTypeNode {
    pub lsquare: TokenNode,
    pub rsquare: TokenNode,
    pub sub_type: TypeExpressionNode,
}

#[derive(Debug)]
pub struct CoreTupleTypeNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub types: SymbolSeparatedSequenceNode<TypeExpressionNode>,
}

#[derive(Debug)]
pub struct CoreHashMapTypeNode {
    pub lcurly: TokenNode,
    pub rcurly: TokenNode,
    pub colon: TokenNode,
    pub key_type: TypeExpressionNode,
    pub value_type: TypeExpressionNode,
}

#[derive(Debug)]
pub struct CoreUserDefinedTypeNode {
    pub name: IdentifierInUseNode,
}

#[derive(Debug)]
pub struct CoreCallablePrototypeNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub params: Option<SymbolSeparatedSequenceNode<NameTypeSpecNode>>,
    pub return_type: Option<(TokenNode, TypeExpressionNode)>, // (`->`, <type_expr>)
}

#[derive(Debug)]
pub struct CoreCallableBodyNode {
    pub colon: TokenNode,
    pub block: BlockNode,
    pub prototype: CallablePrototypeNode,
}

#[derive(Debug)]
pub struct CoreFunctionDeclarationNode {
    pub def_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
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

#[derive(Debug)]
pub struct CoreLambdaDeclarationNode {
    pub lambda_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub body: CallableBodyNode,
}

#[derive(Debug)]
pub struct CoreExpressionStatementNode {
    pub expr: ExpressionNode,
    pub newline: TokenNode,
}

#[derive(Debug, Node)]
pub enum CoreExpressionNode {
    Unary(UnaryExpressionNode),
    Binary(BinaryExpressionNode),
    Comparison(ComparisonNode),
}

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

#[derive(Debug)]
pub struct CoreParenthesisedExpressionNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub expr: ExpressionNode,
}

#[derive(Debug, Node)]
pub enum CoreUnaryExpressionNode {
    Atomic(AtomicExpressionNode),
    Unary(OnlyUnaryExpressionNode),
}

#[derive(Debug)]
pub struct CoreOnlyUnaryExpressionNode {
    pub operator: TokenNode,
    pub unary_expr: UnaryExpressionNode,
    pub operator_kind: UnaryOperatorKind,
}

#[derive(Debug)]
pub struct CoreBinaryExpressionNode {
    pub operator_kind: BinaryOperatorKind,
    pub operator: TokenNode,
    pub left_expr: ExpressionNode,
    pub right_expr: ExpressionNode,
}

#[derive(Debug)]
pub struct CoreComparisonNode {
    pub operands: Vec<ExpressionNode>,
    pub operators: Vec<TokenNode>,
}

#[derive(Debug)]
pub struct CoreCallExpressionNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub function_name: IdentifierInUseNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
}

#[derive(Debug, Node)]
pub enum CoreAtomNode {
    AtomStart(AtomStartNode),           // id, id(...), id::id(...), `self`
    Call(CallNode),                     // A(...)
    PropertyAccess(PropertyAccessNode), // A.id
    MethodAccess(MethodAccessNode),     // A.id(...)
    IndexAccess(IndexAccessNode),       // A[<expr>]
}

#[derive(Debug, Node)]
pub enum CoreAtomStartNode {
    Identifier(IdentifierInUseNode),      // id
    SelfKeyword(SelfKeywordNode),         // self
    Call(CallExpressionNode),             // id(...)
    ClassMethodCall(ClassMethodCallNode), // id::id(...)
}

#[derive(Debug)]
pub struct CorePropertyAccessNode {
    pub dot: TokenNode,
    pub atom: AtomNode,
    pub propertry: IdentifierInUseNode,
}

#[derive(Debug)]
pub struct CoreMethodAccessNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub dot: TokenNode,
    pub atom: AtomNode,
    pub method_name: IdentifierInUseNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
}

#[derive(Debug)]
pub struct CoreIndexAccessNode {
    pub lsquare: TokenNode,
    pub rsquare: TokenNode,
    pub atom: AtomNode,
    pub index: ExpressionNode,
}

#[derive(Debug)]
pub struct CoreCallNode {
    pub atom: AtomNode,
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
}

#[derive(Debug)]
pub struct CoreClassMethodCallNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub double_colon: TokenNode,
    pub class_name: IdentifierInUseNode,
    pub class_method_name: IdentifierInUseNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
}

#[derive(Debug)]
pub struct CoreNameTypeSpecNode {
    pub colon: TokenNode,
    pub name: IdentifierInDeclNode,
    pub data_type: TypeExpressionNode,
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

#[derive(Debug, Node)]
pub enum CoreTokenNode {
    Ok(OkTokenNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug)]
pub struct CoreOkTokenNode {
    pub token: Token,
}

#[derive(Debug)]
pub struct CoreMissingTokenNode {
    pub expected_symbols: Vec<&'static str>,
    pub received_token: Token,
}

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
pub struct InterfaceDeclarationNode(pub Rc<CoreInterfaceDeclarationNode>);
#[derive(Debug, Clone)]
pub struct InterfaceMethodPrototypeWrapperNode(pub Rc<CoreInterfaceMethodPrototypeWrapperNode>);
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

pub enum UnresolvedIdentifier<'a> {
    Unresolved(&'a OkIdentifierInUseNode),
    GenericResolvedToOutsideScope(&'a OkIdentifierInUseNode),
}

// misc "kind" enums
pub enum TypeResolveKind<'a> {
    Resolved(Type),
    Unresolved(Vec<UnresolvedIdentifier<'a>>),
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
