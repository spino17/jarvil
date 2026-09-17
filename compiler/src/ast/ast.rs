// This module contains green tree nodes. Green Tree is top to down immutable typed structure with no parent information.
// See the following for more information on green and red tree, immutability and cheap mutations.
// 1. `https://github.com/apple/swift/tree/5e2c815edfd758f9b1309ce07bfc01c4bc20ec23/lib/Syntax`
// 2. `https://github.com/rust-analyzer/rowan`

// NOTE: This file only contains structure of the AST nodes and not their bounded methods. The methods for respective `ASTNode::<...>`
// is declared in the file `impl_ast.rs`

use crate::lexer::token::BinaryOperatorKind;
use crate::lexer::token::UnaryOperatorKind;
use crate::parser::resolver::BlockKind;
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::{lexer::token::Token, types::core::Type};
use jarvil_macros::Node;
use jarvil_macros::Nodify;
use serde::Serialize;
use std::sync::Arc;
use text_size::TextRange;
use text_size::TextSize;

#[derive(Debug, Clone, Nodify)]
pub enum ASTNode {
    Block(BlockNode),
    StatementIndentWrapper(StatementIndentWrapperNode),
    SkippedTokens(SkippedTokensNode),
    IncorrectlyIndentedStatement(IncorrectlyIndentedStatementNode),
    Statement(StatementNode),
    Break(BreakStatementNode),
    Continue(ContinueStatementNode),
    Return(ReturnStatementNode),
    Conditional(ConditionalStatementNode),
    WhileLoop(WhileLoopStatementNode),
    ForLoop(ForLoopStatementNode),
    ConditionalBlock(ConditionalBlockNode),
    MatchCase(MatchCaseStatementNode),
    CaseBranch(CaseBranchStatementNode),
    InterfaceDeclaration(InterfaceDeclarationNode),
    DeclareFunctionPrototype(DeclareFunctionPrototypeNode),
    InterfaceMethodPrototypeWrapper(DeclareCallablePrototypeNode),
    VariableDeclaration(VariableDeclarationNode),
    Assignment(AssignmentNode),
    OkAssignment(OkAssignmentNode),
    InvalidLValue(InvalidLValueNode),
    RAssignment(RAssignmentNode),
    RVariableDeclaration(RVariableDeclarationNode),
    TypeDeclaration(TypeDeclarationNode),
    StructDeclaration(StructDeclarationNode),
    StructPropertyDeclaration(StructPropertyDeclarationNode),
    EnumDeclaration(EnumDeclarationNode),
    EnumVariantDeclaration(EnumVariantDeclarationNode),
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
    Expression(ExpressionNode),
    AtomicExpression(AtomicExpressionNode),
    ParenthesisedExpression(ParenthesisedExpressionNode),
    UnaryExpression(UnaryExpressionNode),
    OnlyUnaryExpression(OnlyUnaryExpressionNode),
    BinaryExpression(BinaryExpressionNode),
    Comparison(ComparisonNode),
    CallExpression(CallExpressionNode),
    ArrayExpression(ArrayExpressionNode),
    KeyValuePair(KeyValuePairNode),
    HashMapExpression(HashMapExpressionNode),
    TupleExpression(TupleExpressionNode),
    Atom(AtomNode),
    AtomStart(AtomStartNode),
    PropertyAccess(PropertyAccessNode),
    MethodAccess(MethodAccessNode),
    IndexAccess(IndexAccessNode),
    Call(CallNode),
    EnumVariantExprOrClassMethodCall(EnumVariantExprOrClassMethodCallNode),
    NameTypeSpec(NameTypeSpecNode),
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

#[derive(Debug, Serialize)]
pub struct CoreBlockNode {
    pub stmts: Vec<StatementIndentWrapperNode>,
    pub newline: TokenNode,
    #[serde(skip_serializing)]
    pub kind: BlockKind,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreStatementIndentWrapperNode {
    CorrectlyIndented(StatementNode),
    IncorrectlyIndented(IncorrectlyIndentedStatementNode),
    LeadingSkippedTokens(SkippedTokensNode), // skipped tokens leading to the next stmt in block
    TrailingSkippedTokens(SkippedTokensNode), // skipped tokens trailing to the previous stmt in block
    ExtraNewlines(SkippedTokensNode),
}

#[derive(Debug, Serialize)]
pub struct CoreSkippedTokensNode {
    pub skipped_tokens: Vec<SkippedTokenNode>,
}

#[derive(Debug, Serialize)]
pub struct CoreIncorrectlyIndentedStatementNode {
    pub stmt: StatementNode,
    pub expected_indent: i64,
    pub received_indent: i64,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreStatementNode {
    Expression(ExpressionStatementNode),
    Assignment(AssignmentNode),
    VariableDeclaration(VariableDeclarationNode),
    Return(ReturnStatementNode),
    Conditional(ConditionalStatementNode),
    WhileLoop(WhileLoopStatementNode),
    ForLoop(ForLoopStatementNode),
    Break(BreakStatementNode),
    Continue(ContinueStatementNode),
    FunctionWrapper(FunctionWrapperNode),
    BoundedMethodWrapper(BoundedMethodWrapperNode),
    TypeDeclaration(TypeDeclarationNode),
    StructPropertyDeclaration(StructPropertyDeclarationNode),
    MatchCase(MatchCaseStatementNode),
    CaseBranch(CaseBranchStatementNode),
    EnumVariantDeclaration(EnumVariantDeclarationNode),
    InterfaceDeclaration(InterfaceDeclarationNode),
    InterfaceMethodPrototypeWrapper(DeclareCallablePrototypeNode),
    DeclareFunctionPrototype(DeclareFunctionPrototypeNode),
}

#[derive(Debug, Serialize)]
pub struct CoreBreakStatementNode {
    pub break_keyword: TokenNode,
    pub newline: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreContinueStatementNode {
    pub continue_keyword: TokenNode,
    pub newline: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreCaseBranchStatementNode {
    pub case_keyword: TokenNode,
    pub enum_name: IdentifierInDeclNode,
    pub double_colon_node: TokenNode,
    pub variant_name: IdentifierInDeclNode,
    pub variable_name: Option<(TokenNode, IdentifierInDeclNode, TokenNode)>,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreMatchCaseStatementNode {
    pub match_keyword: TokenNode,
    pub expr: ExpressionNode,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreInterfaceDeclarationNode {
    pub interface_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreDeclareFunctionPrototypeNode {
    pub declare_keyword: TokenNode,
    pub decl: DeclareCallablePrototypeNode,
}

#[derive(Debug, Serialize)]
pub struct CoreDeclareCallablePrototypeNode {
    pub def_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub prototype: CallablePrototypeNode,
    pub newline: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreConditionalStatementNode {
    pub if_block: ConditionalBlockNode,
    pub elifs: Vec<ConditionalBlockNode>,
    pub else_block: Option<(TokenNode, TokenNode, BlockNode)>, // ('else', ':', block)
}

#[derive(Debug, Serialize)]
pub struct CoreConditionalBlockNode {
    pub condition_keyword: TokenNode, // 'if' or 'elif'
    pub condition_expr: ExpressionNode,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreWhileLoopStatementNode {
    pub while_keyword: TokenNode,
    pub condition_expr: ExpressionNode,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreForLoopStatementNode {
    pub for_keyword: TokenNode,
    pub loop_variable: IdentifierInDeclNode,
    pub in_keyword: TokenNode,
    pub iterable_expr: ExpressionNode,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreReturnStatementNode {
    pub return_keyword: TokenNode,
    pub expr: Option<ExpressionNode>,
    pub newline: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreVariableDeclarationNode {
    pub let_keyword: TokenNode,
    pub ty_annotation: Option<(TokenNode, TypeExpressionNode)>,
    pub name: IdentifierInDeclNode,
    pub equal: TokenNode,
    pub r_node: RVariableDeclarationNode,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreAssignmentNode {
    Ok(OkAssignmentNode),
    InvalidLValue(InvalidLValueNode),
}

#[derive(Debug, Serialize)]
pub struct CoreOkAssignmentNode {
    pub l_atom: AtomNode,
    pub equal: TokenNode,
    pub r_assign: RAssignmentNode,
}

#[derive(Debug, Serialize)]
pub struct CoreInvalidLValueNode {
    pub l_expr: ExpressionNode,
    pub equal: TokenNode,
    pub r_assign: RAssignmentNode,
}

#[derive(Debug, Serialize)]
pub struct CoreRAssignmentNode {
    pub expr: ExpressionStatementNode,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreRVariableDeclarationNode {
    Lambda(LambdaDeclarationNode),
    Expression(ExpressionStatementNode),
}

#[derive(Debug, Node, Serialize)]
pub enum CoreTypeDeclarationNode {
    Struct(StructDeclarationNode),
    Enum(EnumDeclarationNode),
    Lambda(LambdaTypeDeclarationNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Serialize)]
pub struct CoreStructDeclarationNode {
    pub type_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub struct_keyword: TokenNode,
    pub implementing_interfaces:
        Option<(TokenNode, SymbolSeparatedSequenceNode<IdentifierInUseNode>)>, // (`implements`, [...])
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreStructPropertyDeclarationNode {
    pub name_ty_spec: NameTypeSpecNode,
    pub newline: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreEnumDeclarationNode {
    pub type_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub enum_keyword: TokenNode,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreEnumVariantDeclarationNode {
    pub variant: IdentifierInDeclNode,
    pub ty: Option<(TokenNode, TypeExpressionNode, TokenNode)>,
    pub newline: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreLambdaTypeDeclarationNode {
    pub type_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub lambda_keyword: TokenNode,
    pub equal: TokenNode,
    pub lparen: TokenNode,
    pub ty_tuple: Option<SymbolSeparatedSequenceNode<TypeExpressionNode>>,
    pub rparen: TokenNode,
    pub return_ty: Option<(TokenNode, TypeExpressionNode)>, // (`->`, <type_expr>)
    pub newline: TokenNode,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreTypeExpressionNode {
    Atomic(AtomicTypeNode),
    UserDefined(UserDefinedTypeNode),
    Array(ArrayTypeNode),
    Tuple(TupleTypeNode),
    HashMap(HashMapTypeNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Serialize)]
pub struct CoreAtomicTypeNode {
    pub kind: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreArrayTypeNode {
    pub lsquare: TokenNode,
    pub sub_ty: TypeExpressionNode,
    pub rsquare: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreTupleTypeNode {
    pub lparen: TokenNode,
    pub types: SymbolSeparatedSequenceNode<TypeExpressionNode>,
    pub rparen: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreHashMapTypeNode {
    pub lcurly: TokenNode,
    pub key_ty: TypeExpressionNode,
    pub colon: TokenNode,
    pub value_ty: TypeExpressionNode,
    pub rcurly: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreUserDefinedTypeNode {
    pub name: IdentifierInUseNode,
}

#[derive(Debug, Serialize)]
pub struct CoreCallablePrototypeNode {
    pub lparen: TokenNode,
    pub params: Option<SymbolSeparatedSequenceNode<NameTypeSpecNode>>,
    pub rparen: TokenNode,
    pub return_ty: Option<(TokenNode, TypeExpressionNode)>, // (`->`, <type_expr>)
}

#[derive(Debug, Serialize)]
pub struct CoreCallableBodyNode {
    pub prototype: CallablePrototypeNode,
    pub colon: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Serialize)]
pub struct CoreFunctionDeclarationNode {
    pub def_keyword: TokenNode,
    pub name: IdentifierInDeclNode,
    pub body: CallableBodyNode,
}

#[derive(Debug, Serialize)]
pub struct CoreFunctionWrapperNode {
    pub func_decl: FunctionDeclarationNode,
}

#[derive(Debug, Serialize)]
pub struct CoreBoundedMethodWrapperNode {
    pub func_decl: FunctionDeclarationNode,
}

#[derive(Debug, Serialize)]
pub struct CoreLambdaDeclarationNode {
    pub lambda_keyword: TokenNode,
    pub body: CallableBodyNode,
}

#[derive(Debug, Serialize)]
pub struct CoreExpressionStatementNode {
    pub expr: ExpressionNode,
    pub newline: TokenNode,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreExpressionNode {
    Unary(UnaryExpressionNode),
    Binary(BinaryExpressionNode),
    Comparison(ComparisonNode),
}

#[derive(Debug, Node, Serialize)]
pub enum CoreAtomicExpressionNode {
    Bool(TokenNode),
    Integer(TokenNode),
    FloatingPointNumber(TokenNode),
    Literal(TokenNode),
    ParenthesisedExpression(ParenthesisedExpressionNode),
    ArrayExpression(ArrayExpressionNode),
    HashMapExpression(HashMapExpressionNode),
    TupleExpression(TupleExpressionNode),
    Atom(AtomNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Serialize)]
pub struct CoreParenthesisedExpressionNode {
    pub lparen: TokenNode,
    pub expr: ExpressionNode,
    pub rparen: TokenNode,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreUnaryExpressionNode {
    Atomic(AtomicExpressionNode),
    Unary(OnlyUnaryExpressionNode),
}

#[derive(Debug, Serialize)]
pub struct CoreOnlyUnaryExpressionNode {
    pub operator: TokenNode,
    pub unary_expr: UnaryExpressionNode,
    #[serde(skip_serializing)]
    pub operator_kind: UnaryOperatorKind,
}

#[derive(Debug, Serialize)]
pub struct CoreBinaryExpressionNode {
    pub left_expr: ExpressionNode,
    pub operator: TokenNode,
    pub right_expr: ExpressionNode,
    #[serde(skip_serializing)]
    pub operator_kind: BinaryOperatorKind,
}

#[derive(Debug, Serialize)]
pub struct CoreComparisonNode {
    pub operands: Vec<ExpressionNode>,
    pub operators: Vec<TokenNode>,
}

#[derive(Debug, Serialize)]
pub struct CoreCallExpressionNode {
    pub func_name: IdentifierInUseNode,
    pub lparen: TokenNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    pub rparen: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreArrayExpressionNode {
    pub lsquare: TokenNode,
    pub initials: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    pub rsquare: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreKeyValuePairNode {
    pub key_expr: ExpressionNode,
    pub colon: TokenNode,
    pub value_expr: ExpressionNode,
}

#[derive(Debug, Serialize)]
pub struct CoreHashMapExpressionNode {
    pub lcurly: TokenNode,
    pub initials: Option<SymbolSeparatedSequenceNode<KeyValuePairNode>>,
    pub rcurly: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreTupleExpressionNode {
    pub lround: TokenNode,
    pub initials: SymbolSeparatedSequenceNode<ExpressionNode>,
    pub rround: TokenNode,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreAtomNode {
    AtomStart(AtomStartNode),           // id, id(...), id::id(...), `self`
    Call(CallNode),                     // A(...)
    PropertyAccess(PropertyAccessNode), // A.id
    MethodAccess(MethodAccessNode),     // A.id(...)
    IndexAccess(IndexAccessNode),       // A[<expr>]
}

#[derive(Debug, Node, Serialize)]
pub enum CoreAtomStartNode {
    Identifier(IdentifierInUseNode), // id
    SelfKeyword(SelfKeywordNode),    // self
    Call(CallExpressionNode),        // id(...)
    EnumVariantExprOrClassMethodCall(EnumVariantExprOrClassMethodCallNode), // id::id(...)
}

#[derive(Debug, Serialize)]
pub struct CorePropertyAccessNode {
    pub atom: AtomNode,
    pub dot: TokenNode,
    pub propertry: IdentifierInUseNode,
}

#[derive(Debug, Serialize)]
pub struct CoreMethodAccessNode {
    pub atom: AtomNode,
    pub dot: TokenNode,
    pub method_name: IdentifierInUseNode,
    pub lparen: TokenNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    pub rparen: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreIndexAccessNode {
    pub atom: AtomNode,
    pub lsquare: TokenNode,
    pub index: ExpressionNode,
    pub rsquare: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreCallNode {
    pub atom: AtomNode,
    pub lparen: TokenNode,
    pub params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    pub rparen: TokenNode,
}

#[derive(Debug, Serialize)]
pub struct CoreEnumVariantExprOrClassMethodCallNode {
    pub ty_name: IdentifierInUseNode,
    pub double_colon: TokenNode,
    pub property_name: IdentifierInUseNode,
    pub params: Option<(
        TokenNode,
        Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        TokenNode,
    )>,
}

#[derive(Debug, Serialize)]
pub struct CoreNameTypeSpecNode {
    pub name: IdentifierInDeclNode,
    pub colon: TokenNode,
    pub data_ty: TypeExpressionNode,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreSelfKeywordNode {
    Ok(OkSelfKeywordNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Serialize)]
pub struct CoreOkSelfKeywordNode {
    pub token: OkTokenNode,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreTokenNode {
    Ok(OkTokenNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Serialize)]
pub struct CoreOkTokenNode {
    pub token: Token,
}

#[derive(Debug, Serialize)]
pub struct CoreMissingTokenNode {
    pub expected_symbols: Vec<&'static str>,
    pub received_token: Token,
}

#[derive(Debug, Serialize)]
pub struct CoreSkippedTokenNode {
    pub skipped_token: Token,
}

#[derive(Debug, Serialize)]
pub struct CoreSymbolSeparatedSequenceNode<T: Node + Serialize + Clone> {
    pub entity: T,
    pub remaining_entities: Option<(TokenNode, SymbolSeparatedSequenceNode<T>)>,
}

#[derive(Debug, Node, Serialize)]
pub enum CoreIdentifierInUseNode {
    Ok(OkIdentifierInUseNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Node, Serialize)]
pub enum CoreIdentifierInDeclNode {
    Ok(OkIdentifierInDeclNode),
    MissingTokens(MissingTokenNode),
}

#[derive(Debug, Serialize)]
pub struct CoreOkIdentifierInUseNode {
    pub name: OkTokenNode,
    pub generic_ty_args: Option<(
        TokenNode,
        SymbolSeparatedSequenceNode<TypeExpressionNode>,
        TokenNode,
    )>,
}

#[derive(Debug, Serialize)]
pub struct CoreOkIdentifierInDeclNode {
    pub name: OkTokenNode,
    pub generic_ty_decls: Option<(
        TokenNode,
        SymbolSeparatedSequenceNode<GenericTypeDeclNode>,
        TokenNode,
    )>, // (langle, ..., rangle)
}

#[derive(Debug, Serialize)]
pub struct CoreGenericTypeDeclNode {
    pub generic_ty_name: IdentifierInDeclNode,
    pub interface_bounds: Option<(TokenNode, SymbolSeparatedSequenceNode<IdentifierInUseNode>)>, // (colon, ...)
}

#[derive(Debug, Clone)]
pub struct BlockNode(pub Arc<CoreBlockNode>);
#[derive(Debug, Clone)]
pub struct StatementIndentWrapperNode(pub Arc<CoreStatementIndentWrapperNode>);
#[derive(Debug, Clone)]
pub struct SkippedTokensNode(pub Arc<CoreSkippedTokensNode>);
#[derive(Debug, Clone)]
pub struct IncorrectlyIndentedStatementNode(pub Arc<CoreIncorrectlyIndentedStatementNode>);
#[derive(Debug, Clone)]
pub struct StatementNode(pub Arc<CoreStatementNode>);
#[derive(Debug, Clone)]
pub struct InterfaceDeclarationNode(pub Arc<CoreInterfaceDeclarationNode>);
#[derive(Debug, Clone)]
pub struct DeclareFunctionPrototypeNode(pub Arc<CoreDeclareFunctionPrototypeNode>);
#[derive(Debug, Clone)]
pub struct DeclareCallablePrototypeNode(pub Arc<CoreDeclareCallablePrototypeNode>);
#[derive(Debug, Clone)]
pub struct ReturnStatementNode(pub Arc<CoreReturnStatementNode>);
#[derive(Debug, Clone)]
pub struct VariableDeclarationNode(pub Arc<CoreVariableDeclarationNode>);
#[derive(Debug, Clone)]
pub struct AssignmentNode(pub Arc<CoreAssignmentNode>);
#[derive(Debug, Clone)]
pub struct OkAssignmentNode(pub Arc<CoreOkAssignmentNode>);
#[derive(Debug, Clone)]
pub struct InvalidLValueNode(pub Arc<CoreInvalidLValueNode>);
#[derive(Debug, Clone)]
pub struct RAssignmentNode(pub Arc<CoreRAssignmentNode>);
#[derive(Debug, Clone)]
pub struct RVariableDeclarationNode(pub Arc<CoreRVariableDeclarationNode>);
#[derive(Debug, Clone)]
pub struct TypeDeclarationNode(pub Arc<CoreTypeDeclarationNode>);
#[derive(Debug, Clone)]
pub struct StructDeclarationNode(pub Arc<CoreStructDeclarationNode>);
#[derive(Debug, Clone)]
pub struct StructPropertyDeclarationNode(pub Arc<CoreStructPropertyDeclarationNode>);
#[derive(Debug, Clone)]
pub struct LambdaTypeDeclarationNode(pub Arc<CoreLambdaTypeDeclarationNode>);
#[derive(Debug, Clone)]
pub struct TypeExpressionNode(pub Arc<CoreTypeExpressionNode>);
#[derive(Debug, Clone)]
pub struct AtomicTypeNode(pub Arc<CoreAtomicTypeNode>);
#[derive(Debug, Clone)]
pub struct ArrayTypeNode(pub Arc<CoreArrayTypeNode>);
#[derive(Debug, Clone)]
pub struct TupleTypeNode(pub Arc<CoreTupleTypeNode>);
#[derive(Debug, Clone)]
pub struct HashMapTypeNode(pub Arc<CoreHashMapTypeNode>);
#[derive(Debug, Clone)]
pub struct UserDefinedTypeNode(pub Arc<CoreUserDefinedTypeNode>);
#[derive(Debug, Clone)]
pub struct CallablePrototypeNode(pub Arc<CoreCallablePrototypeNode>);
#[derive(Debug, Clone)]
pub struct CallableBodyNode(pub Arc<CoreCallableBodyNode>);
#[derive(Debug, Clone)]
pub struct FunctionDeclarationNode(pub Arc<CoreFunctionDeclarationNode>);
#[derive(Debug, Clone)]
pub struct FunctionWrapperNode(pub Arc<CoreFunctionWrapperNode>);
#[derive(Debug, Clone)]
pub struct BoundedMethodWrapperNode(pub Arc<CoreBoundedMethodWrapperNode>);
#[derive(Debug, Clone)]
pub struct LambdaDeclarationNode(pub Arc<CoreLambdaDeclarationNode>);
#[derive(Debug, Clone)]
pub struct ExpressionStatementNode(pub Arc<CoreExpressionStatementNode>);
#[derive(Debug, Clone)]
pub struct ExpressionNode(pub Arc<CoreExpressionNode>);
#[derive(Debug, Clone)]
pub struct AtomicExpressionNode(pub Arc<CoreAtomicExpressionNode>);
#[derive(Debug, Clone)]
pub struct ParenthesisedExpressionNode(pub Arc<CoreParenthesisedExpressionNode>);
#[derive(Debug, Clone)]
pub struct UnaryExpressionNode(pub Arc<CoreUnaryExpressionNode>);
#[derive(Debug, Clone)]
pub struct OnlyUnaryExpressionNode(pub Arc<CoreOnlyUnaryExpressionNode>);
#[derive(Debug, Clone)]
pub struct BinaryExpressionNode(pub Arc<CoreBinaryExpressionNode>);
#[derive(Debug, Clone)]
pub struct ComparisonNode(pub Arc<CoreComparisonNode>);
#[derive(Debug, Clone)]
pub struct CallExpressionNode(pub Arc<CoreCallExpressionNode>);
#[derive(Debug, Clone)]
pub struct AtomNode(pub Arc<CoreAtomNode>);
#[derive(Debug, Clone)]
pub struct AtomStartNode(pub Arc<CoreAtomStartNode>);
#[derive(Debug, Clone)]
pub struct PropertyAccessNode(pub Arc<CorePropertyAccessNode>);
#[derive(Debug, Clone)]
pub struct MethodAccessNode(pub Arc<CoreMethodAccessNode>);
#[derive(Debug, Clone)]
pub struct IndexAccessNode(pub Arc<CoreIndexAccessNode>);
#[derive(Debug, Clone)]
pub struct CallNode(pub Arc<CoreCallNode>);
#[derive(Debug, Clone)]
pub struct EnumVariantExprOrClassMethodCallNode(pub Arc<CoreEnumVariantExprOrClassMethodCallNode>);
#[derive(Debug, Clone)]
pub struct NameTypeSpecNode(pub Arc<CoreNameTypeSpecNode>);
#[derive(Debug, Clone)]
pub struct SelfKeywordNode(pub Arc<CoreSelfKeywordNode>);
#[derive(Debug, Clone)]
pub struct OkSelfKeywordNode(pub Arc<CoreOkSelfKeywordNode>);
#[derive(Debug, Clone)]
pub struct TokenNode(pub Arc<CoreTokenNode>);
#[derive(Debug, Clone)]
pub struct OkTokenNode(pub Arc<CoreOkTokenNode>);
#[derive(Debug, Clone)]
pub struct MissingTokenNode(pub Arc<CoreMissingTokenNode>);
#[derive(Debug, Clone)]
pub struct SkippedTokenNode(pub Arc<CoreSkippedTokenNode>);
#[derive(Debug, Clone)]
pub struct GenericTypeDeclNode(pub Arc<CoreGenericTypeDeclNode>);
#[derive(Debug, Clone)]
pub struct IdentifierInUseNode(pub Arc<CoreIdentifierInUseNode>);
#[derive(Debug, Clone)]
pub struct IdentifierInDeclNode(pub Arc<CoreIdentifierInDeclNode>);
#[derive(Debug, Clone)]
pub struct OkIdentifierInUseNode(pub Arc<CoreOkIdentifierInUseNode>);
#[derive(Debug, Clone)]
pub struct OkIdentifierInDeclNode(pub Arc<CoreOkIdentifierInDeclNode>);
#[derive(Debug, Clone)]
pub struct ArrayExpressionNode(pub Arc<CoreArrayExpressionNode>);
#[derive(Debug, Clone)]
pub struct KeyValuePairNode(pub Arc<CoreKeyValuePairNode>);
#[derive(Debug, Clone)]
pub struct HashMapExpressionNode(pub Arc<CoreHashMapExpressionNode>);
#[derive(Debug, Clone)]
pub struct TupleExpressionNode(pub Arc<CoreTupleExpressionNode>);
#[derive(Debug, Clone)]
pub struct ConditionalStatementNode(pub Arc<CoreConditionalStatementNode>);
#[derive(Debug, Clone)]
pub struct ConditionalBlockNode(pub Arc<CoreConditionalBlockNode>);
#[derive(Debug, Clone)]
pub struct BreakStatementNode(pub Arc<CoreBreakStatementNode>);
#[derive(Debug, Clone)]
pub struct ContinueStatementNode(pub Arc<CoreContinueStatementNode>);
#[derive(Debug, Clone)]
pub struct EnumDeclarationNode(pub Arc<CoreEnumDeclarationNode>);
#[derive(Debug, Clone)]
pub struct EnumVariantDeclarationNode(pub Arc<CoreEnumVariantDeclarationNode>);
#[derive(Debug, Clone)]
pub struct MatchCaseStatementNode(pub Arc<CoreMatchCaseStatementNode>);
#[derive(Debug, Clone)]
pub struct CaseBranchStatementNode(pub Arc<CoreCaseBranchStatementNode>);
#[derive(Debug, Clone)]
pub struct WhileLoopStatementNode(pub Arc<CoreWhileLoopStatementNode>);
#[derive(Debug, Clone)]
pub struct ForLoopStatementNode(pub Arc<CoreForLoopStatementNode>);
#[derive(Debug, Clone)]
pub struct SymbolSeparatedSequenceNode<T: Node + Serialize + Clone>(
    pub Arc<CoreSymbolSeparatedSequenceNode<T>>,
);

pub enum UnresolvedIdentifier<'a> {
    Unresolved(&'a OkIdentifierInUseNode),
    GenericResolvedToOutsideScope(&'a OkIdentifierInUseNode, TextRange),
    NotInitialized(&'a OkIdentifierInUseNode, TextRange),
    InvalidGenericTypeArgsProvided(&'a OkIdentifierInUseNode, GenericTypeArgsCheckError),
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
