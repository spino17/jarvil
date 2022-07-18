// AST Nodes have inner mutability to enable dynamic changes to AST like monomorphism of generics or macro expansion.
// ASTNode has weak reference to core nodes to avoid memory leaks. 
// See `https://doc.rust-lang.org/book/ch15-06-reference-cycles.html` for more information

use std::{rc::{Rc, Weak}, cell::RefCell};
use crate::{scope::{core::Scope}, lexer::token::{Token, CoreToken}};

pub trait Node {
    fn set_parent(&self, parent_node: ASTNode);
}

macro_rules! default_node_impl {
    ($t: ident) => {
        impl Node for $t {
            fn set_parent(&self, parent_node: ASTNode) {
                self.0.as_ref().borrow_mut().parent = Some(parent_node);
            }
        }
    };
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    BLOCK(Weak<RefCell<CoreBlockNode>>),
    STATEMENT(Weak<RefCell<CoreStatementNode>>),
    NAME_TYPE_SPECS(Weak<RefCell<CoreNameTypeSpecsNode>>),
    NAME_TYPE_SPEC(Weak<RefCell<CoreNameTypeSpecNode>>),
    TYPE_EXPRESSION(Weak<RefCell<CoreTypeExpressionNode>>),
    ATOMIC_TYPE(Weak<RefCell<CoreAtomicTypeNode>>),
    ARRAY_TYPE(Weak<RefCell<CoreArrayTypeNode>>),
    USER_DEFINED_TYPE(Weak<RefCell<CoreUserDefinedTypeNode>>),
    SKIPPED_TOKENS(Weak<RefCell<CoreSkippedTokens>>),
    EXPRESSION(Weak<RefCell<CoreExpressionNode>>),
    ATOMIC_EXPRESSION(Weak<RefCell<CoreAtomicExpressionNode>>),
    UNARY_EXPRESSION(Weak<RefCell<CoreUnaryExpressionNode>>),
    BINARY_EXPRESSION(Weak<RefCell<CoreBinaryExpressionNode>>),
    PARAMS(Weak<RefCell<CoreParamsNode>>),
    CLASS_METHOD_CALL(Weak<RefCell<CoreClassMethodCallNode>>),
    CALL_EXPRESSION(Weak<RefCell<CoreCallExpressionNode>>),
    ATOM(Weak<RefCell<CoreAtomNode>>),
    PROPERTY_ACCESS(Weak<RefCell<CorePropertyAccessNode>>),
    METHOD_ACCESS(Weak<RefCell<CoreMethodAccessNode>>),
    INDEX_ACCESS(Weak<RefCell<CoreIndexAccessNode>>),
    ATOM_START(Weak<RefCell<CoreAtomStartNode>>),
    VARIABLE_DECLARATION(Weak<RefCell<CoreVariableDeclarationNode>>),
    FUNCTION_DECLARATION(Weak<RefCell<CoreFunctionDeclarationNode>>),
}

#[derive(Debug, Clone)]
pub enum StatemenIndentWrapper {
    CORRECTLY_INDENTED(StatementNode),
    INCORRECTLY_INDENTED((StatementNode, (i64, i64))),
    LEADING_SKIPPED_TOKENS(SkippedTokens),      // skipped tokens leading to the next stmt in block
    TRAILING_SKIPPED_TOKENS(SkippedTokens),     // skipped tokens trailing to the previous stmt in block
    EXTRA_NEWLINES(SkippedTokens),
}

#[derive(Debug, Clone)]
pub struct CoreBlockNode {
    stmts: Rc<RefCell<Vec<StatemenIndentWrapper>>>,
    scope: Option<Scope>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct BlockNode(Rc<RefCell<CoreBlockNode>>);
impl BlockNode {
    pub fn new(stmts: &Rc<RefCell<Vec<StatemenIndentWrapper>>>) -> Self {
        let node = Rc::new(RefCell::new(CoreBlockNode{
            stmts: stmts.clone(),
            scope: None,
            parent: None,
        }));
        for stmt in &*stmts.as_ref().borrow() {
            match stmt {
                StatemenIndentWrapper::CORRECTLY_INDENTED(correct_indented_stmt) => {
                    correct_indented_stmt.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
                StatemenIndentWrapper::INCORRECTLY_INDENTED((incorrect_indented_stmt, _)) => {
                    incorrect_indented_stmt.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
                StatemenIndentWrapper::LEADING_SKIPPED_TOKENS(leading_skipped_tokens) => {
                    leading_skipped_tokens.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
                StatemenIndentWrapper::TRAILING_SKIPPED_TOKENS(trailing_skipped_tokens) => {
                    trailing_skipped_tokens.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
                StatemenIndentWrapper::EXTRA_NEWLINES(extra_newlines) => {
                    extra_newlines.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                },
            }
        }
        BlockNode(node)
    }
}
default_node_impl!(BlockNode);
/*
impl Node for BlockNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}
 */

#[derive(Debug, Clone)]
pub struct CoreSkippedTokens {
    skipped_tokens: Rc<Vec<SkippedTokenNode>>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct SkippedTokens(Rc<RefCell<CoreSkippedTokens>>);
impl SkippedTokens {
    pub fn new_with_leading_skipped_tokens(skipped_tokens: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens{
            skipped_tokens: skipped_tokens.clone(),
            parent: None,
        }));
        for skipped_token in skipped_tokens.as_ref() {
            skipped_token.set_parent(ASTNode::SKIPPED_TOKENS(Rc::downgrade(&node)));
        }
        SkippedTokens(node)
    }

    pub fn new_with_trailing_skipped_tokens(skipped_tokens: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens{
            skipped_tokens: skipped_tokens.clone(),
            parent: None,
        }));
        for skipped_token in skipped_tokens.as_ref() {
            skipped_token.set_parent(ASTNode::SKIPPED_TOKENS(Rc::downgrade(&node)));
        }
        SkippedTokens(node)
    }

    pub fn new_with_extra_newlines(extra_newlines: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens{
            skipped_tokens: extra_newlines.clone(),
            parent: None,
        }));
        for skipped_token in extra_newlines.as_ref() {
            skipped_token.set_parent(ASTNode::SKIPPED_TOKENS(Rc::downgrade(&node)));
        }
        SkippedTokens(node)
    }
}
impl Node for SkippedTokens {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreStatementNode {
    kind: StatementNodeKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum StatementNodeKind {
    // expr, variable declaration, type struct declaration, type lambda declaration, interface declaration, 
    // assignment, if, for, while, return, continue, break, implementation of interfaces, implementation of structs
    EXPRESSION(ExpressionNode),
    VARIABLE_DECLARATION(VariableDeclarationNode),
    FUNCTION_DECLARATION(FunctionDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct StatementNode(Rc<RefCell<CoreStatementNode>>);
impl StatementNode {
    pub fn new_with_expression(expr_node: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::EXPRESSION(expr_node.clone()),
            parent: None,
        }));
        expr_node.set_parent(ASTNode::STATEMENT(Rc::downgrade(&node)));
        StatementNode(node)
    }

    pub fn new_with_variable_declaration(variable_decl_node: &VariableDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::VARIABLE_DECLARATION(variable_decl_node.clone()),
            parent: None,
        }));
        variable_decl_node.set_parent(ASTNode::STATEMENT(Rc::downgrade(&node)));
        StatementNode(node)
    }

    pub fn new_with_function_declaration(function_decl_node: &FunctionDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::FUNCTION_DECLARATION(function_decl_node.clone()),
            parent: None,
        }));
        function_decl_node.set_parent(ASTNode::STATEMENT(Rc::downgrade(&node)));
        StatementNode(node)
    }

    pub fn new_with_missing_tokens(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self {
        StatementNode(Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::MISSING_TOKENS(MissingTokenNode::new(expected_symbols, received_token, lookahead)),
            parent: None,
        })))
    }
}
impl Node for StatementNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreFunctionDeclarationNode {
    name: TokenNode,
    args: Option<NameTypeSpecsNode>,
    return_type: Option<TypeExpressionNode>,
    block: BlockNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclarationNode(Rc<RefCell<CoreFunctionDeclarationNode>>);
impl FunctionDeclarationNode {
    pub fn new(name: &TokenNode, args: &Option<NameTypeSpecsNode>, return_type: &Option<TypeExpressionNode>, block: &BlockNode) -> Self {
        let node = Rc::new(RefCell::new(CoreFunctionDeclarationNode{
            name: name.clone(),
            args: args.clone(),
            return_type: return_type.clone(),
            block: block.clone(),
            parent: None,
        }));
        name.set_parent(ASTNode::FUNCTION_DECLARATION(Rc::downgrade(&node)));
        match args {
            Some(args) => args.set_parent(ASTNode::FUNCTION_DECLARATION(Rc::downgrade(&node))),
            None => {},
        }
        match return_type {
            Some(return_type) => return_type.set_parent(ASTNode::FUNCTION_DECLARATION(Rc::downgrade(&node))),
            None => {},
        }
        block.set_parent(ASTNode::FUNCTION_DECLARATION(Rc::downgrade(&node)));
        FunctionDeclarationNode(node)
    }
}
impl Node for FunctionDeclarationNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreVariableDeclarationNode {
    name: TokenNode,
    r_expr: ExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationNode(Rc<RefCell<CoreVariableDeclarationNode>>);
impl VariableDeclarationNode {
    pub fn new(name: &TokenNode, r_expr: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreVariableDeclarationNode{
            name: name.clone(),
            r_expr: r_expr.clone(),
            parent: None,
        }));
        name.set_parent(ASTNode::VARIABLE_DECLARATION(Rc::downgrade(&node)));
        r_expr.set_parent(ASTNode::VARIABLE_DECLARATION(Rc::downgrade(&node)));
        VariableDeclarationNode(node)
    }
}
impl Node for VariableDeclarationNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreNameTypeSpecsNode {
    arg: NameTypeSpecNode,
    remaining_args: Option<NameTypeSpecsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct NameTypeSpecsNode(Rc<RefCell<CoreNameTypeSpecsNode>>);
impl NameTypeSpecsNode {
    pub fn new_with_args(arg: &NameTypeSpecNode, remaining_args: &NameTypeSpecsNode) -> Self {
        let node = Rc::new(RefCell::new(CoreNameTypeSpecsNode{
            arg: arg.clone(),
            remaining_args: Some(remaining_args.clone()),
            parent: None,
        }));
        arg.set_parent(ASTNode::NAME_TYPE_SPECS(Rc::downgrade(&node)));
        remaining_args.set_parent(ASTNode::NAME_TYPE_SPECS(Rc::downgrade(&node)));
        NameTypeSpecsNode(node)
    }

    pub fn new_with_single_arg(arg: &NameTypeSpecNode) -> Self {
        let node = Rc::new(RefCell::new(CoreNameTypeSpecsNode{
            arg: arg.clone(),
            remaining_args: None,
            parent: None,
        }));
        arg.set_parent(ASTNode::NAME_TYPE_SPECS(Rc::downgrade(&node)));
        NameTypeSpecsNode(node)
    }
}
impl Node for NameTypeSpecsNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreNameTypeSpecNode {
    param_name: TokenNode,
    param_type: TypeExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct NameTypeSpecNode(Rc<RefCell<CoreNameTypeSpecNode>>);
impl NameTypeSpecNode {
    pub fn new(param_name: &TokenNode, param_type: &TypeExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreNameTypeSpecNode{
            param_name: param_name.clone(),
            param_type: param_type.clone(),
            parent: None,
        }));
        param_name.set_parent(ASTNode::NAME_TYPE_SPEC(Rc::downgrade(&node)));
        param_type.set_parent(ASTNode::NAME_TYPE_SPEC(Rc::downgrade(&node)));
        NameTypeSpecNode(node)
    }
}
impl Node for NameTypeSpecNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreTypeExpressionNode {
    kind: TypeExpressionKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum TypeExpressionKind {
    ATOMIC(AtomicTypeNode),
    USER_DEFINED(UserDefinedTypeNode),
    ARRAY(ArrayTypeNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct TypeExpressionNode(Rc<RefCell<CoreTypeExpressionNode>>);
impl TypeExpressionNode {
    pub fn new_with_atomic_type(atomic_type: &TokenNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode{
            kind: TypeExpressionKind::ATOMIC(AtomicTypeNode::new(atomic_type)),
            parent: None,
        })))
    }

    pub fn new_with_user_defined_type(identifier: &TokenNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode{
            kind: TypeExpressionKind::USER_DEFINED(UserDefinedTypeNode::new(identifier)),
            parent: None,
        })))
    }

    pub fn new_with_array_type(array_size: &TokenNode, sub_type: &TypeExpressionNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode{
            kind: TypeExpressionKind::ARRAY(ArrayTypeNode::new(array_size, sub_type)),
            parent: None,
        })))
    }

    pub fn new_with_missing_tokens(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode{
            kind: TypeExpressionKind::MISSING_TOKENS(MissingTokenNode::new(expected_symbols, received_token, lookahead)),
            parent: None,
        })))
    }
}
impl Node for TypeExpressionNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreAtomicTypeNode {
    kind: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct AtomicTypeNode(Rc<RefCell<CoreAtomicTypeNode>>);
impl AtomicTypeNode {
    pub fn new(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicTypeNode{
            kind: token.clone(),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOMIC_TYPE(Rc::downgrade(&node)));
        AtomicTypeNode(node)
    }
}
impl Node for AtomicTypeNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreArrayTypeNode {
    sub_type: TypeExpressionNode,
    size: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct ArrayTypeNode(Rc<RefCell<CoreArrayTypeNode>>);
impl ArrayTypeNode {
    pub fn new(size: &TokenNode, sub_type: &TypeExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreArrayTypeNode{
            sub_type: sub_type.clone(),
            size: size.clone(),
            parent: None,
        }));
        size.set_parent(ASTNode::ARRAY_TYPE(Rc::downgrade(&node)));
        sub_type.set_parent(ASTNode::ARRAY_TYPE(Rc::downgrade(&node)));
        ArrayTypeNode(node)
    }
}
impl Node for ArrayTypeNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreUserDefinedTypeNode {
    token: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct UserDefinedTypeNode(Rc<RefCell<CoreUserDefinedTypeNode>>);
impl UserDefinedTypeNode {
    pub fn new(identifier: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreUserDefinedTypeNode{
            token: identifier.clone(),
            parent: None,
        }));
        identifier.set_parent(ASTNode::USER_DEFINED_TYPE(Rc::downgrade(&node)));
        UserDefinedTypeNode(node)
    }
}
impl Node for UserDefinedTypeNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreTokenNode {
    kind: TokenNodeKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum TokenNodeKind {
    OK(OkTokenNode),
    MISSING(MissingTokenNode),
    SKIPPED(SkippedTokenNode),
}

#[derive(Debug, Clone)]
pub struct TokenNode(Rc<RefCell<CoreTokenNode>>);
impl TokenNode {
    pub fn new_with_ok_token(token: &Token, lookahead: usize) -> Self {
        TokenNode(Rc::new(RefCell::new(CoreTokenNode{
            kind: TokenNodeKind::OK(OkTokenNode::new(token, lookahead)),
            parent: None,
        })))
    }

    pub fn new_with_missing_token(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self {
        TokenNode(Rc::new(RefCell::new(CoreTokenNode{
            kind: TokenNodeKind::MISSING(MissingTokenNode::new(expected_symbols, received_token, lookahead)),
            parent: None,
        })))
    }

    pub fn new_with_skipped_token(skipped_token: &Token, lookahead: usize) -> Self {
        TokenNode(Rc::new(RefCell::new(CoreTokenNode{
            kind: TokenNodeKind::SKIPPED(SkippedTokenNode::new(skipped_token, lookahead)),
            parent: None,
        })))
    }

    pub fn is_ok(&self) -> Option<TokenNode> {
        match &self.0.as_ref().borrow().kind {
            TokenNodeKind::OK(_) => Some(self.clone()),
            _                    => None,
        }
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        match &self.0.as_ref().borrow().kind {
            TokenNodeKind::OK(ok_token) => {
                match ok_token.is_binary_operator() {
                    Some(operator) => return Some(operator),
                    None => None,
                }
            },
            _ => None,
        }
    }
}
impl Node for TokenNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreOkTokenNode {
    token: Token,
    lookahead: usize,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkTokenNode(Rc<RefCell<CoreOkTokenNode>>);
impl OkTokenNode {
    pub fn new(token: &Token, lookahead: usize) -> Self {
        OkTokenNode(Rc::new(RefCell::new(CoreOkTokenNode{
            token: token.clone(),
            lookahead,
            parent: None,
        })))
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        match self.0.as_ref().borrow().token.core_token {
            CoreToken::NOT_EQUAL        => Some(BinaryOperatorKind::NOT_EQUAL),
            CoreToken::DOUBLE_EQUAL     => Some(BinaryOperatorKind::DOUBLE_EQUAL),
            CoreToken::RBRACKET         => Some(BinaryOperatorKind::GREATER),
            CoreToken::GREATER_EQUAL    => Some(BinaryOperatorKind::GREATER_EQUAL),
            CoreToken::LBRACKET         => Some(BinaryOperatorKind::LESS),
            CoreToken::LESS_EQUAL       => Some(BinaryOperatorKind::LESS_EQUAL),
            CoreToken::DASH             => Some(BinaryOperatorKind::MINUS),
            CoreToken::PLUS             => Some(BinaryOperatorKind::PLUS),
            CoreToken::SLASH            => Some(BinaryOperatorKind::DIVIDE),
            CoreToken::STAR             => Some(BinaryOperatorKind::MULTIPLY),
            CoreToken::AND              => Some(BinaryOperatorKind::AND),
            CoreToken::OR               => Some(BinaryOperatorKind::OR),
            _ => None,
        }
    }
}
impl Node for OkTokenNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreMissingTokenNode {
    expected_symbols: Rc<Vec<&'static str>>,
    received_token: Token,
    lookahead: usize, 
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct MissingTokenNode(Rc<RefCell<CoreMissingTokenNode>>);
impl MissingTokenNode {
    pub fn new(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self {
        MissingTokenNode(Rc::new(RefCell::new(CoreMissingTokenNode{
            expected_symbols: expected_symbols.clone(),
            received_token: received_token.clone(),
            lookahead,
            parent: None,
        })))
    }
}
impl Node for MissingTokenNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreSkippedTokenNode {
    skipped_token: Token,
    lookahead: usize,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct SkippedTokenNode(Rc<RefCell<CoreSkippedTokenNode>>);
impl SkippedTokenNode {
    pub fn new(skipped_token: &Token, lookahead: usize) -> Self {
        SkippedTokenNode(Rc::new(RefCell::new(CoreSkippedTokenNode{
            skipped_token: skipped_token.clone(),
            lookahead,
            parent: None,
        })))
    }

    pub fn index(&self) -> usize {
        self.0.as_ref().borrow().skipped_token.index()
    }

    pub fn line_number(&self) -> usize {
        self.0.as_ref().borrow().skipped_token.line_number
    }
}
impl Node for SkippedTokenNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreExpressionNode {
    kind: ExpressionKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    ATOMIC(AtomicExpressionNode),
    UNARY(UnaryExpressionNode),
    BINARY(BinaryExpressionNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct ExpressionNode(Rc<RefCell<CoreExpressionNode>>);
impl ExpressionNode {
    pub fn new_with_atomic(atomic_expr: &AtomicExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: ExpressionKind::ATOMIC(atomic_expr.clone()),
            parent: None,
        }));
        atomic_expr.set_parent(ASTNode::EXPRESSION(Rc::downgrade(&node)));
        ExpressionNode(node)
    }

    pub fn new_with_unary(unary_expr: &UnaryExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: ExpressionKind::UNARY(unary_expr.clone()),
            parent: None,
        }));
        unary_expr.set_parent(ASTNode::EXPRESSION(Rc::downgrade(&node)));
        ExpressionNode(node)
    }

    pub fn new_with_binary(operator: &TokenNode, left_expr: &ExpressionNode, right_expr: &ExpressionNode) -> Self {
        let operator_kind = match operator.is_binary_operator() {
            Some(operator_kind) => operator_kind,
            None => unreachable!("any node passed in this method as operator should be a valid operator"),
        };
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: ExpressionKind::BINARY(BinaryExpressionNode::new(operator_kind, left_expr, right_expr)),
            parent: None,
        }));
        ExpressionNode(node)
    }

    pub fn new_with_missing_tokens(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self {
        ExpressionNode(Rc::new(RefCell::new(CoreExpressionNode{
            kind: ExpressionKind::MISSING_TOKENS(MissingTokenNode::new(expected_symbols, received_token, lookahead)),
            parent: None,
        })))
    }
}

impl Node for ExpressionNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct  CoreAtomicExpressionNode {
    kind: AtomicExpressionKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum AtomicExpressionKind {
    TRUE,
    FALSE,
    INTEGER(TokenNode),
    FLOATING_POINT_NUMBER(TokenNode),
    LITERAL(TokenNode),
    PARENTHESISED_EXPRESSION(ExpressionNode),
    ATOM(AtomNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct AtomicExpressionNode(Rc<RefCell<CoreAtomicExpressionNode>>);
impl AtomicExpressionNode {
    pub fn new_with_true() -> Self {
        AtomicExpressionNode(Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::TRUE,
            parent: None,
        })))
    }

    pub fn new_with_false() -> Self {
        AtomicExpressionNode(Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::FALSE,
            parent: None,
        })))
    }

    pub fn new_with_integer(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::INTEGER(token.clone()),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_floating_point_number(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::FLOATING_POINT_NUMBER(token.clone()),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_literal(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::LITERAL(token.clone()),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_parenthesised_expr(expr: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::PARENTHESISED_EXPRESSION(expr.clone()),
            parent: None,
        }));
        expr.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_atom(atom: &AtomNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::ATOM(atom.clone()),
            parent: None,
        }));
        atom.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_missing_tokens(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self {
        AtomicExpressionNode(Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::MISSING_TOKENS(MissingTokenNode::new(expected_symbols, received_token, lookahead)),
            parent: None,
        })))
    }
}
impl Node for AtomicExpressionNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct  CoreUnaryExpressionNode {
    kind: UnaryExpressionKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorKind {
    PLUS,
    MINUS,
    NOT,
}

#[derive(Debug, Clone)]
pub enum UnaryExpressionKind {
    ATOMIC(AtomicExpressionNode),
    UNARY((UnaryOperatorKind, UnaryExpressionNode)),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct UnaryExpressionNode(Rc<RefCell<CoreUnaryExpressionNode>>);
impl UnaryExpressionNode {
    pub fn new_with_atomic(atomic_expr: &AtomicExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreUnaryExpressionNode{
            kind: UnaryExpressionKind::ATOMIC(atomic_expr.clone()),
            parent: None,
        }));
        atomic_expr.set_parent(ASTNode::UNARY_EXPRESSION(Rc::downgrade(&node)));
        UnaryExpressionNode(node)
    }

    pub fn new_with_unary(operator: UnaryOperatorKind, unary_expr: &UnaryExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreUnaryExpressionNode{
            kind: UnaryExpressionKind::UNARY((operator, unary_expr.clone())),
            parent: None,
        }));
        unary_expr.set_parent(ASTNode::UNARY_EXPRESSION(Rc::downgrade(&node)));
        UnaryExpressionNode(node)
    }

    pub fn new_with_missing_tokens(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self {
        UnaryExpressionNode(Rc::new(RefCell::new(CoreUnaryExpressionNode{
            kind: UnaryExpressionKind::MISSING_TOKENS(MissingTokenNode::new(expected_symbols, received_token, lookahead)),
            parent: None,
        })))
    }
}
impl Node for UnaryExpressionNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreBinaryExpressionNode {
    operator_kind: BinaryOperatorKind,
    left_expr: ExpressionNode,
    right_expr: ExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum BinaryOperatorKind {
    NOT_EQUAL,
    DOUBLE_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    MINUS,
    PLUS,
    DIVIDE,
    MULTIPLY,
    AND,
    OR,
}

#[derive(Debug, Clone)]
pub struct BinaryExpressionNode(Rc<RefCell<CoreBinaryExpressionNode>>);
impl BinaryExpressionNode {
    pub fn new(operator: BinaryOperatorKind, left_expr: &ExpressionNode, right_expr: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreBinaryExpressionNode{
            operator_kind: operator,
            left_expr: left_expr.clone(),
            right_expr: right_expr.clone(),
            parent: None,
        }));
        left_expr.set_parent(ASTNode::BINARY_EXPRESSION(Rc::downgrade(&node)));
        right_expr.set_parent(ASTNode::BINARY_EXPRESSION(Rc::downgrade(&node)));
        BinaryExpressionNode(node)
    }
}
impl Node for BinaryExpressionNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreParamsNode {
    param: ExpressionNode,
    remaining_params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct ParamsNode(Rc<RefCell<CoreParamsNode>>);
impl ParamsNode {
    pub fn new_with_single_param(param: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreParamsNode{
            param: param.clone(),
            remaining_params: None,
            parent: None,
        }));
        param.set_parent(ASTNode::PARAMS(Rc::downgrade(&node)));
        ParamsNode(node)
    }

    pub fn new_with_params(param: &ExpressionNode, remaining_params: &ParamsNode) -> Self {
        let node = Rc::new(RefCell::new(CoreParamsNode{
            param: param.clone(),
            remaining_params: Some(remaining_params.clone()),
            parent: None,
        }));
        param.set_parent(ASTNode::PARAMS(Rc::downgrade(&node)));
        remaining_params.set_parent(ASTNode::PARAMS(Rc::downgrade(&node)));
        ParamsNode(node)
    }
}
impl Node for ParamsNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node)
    }
}

#[derive(Debug, Clone)]
pub struct CoreCallExpressionNode {
    function_name: TokenNode,
    params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct CallExpressionNode(Rc<RefCell<CoreCallExpressionNode>>);
impl CallExpressionNode {
    pub fn new(function_name: &TokenNode, params: &Option<ParamsNode>) -> Self {
        let node = Rc::new(RefCell::new(CoreCallExpressionNode{
            function_name: function_name.clone(),
            params: params.clone(),
            parent: None,
        }));
        function_name.set_parent(ASTNode::CALL_EXPRESSION(Rc::downgrade(&node)));
        match params {
            Some(params) => params.set_parent(ASTNode::CALL_EXPRESSION(Rc::downgrade(&node))),
            None => {},
        }
        CallExpressionNode(node)
    }
}
impl Node for CallExpressionNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreClassMethodCallNode {
    class_name: TokenNode,
    class_method_name: TokenNode,
    params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct ClassMethodCallNode(Rc<RefCell<CoreClassMethodCallNode>>);
impl ClassMethodCallNode {
    pub fn new(class_name: &TokenNode, class_method_name: &TokenNode, params: &Option<ParamsNode>) -> Self {
        let node = Rc::new(RefCell::new(CoreClassMethodCallNode{
            class_name: class_name.clone(),
            class_method_name: class_method_name.clone(),
            params: params.clone(),
            parent: None,
        }));
        class_name.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node)));
        class_method_name.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node)));
        match params {
            Some(params) => params.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node))),
            None => {},
        }
        ClassMethodCallNode(node)
    }
}
impl Node for ClassMethodCallNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreAtomNode {
    kind: AtomKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum AtomKind {
    ATOM_START(AtomStartNode),
    PROPERTRY_ACCESS(PropertyAccessNode),
    METHOD_ACCESS(MethodAccessNode),
    INDEX_ACCESS(IndexAccessNode),
}

#[derive(Debug, Clone)]
pub struct AtomNode(Rc<RefCell<CoreAtomNode>>);
impl AtomNode {
    pub fn new_with_atom_start(atom_start: &AtomStartNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode{
            kind: AtomKind::ATOM_START(atom_start.clone()),
            parent: None,
        }));
        atom_start.set_parent(ASTNode::ATOM(Rc::downgrade(&node)));
        AtomNode(node)
    }

    pub fn new_with_propertry_access(atom: &AtomNode, propertry: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode{
            kind: AtomKind::PROPERTRY_ACCESS(PropertyAccessNode::new(atom, propertry)),
            parent: None,
        }));
        AtomNode(node)
    }

    pub fn new_with_method_access(atom: &AtomNode, method_name: &TokenNode, params: &Option<ParamsNode>) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode{
            kind: AtomKind::METHOD_ACCESS(MethodAccessNode::new(atom, method_name, params)),
            parent: None,
        }));
        AtomNode(node)
    }

    pub fn new_with_index_access(atom: &AtomNode, index: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode{
            kind: AtomKind::INDEX_ACCESS(IndexAccessNode::new(atom, index)),
            parent: None,
        }));
        AtomNode(node)
    }
}
impl Node for AtomNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreAtomStartNode {
    kind: AtomStartKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum AtomStartKind {
    IDENTIFIER(TokenNode),                      // id
    FUNCTION_CALL(CallExpressionNode),          // id(...)
    CLASS_METHOD_CALL(ClassMethodCallNode)   // id::id(...)
}

#[derive(Debug, Clone)]
pub struct AtomStartNode(Rc<RefCell<CoreAtomStartNode>>);
impl AtomStartNode {
    pub fn new_with_identifier(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomStartNode{
            kind: AtomStartKind::IDENTIFIER(token.clone()),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOM_START(Rc::downgrade(&node)));
        AtomStartNode(node)
    }

    pub fn new_with_function_call(call_expr: &CallExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomStartNode{
            kind: AtomStartKind::FUNCTION_CALL(call_expr.clone()),
            parent: None,
        }));
        call_expr.set_parent(ASTNode::ATOM_START(Rc::downgrade(&node)));
        AtomStartNode(node)
    }

    pub fn new_with_class_method_call(class_name: &TokenNode, class_method_name: &TokenNode, 
        params: &Option<ParamsNode>) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomStartNode{
            kind: AtomStartKind::CLASS_METHOD_CALL(ClassMethodCallNode::new(class_name, class_method_name, params)),
            parent: None,
        }));
        AtomStartNode(node)
    }
}
impl Node for AtomStartNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CorePropertyAccessNode {
    atom: AtomNode,
    propertry: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct PropertyAccessNode(Rc<RefCell<CorePropertyAccessNode>>);
impl PropertyAccessNode {
    fn new(atom: &AtomNode, propertry: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CorePropertyAccessNode{
            atom: atom.clone(),
            propertry: propertry.clone(),
            parent: None,
        }));
        atom.set_parent(ASTNode::PROPERTY_ACCESS(Rc::downgrade(&node)));
        propertry.set_parent(ASTNode::PROPERTY_ACCESS(Rc::downgrade(&node)));
        PropertyAccessNode(node)
    }
}
impl Node for PropertyAccessNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreMethodAccessNode {
    atom: AtomNode,
    method_name: TokenNode,
    params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct MethodAccessNode(Rc<RefCell<CoreMethodAccessNode>>);
impl MethodAccessNode {
    pub fn new(atom: &AtomNode, method_name: &TokenNode, params: &Option<ParamsNode>) -> Self {
        let node = Rc::new(RefCell::new(CoreMethodAccessNode{
            atom: atom.clone(),
            method_name: method_name.clone(),
            params: params.clone(),
            parent: None,
        }));
        atom.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node)));
        method_name.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node)));
        match params {
            Some(params) => params.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node))),
            None => {}
        }
        MethodAccessNode(node)
    }
}
impl Node for MethodAccessNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreIndexAccessNode {
    atom: AtomNode,
    index: ExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct IndexAccessNode(Rc<RefCell<CoreIndexAccessNode>>);
impl IndexAccessNode {
    pub fn new(atom: &AtomNode, index: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreIndexAccessNode{
            atom: atom.clone(),
            index: index.clone(),
            parent: None,
        }));
        atom.set_parent(ASTNode::INDEX_ACCESS(Rc::downgrade(&node)));
        index.set_parent(ASTNode::INDEX_ACCESS(Rc::downgrade(&node)));
        IndexAccessNode(node)
    }
}
impl Node for IndexAccessNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

