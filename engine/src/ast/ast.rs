// AST Nodes have inner mutability to enable dynamic changes to AST like monomorphism of generics or macro expansion.
// ASTNode has weak reference to core nodes to avoid memory leaks. 
// See `https://doc.rust-lang.org/book/ch15-06-reference-cycles.html` for more information

use std::{rc::{Rc, Weak}, cell::RefCell};
use crate::{scope::core::Scope, lexer::token::Token, types::atomic};

pub trait Node {
    fn set_parent(&self, parent_node: ASTNode);
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    BLOCK(Weak<RefCell<CoreBlockNode>>),
    STATEMENT(Weak<RefCell<CoreStatementNode>>),
    PARAMS(Weak<RefCell<CoreParamsNode>>),
    PARAM(Weak<RefCell<CoreParamNode>>),
    TYPE_EXPRESSION(Weak<RefCell<CoreTypeExpressionNode>>),
    ATOMIC_TYPE(Weak<RefCell<CoreAtomicTypeNode>>),
    ARRAY_TYPE(Weak<RefCell<CoreArrayTypeNode>>),
    USER_DEFINED_TYPE(Weak<RefCell<CoreUserDefinedTypeNode>>),
    LEADING_SKIPPED_TOKENS(Weak<RefCell<CoreSkippedTokens>>),
    TRAILING_SKIPPED_TOKEN(Weak<RefCell<CoreSkippedTokens>>),
    EXTRA_NEWLINES(Weak<RefCell<CoreSkippedTokens>>),
    EXPRESSION(Weak<RefCell<CoreExpressionNode>>),
    ATOMIC_EXPRESSION(Weak<RefCell<CoreAtomicExpressionNode>>),
    UNARY_EXPRESSION(Weak<RefCell<CoreUnaryExpressionNode>>),
    BINARY_EXPRESSION(Weak<RefCell<CoreBinaryExpressionNode>>),
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
    params: Option<ParamsNode>,
    scope: Option<Scope>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct BlockNode(Rc<RefCell<CoreBlockNode>>);
impl BlockNode {
    pub fn new(stmts: &Rc<RefCell<Vec<StatemenIndentWrapper>>>, params: Option<&ParamsNode>) -> Self {
        let node = Rc::new(RefCell::new(CoreBlockNode{
            stmts: stmts.clone(),
            params: match params {
                Some(params) => Some(params.clone()),
                None => None,
            },
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
        match params {
            Some(params) => params.set_parent(ASTNode::BLOCK(Rc::downgrade(&node))),
            None => {}
        }
        BlockNode(node)
    }
}
impl Node for BlockNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

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
            skipped_token.set_parent(ASTNode::LEADING_SKIPPED_TOKENS(Rc::downgrade(&node)));
        }
        SkippedTokens(node)
    }

    pub fn new_with_trailing_skipped_tokens(skipped_tokens: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens{
            skipped_tokens: skipped_tokens.clone(),
            parent: None,
        }));
        for skipped_token in skipped_tokens.as_ref() {
            skipped_token.set_parent(ASTNode::TRAILING_SKIPPED_TOKEN(Rc::downgrade(&node)));
        }
        SkippedTokens(node)
    }

    pub fn new_with_extra_newlines(extra_newlines: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens{
            skipped_tokens: extra_newlines.clone(),
            parent: None,
        }));
        for skipped_token in extra_newlines.as_ref() {
            skipped_token.set_parent(ASTNode::EXTRA_NEWLINES(Rc::downgrade(&node)));
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
pub enum CoreStatementNode {
    // expr, variable declaration, type struct declaration, type lambda declaration, interface declaration, 
    // assignment, if, for, while, return, continue, break, implementation of interfaces, implementation of structs
}

#[derive(Debug, Clone)]
pub struct StatementNode(Rc<RefCell<CoreStatementNode>>);
impl StatementNode {
    pub fn new() -> Self {
        todo!()
    }
}
impl Node for StatementNode {
    fn set_parent(&self, parent_node: ASTNode) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct CoreParamsNode {
    param: ParamNode,
    remaining_params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct ParamsNode(Rc<RefCell<CoreParamsNode>>);
impl ParamsNode {
    pub fn new(param: &ParamNode, remaining_params: Option<&ParamsNode>) -> Self {
        let node = Rc::new(RefCell::new(CoreParamsNode{
            param: param.clone(),
            remaining_params: match remaining_params {
                Some(remaining_params) => Some(remaining_params.clone()),
                None => None,
            },
            parent: None,
        }));
        param.set_parent(ASTNode::PARAMS(Rc::downgrade(&node)));
        match remaining_params {
            Some(remaining_params) => remaining_params.set_parent(ASTNode::PARAMS(Rc::downgrade(&node))),
            None => {}
        }
        ParamsNode(node)
    }
}
impl Node for ParamsNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreParamNode {
    param_name: TokenNode,
    param_type: TypeExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct ParamNode(Rc<RefCell<CoreParamNode>>);
impl ParamNode {
    pub fn new(param_name: &TokenNode, param_type: &TypeExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreParamNode{
            param_name: param_name.clone(),
            param_type: param_type.clone(),
            parent: None,
        }));
        param_name.set_parent(ASTNode::PARAM(Rc::downgrade(&node)));
        param_type.set_parent(ASTNode::PARAM(Rc::downgrade(&node)));
        ParamNode(node)
    }
}
impl Node for ParamNode {
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
}
impl Node for SkippedTokenNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

#[derive(Debug, Clone)]
pub struct CoreExpressionNode {
    kind: CoreExpressionKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum CoreExpressionKind {
    ATOMIC(AtomicExpressionNode),
    UNARY(UnaryExpressionNode),
    BINARY(BinaryExpressionNode),
}

#[derive(Debug, Clone)]
pub struct ExpressionNode(Rc<RefCell<CoreExpressionNode>>);
impl ExpressionNode {
    pub fn new_with_atomic(atomic_expr: &AtomicExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: CoreExpressionKind::ATOMIC(atomic_expr.clone()),
            parent: None,
        }));
        atomic_expr.set_parent(ASTNode::EXPRESSION(Rc::downgrade(&node)));
        ExpressionNode(node)
    }

    pub fn new_with_unary(unary_expr: &UnaryExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: CoreExpressionKind::UNARY(unary_expr.clone()),
            parent: None,
        }));
        unary_expr.set_parent(ASTNode::EXPRESSION(Rc::downgrade(&node)));
        ExpressionNode(node)
    }

    pub fn new_with_binary(binary_expr: &BinaryExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: CoreExpressionKind::BINARY(binary_expr.clone()),
            parent: None,
        }));
        binary_expr.set_parent(ASTNode::EXPRESSION(Rc::downgrade(&node)));
        ExpressionNode(node)
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
    INTEGER,
    FLOATING_POINT_NUMBER,
    STRING_LITERAL,
    PARENTHESISED_EXPRESSION(ExpressionNode),
    ATOM(),  // add atom node here
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

    pub fn new_with_integer() -> Self {
        AtomicExpressionNode(Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::INTEGER,
            parent: None,
        })))
    }

    pub fn new_with_floating_point_number() -> Self {
        AtomicExpressionNode(Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::FLOATING_POINT_NUMBER,
            parent: None,
        })))
    }

    pub fn new_with_parenthesised_expr(expr: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::PARENTHESISED_EXPRESSION(expr.clone()),
            parent: None,
        }));
        expr.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_atom() -> Self {
        AtomicExpressionNode(Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::ATOM(),
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
    DASH,
    NOT,
}

#[derive(Debug, Clone)]
pub enum UnaryExpressionKind {
    ATOMIC(AtomicExpressionNode),
    UNARY((UnaryOperatorKind, UnaryExpressionNode))
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



