use std::{rc::{Rc, Weak}, cell::RefCell};
use crate::{scope::core::Scope, lexer::token::{TokenKind, Token, MissingToken}};

pub trait Node {
    fn set_parent(&self, parent_node: ASTNode);
}

// ASTNode has weak reference to core nodes to avoid memory leaks
// See `https://doc.rust-lang.org/book/ch15-06-reference-cycles.html` for more information
#[derive(Clone)]
pub enum ASTNode {
    BLOCK(Weak<RefCell<CoreBlockNode>>),
    STATEMENT(Weak<RefCell<CoreStatementNode>>),
    PARAMS(Weak<RefCell<CoreParamsNode>>),
    PARAM(Weak<RefCell<CoreParamNode>>),
    TYPE_EXPRESSION(Weak<RefCell<CoreTypeExpressionNode>>),
    ARRAY_TYPE(Weak<RefCell<CoreArrayTypeNode>>),
    USER_DEFINED_TYPE(Weak<RefCell<CoreUserDefinedTypeNode>>),
    TRAILING_SKIPPED_TOKEN(Weak<RefCell<CoreTrailingSkippedTokens>>),
}

pub enum StatemenIndentWrapper {
    CORRECTLY_INDENTED(StatementNode),
    INCORRECTLY_INDENTED((StatementNode, (i64, i64))),
    TRAILING_SKIPPED_TOKENS(TrailingSkippedTokens),
}

pub struct CoreBlockNode {
    stmts: Rc<RefCell<Vec<StatemenIndentWrapper>>>,
    params: Option<ParamsNode>,
    scope: Option<Scope>,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
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
                StatemenIndentWrapper::TRAILING_SKIPPED_TOKENS(trailing_skipped_tokens) => {
                    trailing_skipped_tokens.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
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

pub struct CoreTrailingSkippedTokens {
    skipped_tokens: Rc<Vec<TokenNode>>,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
pub struct TrailingSkippedTokens(Rc<RefCell<CoreTrailingSkippedTokens>>);
impl TrailingSkippedTokens {
    pub fn new(skipped_tokens: &Rc<Vec<TokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreTrailingSkippedTokens{
            skipped_tokens: skipped_tokens.clone(),
            parent: None,
        }));
        for skipped_token in skipped_tokens.as_ref() {
            skipped_token.set_parent(ASTNode::TRAILING_SKIPPED_TOKEN(Rc::downgrade(&node)));
        }
        TrailingSkippedTokens(node)
    }
}
impl Node for TrailingSkippedTokens {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

pub enum CoreStatementNode {
    // expr, variable declaration, type struct declaration, type lambda declaration, assignment, if, for, while, return,
    // continue, break
}

#[derive(Clone)]
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

pub struct CoreParamsNode {
    param: ParamNode,
    remaining_params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
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

pub struct CoreParamNode {
    param_name: TokenNode,
    param_type: TypeExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
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

/*
pub struct CoreIdentifierNode {
    value: Rc<String>,
    start_index: usize,
    end_index: usize,
    line_number: usize,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
pub struct IdentifierNode(Rc<RefCell<CoreIdentifierNode>>);
impl IdentifierNode {
    pub fn new(value: &Rc<String>, start_index: usize, end_index: usize, line_number: usize) -> Self {
        IdentifierNode(Rc::new(RefCell::new(CoreIdentifierNode{
            value: value.clone(),
            start_index,
            end_index,
            line_number,
            parent: None,
        })))
    }
}
impl Node for IdentifierNode {
    fn set_parent(&self, parent_node: Option<ASTNode>) {
        self.0.as_ref().borrow_mut().parent = parent_node;
    }
}
 */

pub enum CoreTypeExpressionNode {
    ATOMIC(AtomicTypeNode),
    USER_DEFINED(UserDefinedTypeNode),
    ARRAY(ArrayTypeNode),
}

#[derive(Clone)]
pub struct TypeExpressionNode(Rc<RefCell<CoreTypeExpressionNode>>);
impl TypeExpressionNode {
    pub fn new_with_atomic_type(atomic_type: &Rc<String>) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(
            CoreTypeExpressionNode::ATOMIC(AtomicTypeNode::new(atomic_type))
        )))
    }

    pub fn new_with_user_defined_type(identifier: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(
            CoreTypeExpressionNode::USER_DEFINED(UserDefinedTypeNode::new(identifier))
        ));
        TypeExpressionNode(node)
    }

    pub fn new_with_array_type(array_size: &TokenNode, sub_type: &TypeExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(
            CoreTypeExpressionNode::ARRAY(ArrayTypeNode::new(array_size, sub_type))
        ));
        TypeExpressionNode(node)
    }
}
impl Node for TypeExpressionNode {
    fn set_parent(&self, parent_node: ASTNode) {
        match &*self.0.as_ref().borrow_mut() {
            CoreTypeExpressionNode::ATOMIC(atomic_node_data) => {
                atomic_node_data.set_parent(parent_node);
            },
            CoreTypeExpressionNode::USER_DEFINED(user_defined_node_data) => {
                user_defined_node_data.set_parent(parent_node);
            },
            CoreTypeExpressionNode::ARRAY(array_node_data) => {
                array_node_data.set_parent(parent_node);
            }
        }
    }
}

pub struct CoreAtomicTypeNode {
    kind: Rc<String>,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
pub struct AtomicTypeNode(Rc<RefCell<CoreAtomicTypeNode>>);
impl AtomicTypeNode {
    pub fn new(kind: &Rc<String>) -> Self {
        AtomicTypeNode(Rc::new(RefCell::new(CoreAtomicTypeNode{
            kind: kind.clone(),
            parent: None,
        })))
    }
}
impl Node for AtomicTypeNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}

pub struct CoreArrayTypeNode {
    sub_type: TypeExpressionNode,
    size: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
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

pub struct CoreUserDefinedTypeNode {
    token: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
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

pub struct CoreTokenNode {
    kind: TokenKind,
    parent: Option<ASTNode>,
    lookahead: usize,
}

#[derive(Clone)]
pub struct TokenNode(Rc<RefCell<CoreTokenNode>>);
impl TokenNode {
    pub fn new_with_token(token: &Token, lookahead: usize) -> Self {
        TokenNode(Rc::new(RefCell::new(CoreTokenNode{
            kind: TokenKind::AVAILABLE(token.clone()),
            parent: None,
            lookahead,
        })))
    }

    pub fn new_with_missing_token(expected_symbol: &Rc<String>, received_token: &Token, lookahead: usize) -> Self {
        // TODO - log the related error into a error log struct to output on terminal based compilation
        TokenNode(Rc::new(RefCell::new(CoreTokenNode{
            kind: TokenKind::MISSING(MissingToken{
                expected_symbol: expected_symbol.clone(),
                received_token: received_token.clone(),
            }),
            parent: None,
            lookahead,
        })))
    }

    pub fn new_with_skipped_token(skipped_token: &Token, lookahead: usize) -> Self {
        // TODO - log the related error into a error log struct to output on terminal based compilation
        TokenNode(Rc::new(RefCell::new(CoreTokenNode{
            kind: TokenKind::SKIPPED(skipped_token.clone()),
            parent: None,
            lookahead,
        })))
    }
}
impl Node for TokenNode {
    fn set_parent(&self, parent_node: ASTNode) {
        self.0.as_ref().borrow_mut().parent = Some(parent_node);
    }
}
