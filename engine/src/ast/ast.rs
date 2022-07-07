use std::{rc::Rc, cell::RefCell};
use crate::scope::core::Scope;

pub enum ASTNode {
    BLOCK(BlockNode),
    STATEMENT(StatementNode),
    PARAM(ParamNode),
    IDENTIFIER(IdentifierNode),
    TYPE_EXPRESSION(TypeExpressionNode),
}

#[derive(Clone)]
pub struct BlockNode(Rc<RefCell<CoreBlockNode>>);
impl BlockNode {
    pub fn new(stmts: Vec<StatementNode>, params: Vec<ParamNode>) -> Self {
        BlockNode(Rc::new(RefCell::new(CoreBlockNode{
            stmts,
            params,
            scope: None,
        })))
    }
}
struct CoreBlockNode {
    stmts: Vec<StatementNode>,
    params: Vec<ParamNode>,
    scope: Option<Scope>,
}

#[derive(Clone)]
pub struct StatementNode(Rc<RefCell<CoreStatementNode>>);
impl StatementNode {
    pub fn new() -> Self {
        todo!()
    }
}
enum CoreStatementNode {
    // expr, variable declaration, type struct declaration, type lambda declaration, assignment, if, for, while, return,
    // continue, break
}

#[derive(Clone)]
pub struct ParamNode(Rc<RefCell<CoreParamNode>>);
impl ParamNode {
    pub fn new(param_name: &IdentifierNode, param_type: &TypeExpressionNode) -> Self {
        ParamNode(Rc::new(RefCell::new(CoreParamNode{
            param_name: param_name.clone(),
            param_type: param_type.clone(),
        })))
    }
}
struct CoreParamNode {
    param_name: IdentifierNode,
    param_type: TypeExpressionNode,
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
        })))
    }
}
struct CoreIdentifierNode {
    value: Rc<String>,
    start_index: usize,
    end_index: usize,
    line_number: usize,
}

#[derive(Clone)]
pub struct TypeExpressionNode(Rc<RefCell<CoreTypeExpressionNode>>);
impl TypeExpressionNode {
    pub fn new_with_atomic_type(atomic_type: &Rc<String>) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode::ATOMIC(atomic_type.clone()))))
    }

    pub fn new_with_user_defined_type(identifier: &IdentifierNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode::USER_DEFINED(identifier.clone()))))
    }

    pub fn new_with_array_type(array_size: Rc<String>, sub_type: &TypeExpressionNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode::ARRAY((array_size, sub_type.clone())))))
    }
}
enum CoreTypeExpressionNode {
    ATOMIC(Rc<String>),
    USER_DEFINED(IdentifierNode),
    ARRAY((Rc<String>, TypeExpressionNode))
}