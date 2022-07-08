use std::{rc::{Rc, Weak}, cell::RefCell};
use crate::scope::core::Scope;

pub trait Node {
    fn set_parent(&self, parent_node: Option<ASTNode>);
}

// ASTNode has weak reference to core nodes to avoid memory leaks
// See `https://doc.rust-lang.org/book/ch15-06-reference-cycles.html` for more information
pub enum ASTNode {
    BLOCK(Weak<RefCell<CoreBlockNode>>),
    STATEMENT(Weak<RefCell<CoreStatementNode>>),
    PARAM(Weak<RefCell<CoreParamNode>>),
    IDENTIFIER(Weak<RefCell<CoreIdentifierNode>>),
    TYPE_EXPRESSION(Weak<RefCell<CoreTypeExpressionNode>>),
}

pub struct CoreBlockNode {
    stmts: Vec<StatementNode>,
    params: Vec<ParamNode>,
    scope: Option<Scope>,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
pub struct BlockNode(Rc<RefCell<CoreBlockNode>>);
impl BlockNode {
    pub fn new(stmts: Vec<StatementNode>, params: Vec<ParamNode>, parent: Option<ASTNode>) -> Self {
        let node = BlockNode(Rc::new(RefCell::new(CoreBlockNode{
            stmts,
            params,
            scope: None,
            parent,
        })));
        node
    }
}
impl Node for BlockNode {
    fn set_parent(&self, parent_node: Option<ASTNode>) {
        self.0.as_ref().borrow_mut().parent = parent_node;
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
    fn set_parent(&self, parent_node: Option<ASTNode>) {
        todo!()
    }
}

pub struct CoreParamNode {
    param_name: IdentifierNode,
    param_type: TypeExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
pub struct ParamNode(Rc<RefCell<CoreParamNode>>);
impl ParamNode {
    pub fn new(param_name: &IdentifierNode, param_type: &TypeExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreParamNode{
            param_name: param_name.clone(),
            param_type: param_type.clone(),
            parent: None,
        }));
        param_name.set_parent(Some(ASTNode::PARAM(Rc::downgrade(&node))));
        param_type.set_parent(Some(ASTNode::PARAM(Rc::downgrade(&node))));
        ParamNode(node)
    }
}
impl Node for ParamNode {
    fn set_parent(&self, parent_node: Option<ASTNode>) {
        self.0.as_ref().borrow_mut().parent = parent_node;
    }
}

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

pub enum CoreTypeExpressionNode {
    ATOMIC(AtomicTypeNode),
    USER_DEFINED(IdentifierNode),
    ARRAY(ArrayTypeNode),
}

#[derive(Clone)]
pub struct TypeExpressionNode(Rc<RefCell<CoreTypeExpressionNode>>);
impl TypeExpressionNode {
    pub fn new_with_atomic_type(atomic_type: &Rc<String>) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(
            CoreTypeExpressionNode::ATOMIC(AtomicTypeNode::new(atomic_type, None))
        )))
    }

    pub fn new_with_user_defined_type(identifier: &IdentifierNode) -> Self {
        let node = Rc::new(RefCell::new(
            CoreTypeExpressionNode::USER_DEFINED(identifier.clone())
        ));
        identifier.set_parent(Some(ASTNode::TYPE_EXPRESSION(Rc::downgrade(&node))));
        TypeExpressionNode(node)
    }

    pub fn new_with_array_type(array_size: &Rc<String>, sub_type: &TypeExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(
            CoreTypeExpressionNode::ARRAY(ArrayTypeNode::new(array_size, sub_type, None))
        ));
        sub_type.set_parent(Some(ASTNode::TYPE_EXPRESSION(Rc::downgrade(&node))));
        TypeExpressionNode(node)
    }
}
impl Node for TypeExpressionNode {
    fn set_parent(&self, parent_node: Option<ASTNode>) {
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
    pub fn new(kind: &Rc<String>, parent: Option<ASTNode>) -> Self {
        AtomicTypeNode(Rc::new(RefCell::new(CoreAtomicTypeNode{
            kind: kind.clone(),
            parent,
        })))
    }
}
impl Node for AtomicTypeNode {
    fn set_parent(&self, parent_node: Option<ASTNode>) {
        self.0.as_ref().borrow_mut().parent = parent_node;
    }
}

pub struct CoreArrayTypeNode {
    sub_type: TypeExpressionNode,
    size: Rc<String>,
    parent: Option<ASTNode>,
}

#[derive(Clone)]
pub struct ArrayTypeNode(Rc<RefCell<CoreArrayTypeNode>>);
impl ArrayTypeNode {
    pub fn new(size: &Rc<String>, sub_type: &TypeExpressionNode, parent: Option<ASTNode>) -> Self {
        ArrayTypeNode(Rc::new(RefCell::new(CoreArrayTypeNode{
            sub_type: sub_type.clone(),
            size: size.clone(),
            parent,
        })))
    }
}
impl Node for ArrayTypeNode {
    fn set_parent(&self, parent_node: Option<ASTNode>) {
        self.0.as_ref().borrow_mut().parent = parent_node;
    }
}