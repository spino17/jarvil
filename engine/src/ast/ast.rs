use std::{rc::Rc, cell::RefCell};

pub enum ASTNode {
    IDENTIFIER(IdentifierNode),
    TYPE_EXPRESSION(TypeExpressionNode),
}

pub struct IdentifierNode(pub Rc<RefCell<CoreIdentifierNode>>);
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
pub struct CoreIdentifierNode {
    value: Rc<String>,
    start_index: usize,
    end_index: usize,
    line_number: usize,
}

pub struct TypeExpressionNode(pub Rc<RefCell<CoreTypeExpressionNode>>);
impl TypeExpressionNode {
    pub fn new_with_atomic_type(atomic_type: &Rc<String>) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode::ATOMIC(atomic_type.clone()))))
    }

    pub fn new_with_user_defined_type(identifier: &IdentifierNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode::USER_DEFINED(IdentifierNode(identifier.0.clone())))))
    }

    pub fn new_with_array_type(array_size: Rc<String>, sub_type: &TypeExpressionNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode::ARRAY((array_size, TypeExpressionNode(sub_type.0.clone()))))))
    }
}
pub enum CoreTypeExpressionNode {
    ATOMIC(Rc<String>),
    USER_DEFINED(IdentifierNode),
    ARRAY((Rc<String>, TypeExpressionNode))
}