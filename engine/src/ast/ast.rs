use std::cell::{RefCell};
use std::rc::Rc;
use crate::types::core::{Type, CoreType};
use crate::scope::Env;

trait Node {
    fn traverse(&self);
}

pub enum ASTNode {
    VARIABLE_DECLARATION(VariableDeclarationNode),
    R_ASSIGNMENT(RAssignmentNode),
    ROUTINE_CALL(RoutineCallNode),
    PARAMS(ParamsNode),
    PARAM(ParamNode),
    BEXPR(BexprNode),
    EXPR(ExprNode),
    ATOM(AtomNode),
    IDENTIFIER(IdentifierNode),
    LITERAL(LiteralNode),
}

pub struct VariableDeclarationNode(pub Rc<RefCell<CoreVariableDeclarationNode>>, pub Env);
pub struct CoreVariableDeclarationNode {
    pub left_side: IdentifierNode,
    pub right_side: RAssignmentNode,
}

pub struct RAssignmentNode(pub Rc<RefCell<CoreRAssignmentNode>>, pub Env);
pub enum CoreRAssignmentNode {
    PARAM(ParamNode),
}

pub struct RoutineCallNode(pub Rc<RefCell<CoreRoutineCallNode>>, pub Env);
pub struct CoreRoutineCallNode {
    pub name: Rc<String>,
    pub kind: RoutineKind,
    pub params: Option<ParamsNode>,
}
pub enum RoutineKind {
    FUNCTION,
    LAMBDA,
    CONSTRUCTOR(Rc<String>),    // struct name key to symbol table
    METHOD(Rc<String>),         // struct name key to symbol table
    CLASS_METHOD(Rc<String>),   // struct name key to symbol table
}

pub struct ParamsNode(pub Rc<RefCell<CoreParamsNode>>, pub Env);
pub struct CoreParamsNode {
    pub param: ParamNode,
    pub remaining_params: Option<ParamsNode>,
}

pub struct ParamNode(pub Rc<RefCell<CoreParamNode>>, pub Env);
pub enum CoreParamNode {
    BXPR(BexprNode),
    EXPR(ExprNode),
    ATOM(AtomNode),
    LITERAL(LiteralNode),
}

pub struct BexprNode(pub Rc<RefCell<CoreBexprNode>>, pub Env);
pub struct CoreBexprNode {

}

pub struct ExprNode(pub Rc<RefCell<CoreExprNode>>, pub Env);
pub struct CoreExprNode {

}

pub struct AtomNode(pub Rc<RefCell<CoreAtomNode>>, pub Env);
pub struct CoreAtomNode {
    pub kind: AtomKind,
    pub data_type: Type,
}
pub enum AtomKind {
    START(AtomStartNode),
    INDEX_ACCESS(IndexAccessNode),
    PROPERTRY_ACCESS(PropertryAccessNode),
    METHOD_ACCESS(MethodAccessNode),
}

pub struct AtomStartNode(pub Rc<RefCell<CoreAtomStartNode>>, pub Env);
pub enum CoreAtomStartNode {
    ROUTINE_CALL(RoutineCallNode),
    IDENTIFIER(IdentifierNode),
}

pub struct IndexAccessNode(pub Rc<RefCell<CoreIndexAccessNode>>, pub Env);
pub struct CoreIndexAccessNode {
    pub indexable_atom: AtomNode,
    pub index: ParamNode,
}

pub struct PropertryAccessNode(pub Rc<RefCell<CorePropertryAccessNode>>, pub Env);
pub struct CorePropertryAccessNode {
    pub object: AtomNode,
    pub propertry: Rc<String>,
}

pub struct MethodAccessNode(pub Rc<RefCell<CoreMethodAccessNode>>, pub Env);
pub struct CoreMethodAccessNode {
    pub object: AtomNode,
    pub method: RoutineCallNode,
}

pub struct IdentifierNode(pub Rc<RefCell<CoreIdentifierNode>>, pub Env);
pub struct CoreIdentifierNode {
    pub name: Rc<String>,
}

pub struct LiteralNode(pub Rc<RefCell<CoreLiteralNode>>, pub Env);
pub struct CoreLiteralNode {
    pub value: Rc<String>,
}