use std::cell::{RefCell};
use std::rc::Rc;
use crate::types::{Type, CoreType};
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

pub struct VariableDeclarationNode(Rc<RefCell<CoreVariableDeclarationNode>>, Env);
impl VariableDeclarationNode {
    pub fn new(env: &Env, 
        left_side: &IdentifierNode, right_side: &RAssignmentNode) -> Self {
        VariableDeclarationNode(Rc::new(RefCell::new(CoreVariableDeclarationNode{
            left_side: IdentifierNode(left_side.0.clone(), Env(env.0.clone())),
            right_side: RAssignmentNode(right_side.0.clone(), Env(env.0.clone())),
        })), Env(env.0.clone()))
    }
}
struct CoreVariableDeclarationNode {
    left_side: IdentifierNode,
    right_side: RAssignmentNode,
}

pub struct RAssignmentNode(Rc<RefCell<CoreRAssignmentNode>>, Env);
impl RAssignmentNode {
    pub fn new_with_param(env: &Env, param: &ParamNode) -> Self {
        RAssignmentNode(Rc::new(RefCell::new(
            CoreRAssignmentNode::PARAM(ParamNode(param.0.clone(), Env(env.0.clone()))))
        ), Env(env.0.clone()))
    }
}
enum CoreRAssignmentNode {
    PARAM(ParamNode),
}

pub struct RoutineCallNode(Rc<RefCell<CoreRoutineCallNode>>, Env);
struct CoreRoutineCallNode {
    name: Rc<String>,
    kind: RoutineKind,
    params: Option<ParamsNode>,
}
enum RoutineKind {
    FUNCTION,
    LAMBDA,
    CONSTRUCTOR,
    CLASS_METHOD(Rc<String>),  // struct name key to symbol table
}

pub struct ParamsNode(Rc<RefCell<CoreParamsNode>>, Env);
struct CoreParamsNode {
    param: ParamNode,
    remaining_params: Option<ParamsNode>,
}

pub struct ParamNode(Rc<RefCell<CoreParamNode>>, Env);
enum CoreParamNode {
    BXPR(BexprNode),
    EXPR(ExprNode),
    ATOM(AtomNode),
    LITERAL(LiteralNode),
}

pub struct BexprNode(Rc<RefCell<CoreBexprNode>>, Env);
struct CoreBexprNode {

}

pub struct ExprNode(Rc<RefCell<CoreExprNode>>, Env);
struct CoreExprNode {

}

pub struct AtomNode(Rc<RefCell<CoreAtomNode>>, Env);
struct CoreAtomNode {
    kind: AtomKind,
    data_type: Type,
}
enum AtomKind {
    START(AtomStartNode),
    INDEX_ACCESS(IndexAccessNode),
    PROPERTRY_ACCESS(PropertryAccessNode),
    METHOD_ACCESS(MethodAccessNode),
}

pub struct AtomStartNode(Rc<RefCell<CoreAtomStartNode>>, Env);
enum CoreAtomStartNode {
    ROUTINE_CALL(RoutineCallNode),
    IDENTIFIER(IdentifierNode),
}

pub struct IndexAccessNode(Rc<RefCell<CoreIndexAccessNode>>, Env);
struct CoreIndexAccessNode {
    indexable_atom: AtomNode,
    index: ParamNode,
}

pub struct PropertryAccessNode(Rc<RefCell<CorePropertryAccessNode>>, Env);
struct CorePropertryAccessNode {
    object: AtomNode,
    propertry: Rc<String>,
}

pub struct MethodAccessNode(Rc<RefCell<CoreMethodAccessNode>>, Env);
struct CoreMethodAccessNode {
    object: AtomNode,
    method: RoutineCallNode,
}

pub struct IdentifierNode(Rc<RefCell<CoreIdentifierNode>>, Env);
struct CoreIdentifierNode {
    name: Rc<String>,
}

pub struct LiteralNode(Rc<RefCell<CoreLiteralNode>>, Env);
struct CoreLiteralNode {
    value: Rc<String>,
}