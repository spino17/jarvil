use std::cell::{RefCell, Ref};
use std::rc::Rc;
use crate::types::Type;

trait Node {
    fn traverse(&self);
}

pub enum ASTNode {
    VARIABLE_DECLARATION(VariableDeclarationNode),
    R_ASSIGNMENT(RAssignmentNode),
    ROUTINE_CALL(RoutineCallNode),
    PARAM(ParamNode),
    BEXPR(BexprNode),
    EXPR(ExprNode),
    ATOM(AtomNode),
    IDENTIFIER(IdentifierNode),
    LITERAL(LiteralNode),
}

pub struct VariableDeclarationNode(Rc<RefCell<CoreVariableDeclarationNode>>);
struct CoreVariableDeclarationNode {
    left_side: IdentifierNode,
    right_side: RAssignmentNode,
}

pub struct RAssignmentNode(Rc<RefCell<CoreRAssignmentNode>>);
enum CoreRAssignmentNode {
    PARAM(ParamNode),
}

pub struct RoutineCallNode(Rc<RefCell<CoreRoutineCallNode>>);
struct CoreRoutineCallNode {
    name: Rc<String>,
    kind: RoutineKind,
    // params
    // return type
}

enum RoutineKind {
    FUNCTION,
    LAMBDA,
    CONSTRUCTOR,
    CLASS_METHOD
}

pub struct ParamNode(Rc<RefCell<CoreParamNode>>);
enum CoreParamNode {
    BXPR(BexprNode),
    EXPR(ExprNode),
    ATOM(AtomNode),
    LITERAL(LiteralNode),
}

pub struct BexprNode(Rc<RefCell<CoreBexprNode>>);
struct CoreBexprNode {

}

pub struct ExprNode(Rc<RefCell<CoreExprNode>>);
struct CoreExprNode {

}

pub struct AtomNode(Rc<RefCell<CoreAtomNode>>);
enum CoreAtomNode {
    START(AtomStartNode),
    INDEX_ACCESS(IndexAccessNode),
    PROPERTRY_ACCESS(PropertryAccessNode),
    METHOD_ACCESS(MethodAccessNode),
}

pub struct AtomStartNode(Rc<RefCell<CoreAtomStartNode>>);
enum CoreAtomStartNode {
    ROUTINE_CALL(RoutineCallNode),
    IDENTIFIER,
}

pub struct IndexAccessNode(Rc<RefCell<CoreIndexAccessNode>>);
struct CoreIndexAccessNode {
    indexable_atom: AtomNode,
    index: ParamNode,
}

pub struct PropertryAccessNode(Rc<RefCell<CorePropertryAccessNode>>);
struct CorePropertryAccessNode {
    object: AtomNode,
    propertry: Rc<String>,
}

pub struct MethodAccessNode(Rc<RefCell<CoreMethodAccessNode>>);
struct CoreMethodAccessNode {
    object: AtomNode,
    method: RoutineCallNode,
}

pub struct IdentifierNode(Rc<RefCell<CoreIdentifierNode>>);
struct CoreIdentifierNode {
    name: Rc<String>,
    data_type: Type,
}

pub struct LiteralNode(Rc<RefCell<CoreLiteralNode>>);
struct CoreLiteralNode {
    value: Rc<String>,
}