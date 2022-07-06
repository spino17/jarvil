use crate::ast::ast::{VariableDeclarationNode, 
    IdentifierNode, RAssignmentNode, ParamNode, BexprNode, ExprNode, CoreVariableDeclarationNode, CoreRAssignmentNode,
    RoutineCallNode, RoutineKind, ParamsNode, CoreRoutineCallNode, CoreParamsNode, AtomNode, LiteralNode, AtomStartNode,
    IndexAccessNode};
use crate::scope::Env;
use std::cell::RefCell;
use std::rc::Rc;
use crate::types::Type;

use super::ast::{PropertryAccessNode, MethodAccessNode};

impl VariableDeclarationNode {
    pub fn new(env: &Env, 
        left_side: &IdentifierNode, right_side: &RAssignmentNode) -> Self {
        VariableDeclarationNode(Rc::new(RefCell::new(CoreVariableDeclarationNode{
            left_side: IdentifierNode(left_side.0.clone(), Env(env.0.clone())),
            right_side: RAssignmentNode(right_side.0.clone(), Env(env.0.clone())),
        })), Env(env.0.clone()))
    }
}

impl RAssignmentNode {
    pub fn new_with_param(env: &Env, param: &ParamNode) -> Self {
        RAssignmentNode(Rc::new(RefCell::new(
            CoreRAssignmentNode::PARAM(ParamNode(param.0.clone(), Env(env.0.clone()))))
        ), Env(env.0.clone()))
    }
}

impl RoutineCallNode {
    pub fn new(env: &Env, routine_name: &Rc<String>, kind: RoutineKind, params: Option<&ParamsNode>) -> Self {
        RoutineCallNode(Rc::new(RefCell::new(CoreRoutineCallNode{
            name: routine_name.clone(),
            kind,
            params: match params {
                Some(params) => Some(ParamsNode(params.0.clone(), Env(env.0.clone()))),
                None => None,
            }
        })), Env(env.0.clone()))
    }
}

impl ParamsNode {
    pub fn new(env: &Env, param: &ParamNode, remaining_params: Option<&ParamsNode>) -> Self {
        ParamsNode(Rc::new(RefCell::new(CoreParamsNode{
            param: ParamNode(param.0.clone(), Env(env.0.clone())),
            remaining_params: match remaining_params {
                Some(remaining_params) => Some(ParamsNode(remaining_params.0.clone(), Env(env.0.clone()))),
                None => None,
            }
        })), Env(env.0.clone()))
    }
}

impl ParamNode {
    pub fn new_with_bexpr(env: &Env, bexpr: &BexprNode) -> Self {
        todo!()
    }

    pub fn new_with_expr(env: &Env, bexpr: &ExprNode) -> Self {
        todo!()
    }

    pub fn new_with_atom(env: &Env, bexpr: &AtomNode) -> Self {
        todo!()
    }

    pub fn new_with_literal(env: &Env, bexpr: &LiteralNode) -> Self {
        todo!()
    }
}

impl BexprNode {
    pub fn new(env: &Env) -> Self {
        todo!()
    }
}

impl ExprNode {
    pub fn new(env: &Env) -> Self {
        todo!()
    }
}

impl AtomNode {
    pub fn new_with_start(env: &Env, data_type: &Type, atom_start: &AtomStartNode) -> Self {
        todo!()
    }

    pub fn new_with_index_access(env: &Env, data_type: &Type, index_access: &IndexAccessNode) -> Self {
        todo!()
    }

    pub fn new_with_propertry_access(env: &Env, data_type: &Type, propertry_access: &PropertryAccessNode) -> Self {
        todo!()
    }

    pub fn new_with_method_access(env: &Env, data_type: &Type, method_access: &MethodAccessNode) -> Self {
        todo!()
    }

    pub fn get_type(&self) -> Type {
        Type(self.0.as_ref().borrow().data_type.0.clone())
    }
}

impl IdentifierNode {
    pub fn new(env: &Env, name: &Rc<String>) -> Self {
        todo!()
    }
}

impl LiteralNode {
    pub fn new(env: &Env, value: &Rc<String>) -> Self {
        todo!()
    }
}