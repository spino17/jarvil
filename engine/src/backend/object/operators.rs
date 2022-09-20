use super::{
    core::{CoreObject, Object},
    list::ListObject,
};
use crate::{
    backend::{
        data::Data,
        vm::{self, VM},
    },
    error::constants::TYPE_CHECK_BUG_ERROR_MSG,
    lexer::token::BinaryOperatorKind,
};

pub fn eval_obj_binary_op(
    l_obj: &Object,
    r_obj: &Object,
    op_kind: BinaryOperatorKind,
    vm: &mut VM,
) -> Data {
    match op_kind {
        BinaryOperatorKind::Add => eval_obj_add(l_obj, r_obj, vm),
        BinaryOperatorKind::NotEqual => eval_obj_not_equal(l_obj, r_obj, vm),
        BinaryOperatorKind::DoubleEqual => eval_obj_double_equal(l_obj, r_obj, vm),
        BinaryOperatorKind::Subtract => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        BinaryOperatorKind::Multiply => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        BinaryOperatorKind::Divide => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        BinaryOperatorKind::Greater => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        BinaryOperatorKind::Less => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        BinaryOperatorKind::GreaterEqual => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        BinaryOperatorKind::LessEqual => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        BinaryOperatorKind::And => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        BinaryOperatorKind::Or => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn eval_obj_add(l_obj: &Object, r_obj: &Object, vm: &mut VM) -> Data {
    match &l_obj.core {
        CoreObject::STRING(l_str) => {
            let r_str = r_obj.as_string();
            Data::OBJ(Object::new_with_string(l_str.clone() + r_str, vm))
        }
        CoreObject::LIST(l_list) => {
            let r_list = r_obj.as_list();
            Data::OBJ(Object::new_with_list(l_list.clone() + r_list, vm))
        }
    }
}

pub fn eval_obj_double_equal(l_obj: &Object, r_obj: &Object, vm: &mut VM) -> Data {
    match &l_obj.core {
        CoreObject::STRING(l_str) => {
            let r_str = r_obj.as_string();
            Data::BOOL(l_str.clone() == r_str)
        }
        CoreObject::LIST(l_list) => {
            let r_list = r_obj.as_list();
            Data::BOOL(ListObject::is_equal(l_list, &r_list, vm))
        }
        _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn eval_obj_not_equal(l_obj: &Object, r_obj: &Object, vm: &mut VM) -> Data {
    match &l_obj.core {
        CoreObject::STRING(l_str) => {
            let r_str = r_obj.as_string();
            Data::BOOL(l_str.clone() != r_str)
        }
        CoreObject::LIST(l_list) => {
            let r_list = r_obj.as_list();
            Data::BOOL(!ListObject::is_equal(l_list, &r_list, vm))
        }
        _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}
