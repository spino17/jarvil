use super::core::{CoreObject, Object};
use crate::{backend::vm::VM, lexer::token::BinaryOperatorKind};

pub fn eval_obj_binary_op(
    l_obj: Object,
    r_obj: Object,
    op_kind: BinaryOperatorKind,
    vm: &mut VM,
) -> Object {
    match op_kind {
        BinaryOperatorKind::Add => eval_obj_add(l_obj, r_obj, vm),
        BinaryOperatorKind::Subtract => todo!(),
        BinaryOperatorKind::Multiply => todo!(),
        BinaryOperatorKind::Divide => todo!(),
        BinaryOperatorKind::NotEqual => todo!(),
        BinaryOperatorKind::DoubleEqual => todo!(),
        BinaryOperatorKind::Greater => todo!(),
        BinaryOperatorKind::Less => todo!(),
        BinaryOperatorKind::GreaterEqual => todo!(),
        BinaryOperatorKind::LessEqual => todo!(),
        BinaryOperatorKind::And => todo!(),
        BinaryOperatorKind::Or => todo!(),
    }
}

pub fn eval_obj_add(l_obj: Object, r_obj: Object, vm: &mut VM) -> Object {
    match l_obj.core {
        CoreObject::STRING(l_str) => {
            let r_str = r_obj.as_string();
            return Object::new_with_string(l_str + r_str, vm);
        }
        CoreObject::LIST(l_list) => {
            let r_list = r_obj.as_list();
            return Object::new_with_list(l_list + r_list, vm);
        }
    }
}
