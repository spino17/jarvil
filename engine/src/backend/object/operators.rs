use super::{
    core::{CoreObject, Object, ObjectTracker},
    list::ListObject,
};
use crate::{
    backend::{data::Data, vm::VM},
    error::constants::TYPE_CHECK_BUG_ERROR_MSG,
    lexer::token::BinaryOperatorKind,
};

pub fn eval_obj_binary_op(
    l_obj: Object,
    r_obj: Object,
    op_kind: BinaryOperatorKind,
    tracker: &ObjectTracker,
) -> Data {
    match op_kind {
        BinaryOperatorKind::Add => eval_obj_add(l_obj, r_obj, tracker),
        BinaryOperatorKind::Subtract => eval_obj_subtract(l_obj, r_obj, tracker),
        BinaryOperatorKind::Multiply => eval_obj_multiply(l_obj, r_obj, tracker),
        BinaryOperatorKind::Divide => eval_obj_divide(l_obj, r_obj, tracker),
        BinaryOperatorKind::Greater => eval_obj_greater(l_obj, r_obj, tracker),
        BinaryOperatorKind::Less => eval_obj_less(l_obj, r_obj, tracker),
        BinaryOperatorKind::GreaterEqual => eval_obj_greater_equal(l_obj, r_obj, tracker),
        BinaryOperatorKind::LessEqual => eval_obj_less_equal(l_obj, r_obj, tracker),
        BinaryOperatorKind::And => eval_obj_and(l_obj, r_obj, tracker),
        BinaryOperatorKind::Or => eval_obj_or(l_obj, r_obj, tracker),
        BinaryOperatorKind::NotEqual => eval_obj_not_equal(l_obj, r_obj, tracker),
        BinaryOperatorKind::DoubleEqual => eval_obj_double_equal(l_obj, r_obj, tracker),
    }
}

pub fn eval_obj_add(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    match l_obj.core {
        CoreObject::STRING(l_str) => {
            let r_str = r_obj.as_string();
            Data::OBJ(Object::new_with_string(l_str + r_str, tracker))
        }
        CoreObject::LIST(l_list) => {
            let r_list = r_obj.as_list();
            Data::OBJ(Object::new_with_list(l_list + r_list, tracker))
        }
        CoreObject::FUNCTION(_) => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn eval_obj_subtract(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_multiply(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_divide(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_greater(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_greater_equal(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_less(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_less_equal(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_and(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_or(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    todo!()
}

pub fn eval_obj_double_equal(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    match l_obj.core {
        CoreObject::STRING(l_str) => {
            let r_str = r_obj.as_string();
            Data::BOOL(l_str == r_str)
        }
        CoreObject::LIST(l_list) => {
            let r_list = r_obj.as_list();
            Data::BOOL(ListObject::is_equal(&l_list, &r_list, tracker))
        }
        _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn eval_obj_not_equal(l_obj: Object, r_obj: Object, tracker: &ObjectTracker) -> Data {
    match l_obj.core {
        CoreObject::STRING(l_str) => {
            let r_str = r_obj.as_string();
            Data::BOOL(l_str != r_str)
        }
        CoreObject::LIST(l_list) => {
            let r_list = r_obj.as_list();
            Data::BOOL(!ListObject::is_equal(&l_list, &r_list, tracker))
        }
        CoreObject::FUNCTION(_) => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}
