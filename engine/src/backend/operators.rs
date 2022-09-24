use super::{
    data::Data,
    object::{
        core::ObjectTracker,
        operators::{
            eval_obj_add, eval_obj_and, eval_obj_divide, eval_obj_double_equal, eval_obj_greater,
        },
    },
    vm::VM,
};
use crate::{
    backend::object::operators::{
        eval_obj_greater_equal, eval_obj_less, eval_obj_less_equal, eval_obj_multiply, eval_obj_or,
        eval_obj_subtract,
    },
    error::constants::TYPE_CHECK_BUG_ERROR_MSG,
    lexer::token::{BinaryOperatorKind, UnaryOperatorKind},
};

pub fn eval_unary_op(data: Data, op_index: UnaryOperatorKind) -> Data {
    match op_index {
        UnaryOperatorKind::Plus => eval_plus(data),
        UnaryOperatorKind::Minus => eval_minus(data),
        UnaryOperatorKind::Not => eval_not(data),
    }
}

pub fn eval_plus(data: Data) -> Data {
    match data {
        Data::INT(val) => Data::INT(val),
        Data::FLOAT(val) => Data::FLOAT(val),
        _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn eval_minus(data: Data) -> Data {
    match data {
        Data::INT(val) => Data::INT(-val),
        Data::FLOAT(val) => Data::FLOAT(-val),
        _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn eval_not(data: Data) -> Data {
    Data::BOOL(!data.as_bool())
}

pub fn eval_binary_op(
    l_data: Data,
    r_data: Data,
    op_kind: BinaryOperatorKind,
    tracker: &ObjectTracker,
) -> Data {
    match op_kind {
        BinaryOperatorKind::Add => eval_add(l_data, r_data, tracker),
        BinaryOperatorKind::Subtract => eval_subtract(l_data, r_data, tracker),
        BinaryOperatorKind::Multiply => eval_multiply(l_data, r_data, tracker),
        BinaryOperatorKind::Divide => eval_divide(l_data, r_data, tracker),
        BinaryOperatorKind::NotEqual => eval_not_equal(l_data, r_data, tracker),
        BinaryOperatorKind::DoubleEqual => eval_double_equal(l_data, r_data, tracker),
        BinaryOperatorKind::Greater => eval_greater(l_data, r_data, tracker),
        BinaryOperatorKind::Less => eval_less(l_data, r_data, tracker),
        BinaryOperatorKind::GreaterEqual => eval_greater_equal(l_data, r_data, tracker),
        BinaryOperatorKind::LessEqual => eval_less_equal(l_data, r_data, tracker),
        BinaryOperatorKind::And => eval_and(l_data, r_data, tracker),
        BinaryOperatorKind::Or => eval_or(l_data, r_data, tracker),
    }
}

pub fn eval_add(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_arithmetic_op!(+, l_data, r_data, eval_obj_add, tracker);
}

pub fn eval_subtract(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_arithmetic_op!(-, l_data, r_data, eval_obj_subtract, tracker);
}

pub fn eval_multiply(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_arithmetic_op!(*, l_data, r_data, eval_obj_multiply, tracker);
}

pub fn eval_divide(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    match l_data {
        Data::INT(l_val) => match r_data {
            Data::INT(r_val) => return Data::FLOAT(l_val as f64 / r_val as f64),
            Data::FLOAT(r_val) => return Data::FLOAT(l_val as f64 / r_val),
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::FLOAT(l_val) => match r_data {
            Data::INT(r_val) => return Data::FLOAT(l_val / r_val as f64),
            Data::FLOAT(r_val) => return Data::FLOAT(l_val / r_val),
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::OBJ(l_obj) => match r_data {
            Data::OBJ(r_obj) => return eval_obj_divide(l_obj, r_obj, tracker),
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::BOOL(_) => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn eval_double_equal(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    let is_eq = match l_data {
        Data::INT(l_val) => match r_data {
            Data::INT(r_val) => l_val == r_val,
            Data::FLOAT(r_val) => l_val as f64 == r_val,
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::FLOAT(l_val) => match r_data {
            Data::INT(r_val) => l_val == r_val as f64,
            Data::FLOAT(r_val) => l_val == r_val,
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::BOOL(l_val) => match r_data {
            Data::BOOL(r_val) => l_val == r_val,
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::OBJ(l_obj) => match r_data {
            Data::OBJ(r_obj) => return eval_obj_double_equal(l_obj, r_obj, tracker),
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
    };
    Data::BOOL(is_eq)
}

pub fn eval_not_equal(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    Data::BOOL(!eval_double_equal(l_data, r_data, tracker).as_bool())
}

pub fn eval_greater(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_comparison_op!(>, l_data, r_data, eval_obj_greater, tracker);
}

pub fn eval_greater_equal(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_comparison_op!(>=, l_data, r_data, eval_obj_greater_equal, tracker);
}

pub fn eval_less(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_comparison_op!(<, l_data, r_data, eval_obj_less, tracker);
}

pub fn eval_less_equal(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_comparison_op!(<=, l_data, r_data, eval_obj_less_equal, tracker);
}

pub fn eval_and(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_logical_op!(&&, l_data, r_data, eval_obj_and, tracker);
}

pub fn eval_or(l_data: Data, r_data: Data, tracker: &ObjectTracker) -> Data {
    impl_eval_logical_op!(||, l_data, r_data, eval_obj_or, tracker);
}
