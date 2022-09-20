use super::{data::Data, object::operators::eval_obj_double_equal, vm::VM};
use crate::{
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
    l_data: &Data,
    r_data: &Data,
    op_kind: BinaryOperatorKind,
    vm: &mut VM,
) -> Data {
    match op_kind {
        BinaryOperatorKind::Add => eval_add(l_data, r_data, vm),
        BinaryOperatorKind::Subtract => eval_subtract(l_data, r_data, vm),
        BinaryOperatorKind::Multiply => eval_multiply(l_data, r_data, vm),
        BinaryOperatorKind::Divide => eval_divide(l_data, r_data, vm),
        BinaryOperatorKind::NotEqual => eval_not_equal(l_data, r_data, vm),
        BinaryOperatorKind::DoubleEqual => eval_double_equal(l_data, r_data, vm),
        BinaryOperatorKind::Greater => eval_greater(l_data, r_data, vm),
        BinaryOperatorKind::Less => eval_less(l_data, r_data, vm),
        BinaryOperatorKind::GreaterEqual => eval_greater_equal(l_data, r_data, vm),
        BinaryOperatorKind::LessEqual => eval_less_equal(l_data, r_data, vm),
        BinaryOperatorKind::And => eval_and(l_data, r_data, vm),
        BinaryOperatorKind::Or => eval_or(l_data, r_data, vm),
    }
}

pub fn eval_add(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_subtract(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_multiply(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_divide(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_double_equal(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    let is_eq = match l_data {
        Data::INT(l_val) => match r_data {
            Data::INT(r_val) => *l_val == *r_val,
            Data::FLOAT(r_val) => *l_val as f64 == *r_val,
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::FLOAT(l_val) => match r_data {
            Data::INT(r_val) => *l_val == *r_val as f64,
            Data::FLOAT(r_val) => *l_val == *r_val,
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::BOOL(l_val) => match r_data {
            Data::BOOL(r_val) => *l_val == *r_val,
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
        Data::OBJ(l_obj) => match r_data {
            Data::OBJ(r_obj) => return eval_obj_double_equal(l_obj, r_obj, vm),
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
        },
    };
    Data::BOOL(is_eq)
}

pub fn eval_not_equal(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    Data::BOOL(!eval_double_equal(l_data, r_data, vm).as_bool())
}

pub fn eval_greater(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_greater_equal(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_less(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_less_equal(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_and(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_or(l_data: &Data, r_data: &Data, vm: &mut VM) -> Data {
    todo!()
}
