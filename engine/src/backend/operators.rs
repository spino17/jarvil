use super::{data::Data, vm::VM};
use crate::{
    error::constants::TYPE_CHECK_BUG_ERROR_MSG,
    lexer::token::{BinaryOperatorKind, UnaryOperatorKind},
};

pub fn eval_binary_op(
    l_data: Data,
    r_data: Data,
    op_index: BinaryOperatorKind,
    vm: &mut VM,
) -> Data {
    match op_index {
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

pub fn eval_add(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_subtract(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_multiply(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_divide(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_double_equal(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_not_equal(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_greater(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_greater_equal(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_less(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_less_equal(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_and(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn eval_or(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

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
