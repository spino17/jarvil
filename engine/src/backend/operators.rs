use super::{data::Data, vm::VM};
use crate::{
    error::constants::TYPE_CHECK_BUG_ERROR_MSG,
    lexer::token::{BinaryOperatorKind, UnaryOperatorKind},
};

pub fn binary_op(l_data: Data, r_data: Data, op_index: BinaryOperatorKind, vm: &mut VM) -> Data {
    match op_index {
        BinaryOperatorKind::Add => binary_add(l_data, r_data, vm),
        BinaryOperatorKind::Subtract => binary_subtract(l_data, r_data, vm),
        BinaryOperatorKind::Multiply => binary_multiply(l_data, r_data, vm),
        BinaryOperatorKind::Divide => binary_divide(l_data, r_data, vm),
        BinaryOperatorKind::NotEqual => binary_not_equal(l_data, r_data, vm),
        BinaryOperatorKind::DoubleEqual => binary_double_equal(l_data, r_data, vm),
        BinaryOperatorKind::Greater => binary_greater(l_data, r_data, vm),
        BinaryOperatorKind::Less => binary_less(l_data, r_data, vm),
        BinaryOperatorKind::GreaterEqual => binary_greater_equal(l_data, r_data, vm),
        BinaryOperatorKind::LessEqual => binary_less_equal(l_data, r_data, vm),
        BinaryOperatorKind::And => binary_and(l_data, r_data, vm),
        BinaryOperatorKind::Or => binary_or(l_data, r_data, vm),
    }
}

pub fn binary_add(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_subtract(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_multiply(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_divide(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_double_equal(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_not_equal(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_greater(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_greater_equal(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_less(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_less_equal(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_and(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn binary_or(l_data: Data, r_data: Data, vm: &mut VM) -> Data {
    todo!()
}

pub fn unary_op(data: Data, op_index: UnaryOperatorKind) -> Data {
    match op_index {
        UnaryOperatorKind::Plus => unary_plus(data),
        UnaryOperatorKind::Minus => unary_minus(data),
        UnaryOperatorKind::Not => unary_not(data),
    }
}

pub fn unary_plus(data: Data) -> Data {
    match data {
        Data::INT(val) => Data::INT(val),
        Data::FLOAT(val) => Data::FLOAT(val),
        _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn unary_minus(data: Data) -> Data {
    match data {
        Data::INT(val) => Data::INT(-val),
        Data::FLOAT(val) => Data::FLOAT(-val),
        _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}

pub fn unary_not(data: Data) -> Data {
    Data::BOOL(!data.as_bool())
}
