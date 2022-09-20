use super::data::Data;
use crate::{
    error::constants::TYPE_CHECK_BUG_ERROR_MSG,
    lexer::token::{BinaryOperatorKind, UnaryOperatorKind},
};

pub fn binary_op(data_1: Data, data_2: Data, op_index: BinaryOperatorKind) -> Data {
    match op_index {
        BinaryOperatorKind::Add => todo!(),
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
    match data {
        Data::BOOL(val) => Data::BOOL(!val),
        _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
    }
}
