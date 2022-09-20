use super::core::Type;
use crate::{
    constants::common::{BOOL, FLOAT, INT, STRING},
    lexer::token::BinaryOperatorKind,
};

pub fn check_operator(l_type: &Type, r_type: &Type, op_kind: &BinaryOperatorKind) -> Option<Type> {
    match op_kind {
        BinaryOperatorKind::Add => check_add(l_type, r_type),
        BinaryOperatorKind::Subtract => check_subtract(l_type, r_type),
        BinaryOperatorKind::Multiply => check_multiply(l_type, r_type),
        BinaryOperatorKind::Divide => check_divide(l_type, r_type),
        BinaryOperatorKind::Less => check_less(l_type, r_type),
        BinaryOperatorKind::LessEqual => check_less_equal(l_type, r_type),
        BinaryOperatorKind::Greater => check_greater(l_type, r_type),
        BinaryOperatorKind::GreaterEqual => check_greater_equal(l_type, r_type),
        BinaryOperatorKind::DoubleEqual => check_double_equal(l_type, r_type),
        BinaryOperatorKind::NotEqual => check_not_equal(l_type, r_type),
        BinaryOperatorKind::And => check_and(l_type, r_type),
        BinaryOperatorKind::Or => check_or(l_type, r_type),
    }
}

fn check_add(l_type: &Type, r_type: &Type) -> Option<Type> {
    if l_type.is_numeric() && r_type.is_numeric() {
        if l_type.is_float() || r_type.is_float() {
            return Some(Type::new_with_atomic(FLOAT));
        } else {
            return Some(Type::new_with_atomic(INT));
        }
    } else if l_type.is_string() && r_type.is_string() {
        return Some(Type::new_with_atomic(STRING));
    } else {
        // TODO - check whether l_type and r_type is equal and implement `Add` interface
    }
    None
}

fn check_subtract(l_type: &Type, r_type: &Type) -> Option<Type> {
    // TODO - check if type is numeric => int - float = float, int - int = ints
    if l_type.is_numeric() && r_type.is_numeric() {
        if l_type.is_float() || r_type.is_float() {
            return Some(Type::new_with_atomic(FLOAT));
        } else {
            return Some(Type::new_with_atomic(INT));
        }
    } else {
        // TODO - check whether l_type and r_type is equal and implement `Subtract` interface
    }
    None
}

fn check_multiply(l_type: &Type, r_type: &Type) -> Option<Type> {
    if l_type.is_numeric() && r_type.is_numeric() {
        if l_type.is_float() || r_type.is_float() {
            return Some(Type::new_with_atomic(FLOAT));
        } else {
            return Some(Type::new_with_atomic(INT));
        }
    } else {
        // TODO - check whether l_type and r_type is equal and implement `Multiply` interface
    }
    None
}

fn check_divide(l_type: &Type, r_type: &Type) -> Option<Type> {
    if l_type.is_numeric() && r_type.is_numeric() {
        return Some(Type::new_with_atomic(FLOAT));
    } else {
        // TODO - check whether l_type and r_type is equal and implement `Divide` interface
    }
    None
}

fn check_and(l_type: &Type, r_type: &Type) -> Option<Type> {
    if l_type.is_bool() && r_type.is_bool() {
        return Some(Type::new_with_atomic(BOOL));
    } else {
        return None;
    }
}

fn check_or(l_type: &Type, r_type: &Type) -> Option<Type> {
    if l_type.is_bool() && r_type.is_bool() {
        return Some(Type::new_with_atomic(BOOL));
    } else {
        return None;
    }
}

fn check_greater(l_type: &Type, r_type: &Type) -> Option<Type> {
    let is_ok = if l_type.is_numeric() && r_type.is_numeric() {
        true
    } else {
        // TODO - check whether l_type and r_type is equal and implement `Greater` interface
        false
    };
    if is_ok {
        return Some(Type::new_with_atomic(BOOL));
    }
    None
}

fn check_less(l_type: &Type, r_type: &Type) -> Option<Type> {
    let is_ok = if l_type.is_numeric() && r_type.is_numeric() {
        true
    } else {
        // TODO - check whether l_type and r_type is equal and implement `Less` interface
        false
    };
    if is_ok {
        return Some(Type::new_with_atomic(BOOL));
    }
    None
}

fn check_greater_equal(l_type: &Type, r_type: &Type) -> Option<Type> {
    let is_ok = if l_type.is_numeric() && r_type.is_numeric() {
        true
    } else {
        // TODO - check whether l_type and r_type is equal and implement `GreaterEqual` interface
        false
    };
    if is_ok {
        return Some(Type::new_with_atomic(BOOL));
    }
    None
}

fn check_less_equal(l_type: &Type, r_type: &Type) -> Option<Type> {
    let is_ok = if l_type.is_numeric() && r_type.is_numeric() {
        true
    } else {
        // TODO - check whether l_type and r_type is equal and implement `LessEqual` interface
        false
    };
    if is_ok {
        return Some(Type::new_with_atomic(BOOL));
    }
    None
}

fn check_double_equal(l_type: &Type, r_type: &Type) -> Option<Type> {
    let is_ok = if l_type.is_numeric() && r_type.is_numeric() {
        true
    } else if l_type.is_string() && r_type.is_string() {
        true
    } else {
        // TODO - check whether l_type and r_type is equal and implement `DoubleEqual` interface
        false
    };
    if is_ok {
        return Some(Type::new_with_atomic(BOOL));
    }
    None
}

fn check_not_equal(l_type: &Type, r_type: &Type) -> Option<Type> {
    let is_ok = if l_type.is_numeric() && r_type.is_numeric() {
        true
    } else if l_type.is_string() && r_type.is_string() {
        true
    } else {
        // TODO - check whether l_type and r_type is equal and implement `DoubleEqual` interface
        false
    };
    if is_ok {
        return Some(Type::new_with_atomic(BOOL));
    }
    None
}
