use super::core::Type;
use crate::{
    constants::common::{BOOL, FLOAT, INT, STRING},
    lexer::token::BinaryOperatorKind,
};

pub trait Operator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type>;
}

pub fn operator_factory(kind: &BinaryOperatorKind) -> Box<dyn Operator> {
    match kind {
        BinaryOperatorKind::PLUS => Box::new(AddOperator::new()),
        BinaryOperatorKind::MINUS => Box::new(SubtractOperator::new()),
        BinaryOperatorKind::MULTIPLY => Box::new(MultiplyOperator::new()),
        BinaryOperatorKind::DIVIDE => Box::new(DivideOperator::new()),
        BinaryOperatorKind::LESS => Box::new(LessOperator::new()),
        BinaryOperatorKind::LESS_EQUAL => Box::new(LessEqualOperator::new()),
        BinaryOperatorKind::GREATER => Box::new(GreaterOperator::new()),
        BinaryOperatorKind::GREATER_EQUAL => Box::new(GreaterEqualOperator::new()),
        BinaryOperatorKind::DOUBLE_EQUAL => Box::new(DoubleEqualOperator::new()),
        BinaryOperatorKind::NOT_EQUAL => Box::new(NotEqualOperator::new()),
        BinaryOperatorKind::AND => Box::new(AndOperator::new()),
        BinaryOperatorKind::OR => Box::new(OrOperator::new()),
    }
}

struct CoreOperator {
    kind: BinaryOperatorKind,
    symbol: &'static str,
    interface: &'static str,
}

impl CoreOperator {
    pub fn new(kind: BinaryOperatorKind, symbol: &'static str, interface: &'static str) -> Self {
        CoreOperator {
            kind,
            symbol,
            interface,
        }
    }
}

pub struct AddOperator(CoreOperator);

impl AddOperator {
    pub fn new() -> Self {
        AddOperator(CoreOperator::new(BinaryOperatorKind::PLUS, "+", "Add"))
    }
}

impl Operator for AddOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct SubtractOperator(CoreOperator);

impl SubtractOperator {
    pub fn new() -> Self {
        SubtractOperator(CoreOperator::new(
            BinaryOperatorKind::MINUS,
            "-",
            "Subtract",
        ))
    }
}

impl Operator for SubtractOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct MultiplyOperator(CoreOperator);

impl MultiplyOperator {
    pub fn new() -> Self {
        MultiplyOperator(CoreOperator::new(
            BinaryOperatorKind::MULTIPLY,
            "*",
            "Multiply",
        ))
    }
}

impl Operator for MultiplyOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct DivideOperator(CoreOperator);

impl DivideOperator {
    pub fn new() -> Self {
        DivideOperator(CoreOperator::new(BinaryOperatorKind::DIVIDE, "/", "Divide"))
    }
}

impl Operator for DivideOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct AndOperator(CoreOperator);

impl AndOperator {
    pub fn new() -> Self {
        AndOperator(CoreOperator::new(BinaryOperatorKind::AND, "and", "And"))
    }
}

impl Operator for AndOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
        if l_type.is_bool() && r_type.is_bool() {
            return Some(Type::new_with_atomic(BOOL));
        } else {
            return None;
        }
    }
}

pub struct OrOperator(CoreOperator);

impl OrOperator {
    pub fn new() -> Self {
        OrOperator(CoreOperator::new(BinaryOperatorKind::OR, "or", "Or"))
    }
}

impl Operator for OrOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
        if l_type.is_bool() && r_type.is_bool() {
            return Some(Type::new_with_atomic(BOOL));
        } else {
            return None;
        }
    }
}

pub struct GreaterOperator(CoreOperator);

impl GreaterOperator {
    pub fn new() -> Self {
        GreaterOperator(CoreOperator::new(
            BinaryOperatorKind::GREATER,
            ">",
            "Greater",
        ))
    }
}

impl Operator for GreaterOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct LessOperator(CoreOperator);

impl LessOperator {
    pub fn new() -> Self {
        LessOperator(CoreOperator::new(BinaryOperatorKind::LESS, "<", "Less"))
    }
}

impl Operator for LessOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct GreaterEqualOperator(CoreOperator);

impl GreaterEqualOperator {
    pub fn new() -> Self {
        GreaterEqualOperator(CoreOperator::new(
            BinaryOperatorKind::GREATER_EQUAL,
            ">=",
            "GreaterEqual",
        ))
    }
}

impl Operator for GreaterEqualOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct LessEqualOperator(CoreOperator);

impl LessEqualOperator {
    pub fn new() -> Self {
        LessEqualOperator(CoreOperator::new(
            BinaryOperatorKind::LESS_EQUAL,
            "<=",
            "LessEqual",
        ))
    }
}

impl Operator for LessEqualOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct DoubleEqualOperator(CoreOperator);

impl DoubleEqualOperator {
    pub fn new() -> Self {
        DoubleEqualOperator(CoreOperator::new(
            BinaryOperatorKind::DOUBLE_EQUAL,
            "==",
            "DoubleEqual",
        ))
    }
}

impl Operator for DoubleEqualOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
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
}

pub struct NotEqualOperator(CoreOperator);

impl NotEqualOperator {
    pub fn new() -> Self {
        NotEqualOperator(CoreOperator::new(
            BinaryOperatorKind::NOT_EQUAL,
            "!=",
            "NotEqual",
        ))
    }
}

impl Operator for NotEqualOperator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type> {
        let is_ok = if l_type.is_numeric() && r_type.is_numeric() {
            true
        } else if l_type.is_string() && r_type.is_string() {
            true
        } else {
            // TODO - check whether l_type and r_type is equal and implement `NotEqual` interface
            false
        };
        if is_ok {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }
}
