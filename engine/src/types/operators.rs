use crate::lexer::token::BinaryOperatorKind;

use super::core::Type;

pub trait Operator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type>;
    fn kind(&self) -> BinaryOperatorKind;
}

pub fn operator_factory(kind: &BinaryOperatorKind) -> Box<dyn Operator> {
    match kind {
        BinaryOperatorKind::PLUS => Box::new(AddOperator::new()),
        _ => todo!(),
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::PLUS;
    }
}
