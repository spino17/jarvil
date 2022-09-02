use super::core::Type;
use crate::lexer::token::BinaryOperatorKind;

pub trait Operator {
    fn check_operation(&self, l_type: &Type, r_type: &Type) -> Option<Type>;
    fn kind(&self) -> BinaryOperatorKind;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::PLUS;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::MINUS;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::MULTIPLY;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::DIVIDE;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::AND;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::OR;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::GREATER;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::LESS;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::GREATER_EQUAL;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::LESS_EQUAL;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::DOUBLE_EQUAL;
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
        todo!()
    }

    fn kind(&self) -> BinaryOperatorKind {
        return BinaryOperatorKind::NOT_EQUAL;
    }
}
