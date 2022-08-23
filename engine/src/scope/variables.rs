use crate::types::core::Type;

use super::function::FunctionData;

#[derive(Debug, Clone)]
pub enum VariableData { // TODO - make it into enum to accomodate Lambdas also
    EXPRESSION(ExpressionVariable),
    LAMBDA(LambdaVariable),
}

#[derive(Debug, Clone)]
pub struct ExpressionVariable {
    pub data_type: Type,
    pub is_init: bool,
}

#[derive(Debug, Clone)]
pub struct LambdaVariable {
    pub func_data: FunctionData,
    pub is_init: bool,
}
