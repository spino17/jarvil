use crate::types::core::Type;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct FunctionData {
    pub params: Rc<Vec<(String, Type)>>,
    pub return_type: Rc<Option<Type>>,
}
