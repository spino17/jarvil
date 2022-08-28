use crate::types::core::Type;
use std::{rc::Rc, vec};

#[derive(Debug)]
pub struct CoreFunctionData {
    pub params: Rc<Vec<(String, Type)>>,
    pub return_type: Option<Type>,
}
impl Default for CoreFunctionData {
    fn default() -> Self {
        CoreFunctionData{
            params: Rc::new(vec![]),
            return_type: None,
        }
    }
}
impl Clone for CoreFunctionData {
    fn clone(&self) -> Self {
        CoreFunctionData{
            params: self.params.clone(),
            return_type: match &self.return_type {
                Some(return_type) => Some(return_type.clone()),
                None => None,
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionData(pub Option<CoreFunctionData>);