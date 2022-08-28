use crate::types::core::Type;
use std::{rc::Rc, vec};

#[derive(Debug)]
pub struct FunctionData {
    pub params: Rc<Vec<(String, Type)>>,
    pub return_type: Option<Type>,
}
impl Default for FunctionData {
    fn default() -> Self {
        FunctionData{
            params: Rc::new(vec![]),
            return_type: None,
        }
    }
}
impl Clone for FunctionData {
    fn clone(&self) -> Self {
        FunctionData{
            params: self.params.clone(),
            return_type: match &self.return_type {
                Some(return_type) => Some(return_type.clone()),
                None => None,
            }
        }
    }
}