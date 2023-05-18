use crate::{ast::ast::BlockNode, types::core::Type};
use std::{rc::Rc, vec};

#[derive(Debug)]
pub struct FunctionData {
    pub params: Rc<Vec<(Rc<String>, Type)>>,
    pub return_type: Type,
}

impl FunctionData {
    pub fn new(params: Vec<(Rc<String>, Type)>, return_type: Type) -> Self {
        FunctionData {
            params: Rc::new(params),
            return_type,
        }
    }

    pub fn set_data(&mut self, params: Vec<(Rc<String>, Type)>, return_type: Type) {
        self.params = Rc::new(params);
        self.return_type = return_type;
    }
}

impl Default for FunctionData {
    fn default() -> Self {
        FunctionData {
            params: Rc::new(vec![]),
            return_type: Type::new_with_void(),
        }
    }
}

impl Clone for FunctionData {
    fn clone(&self) -> Self {
        FunctionData {
            params: self.params.clone(),
            return_type: self.return_type.clone(),
        }
    }
}
