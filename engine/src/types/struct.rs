use std::rc::Rc;
use crate::types::core::{TypeCheck, Type, CoreType};

#[derive(Debug)]
pub struct Struct {
    pub name: Rc<String>,
}

impl TypeCheck for Struct {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::STRUCT(struct_data) => {
                self.name.eq(&struct_data.name)
            },
            _ => false
        }
    }
}