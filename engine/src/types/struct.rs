use std::rc::Rc;
use crate::types::core::{AbstractType, Type, CoreType};

#[derive(Debug)]
pub struct Struct {
    name: Rc<String>, 
}

impl AbstractType for Struct {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::STRUCT(struct_data) => {
                self.name.eq(&struct_data.name)
            },
            _ => false
        }
    }

    fn to_string(&self) -> Rc<String> {
        self.name.clone()
    }
}