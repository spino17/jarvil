use crate::types::core::{AbstractType, CoreType, Type};
use std::{fmt::Formatter, rc::Rc};

#[derive(Debug)]
pub struct Array {
    pub size: usize,
    pub element_type: Type,
}

impl Array {
    pub fn new(element_type: &Type, size: usize) -> Array {
        Array {
            element_type: element_type.clone(),
            size,
        }
    }
}

impl AbstractType for Array {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::ARRAY(array_data) => {
                if array_data.size != self.size {
                    false
                } else {
                    self.element_type.is_eq(&array_data.element_type)
                }
            }
            _ => false,
        }
    }
}

impl ToString for Array {
    fn to_string(&self) -> String {
        format!("[{}; {}]", self.element_type.to_string(), self.size)
    }
}
