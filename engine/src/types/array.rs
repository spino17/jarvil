use crate::types::core::{AbstractType, CoreType, Type};
use std::{rc::Rc, fmt::Formatter};

#[derive(Debug)]
pub struct Array {
    pub size: usize,
    pub element_type: Type,
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
impl std::fmt::Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f, 
            "[{}; {}]",
            self.element_type.to_string(),
            self.size
        )
    }
}
