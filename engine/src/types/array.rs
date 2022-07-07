use crate::types::core::{Type, AbstractType, CoreType};
use std::rc::Rc;

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
            },
            _ => false
        }
    }

    fn to_string(&self) -> std::rc::Rc<String> {
        Rc::new(format!("[{}, {}]", AbstractType::to_string(&self.element_type), self.size))
    }
}