use std::rc::Rc;
use crate::types::core::{AbstractType, Type, CoreType};
use crate::scope::user_defined_types::StructData;

#[derive(Debug)]
pub struct Struct(StructData);

impl AbstractType for Struct {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::STRUCT(struct_data) => {
                self.0.name.eq(&struct_data.0.name)
            },
            _ => false
        }
    }

    fn to_string(&self) -> Rc<String> {
        self.0.name.clone()
    }
}