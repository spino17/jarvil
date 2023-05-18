use super::core::OperatorCompatiblity;
use crate::types::core::{AbstractType, CoreType, Type};

#[derive(Debug)]
pub struct HashMap {
    pub key_type: Type,
    pub value_type: Type,
}

impl HashMap {
    pub fn new(key_type: &Type, value_type: &Type) -> HashMap {
        HashMap {
            key_type: key_type.clone(),
            value_type: value_type.clone(),
        }
    }
}

impl AbstractType for HashMap {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::HASHMAP(hashmap_data) => {
                self.key_type.is_eq(&hashmap_data.key_type)
                    && self.value_type.is_eq(&hashmap_data.value_type)
            }
            _ => false,
        }
    }
}

impl ToString for HashMap {
    fn to_string(&self) -> String {
        format!(
            "{{{} : {}}}",
            self.key_type.to_string(),
            self.value_type.to_string()
        )
    }
}

impl OperatorCompatiblity for HashMap {
    fn check_add(&self, other: &Type) -> Option<Type> {
        None
    }

    fn check_subtract(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_multiply(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_divide(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_double_equal(&self, other: &Type) -> Option<Type> {
        None
    }

    fn check_greater(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_less(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_and(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type) -> Option<Type> {
        None
    }
}
