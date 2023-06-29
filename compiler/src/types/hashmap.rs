use super::core::OperatorCompatiblity;
use crate::{
    constants::common::BOOL,
    lexer::token::BinaryOperatorKind,
    types::core::{AbstractType, CoreType, Type},
};

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
            CoreType::HashMap(hashmap_data) => {
                self.key_type.is_eq(&hashmap_data.key_type)
                    && self.value_type.is_eq(&hashmap_data.value_type)
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn stringify(&self) -> String {
        todo!()
    }

    fn concretize(&self) -> Vec<Type> {
        let key_ty = &self.key_type;
        let value_ty = &self.value_type;
        let key_concrete_types = key_ty.concretize();
        let value_concrete_types = value_ty.concretize();
        let mut hashmap_concrete_types = vec![];
        // TODO - currently key and value are separately concretized without
        // taking into accound any constraints that might exist between them.
        // for example the hashmap might be `{T: T}` which let's say T takes
        // concrete values from set [int, str, bool] then the concrete hashmap types
        // should be [{int: int}, {str: str}, {bool: bool}] but according to below logic
        // it will produce [{int: int}, {int: str}, {int: bool}, {str: int}, {str: str} ...]
        // Size of this vector is directly propotional to the code segments generated in Python code.
        // For now it is fine as mostly generic types are very rarely used in calling of the constructs.
        for key_ty in &key_concrete_types {
            for value_ty in &value_concrete_types {
                hashmap_concrete_types.push(Type::new_with_hashmap(key_ty, value_ty));
            }
        }
        return hashmap_concrete_types;
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
    fn check_add(&self, _other: &Type) -> Option<Type> {
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
        match other.0.as_ref() {
            CoreType::HashMap(other_hashmap) => {
                if self
                    .key_type
                    .check_operator(&other_hashmap.key_type, &BinaryOperatorKind::DoubleEqual)
                    .is_some()
                    && self
                        .value_type
                        .check_operator(&other_hashmap.value_type, &BinaryOperatorKind::DoubleEqual)
                        .is_some()
                {
                    return Some(Type::new_with_atomic(BOOL));
                }
                return None;
            }
            _ => return None,
        }
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
