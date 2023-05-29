use super::core::OperatorCompatiblity;
use crate::lexer::token::BinaryOperatorKind;
use crate::{
    constants::common::BOOL,
    types::core::{AbstractType, CoreType, Type},
};

#[derive(Debug)]
pub struct Array {
    pub element_type: Type,
}

impl Array {
    pub fn new(element_type: &Type) -> Array {
        Array {
            element_type: element_type.clone(),
        }
    }

    fn check_operator_for_array(
        &self,
        other: &Type,
        operator_kind: &BinaryOperatorKind,
    ) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Array(other_array) => {
                if self
                    .element_type
                    .check_operator(&other_array.element_type, operator_kind)
                    .is_some()
                {
                    return Some(Type::new_with_atomic(BOOL));
                }
                return None;
            }
            _ => None,
        }
    }
}

impl AbstractType for Array {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::Array(array_data) => self.element_type.is_eq(&array_data.element_type),
            CoreType::Any => true,
            _ => false,
        }
    }
}

impl ToString for Array {
    fn to_string(&self) -> String {
        format!("[{}]", self.element_type.to_string())
    }
}

impl OperatorCompatiblity for Array {
    fn check_add(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Array(array) => {
                let sub_type = &array.element_type;
                if self.element_type.is_eq(sub_type) {
                    return Some(Type::new_with_array(sub_type));
                } else {
                    return None;
                }
            }
            _ => None,
        }
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
        self.check_operator_for_array(other, &BinaryOperatorKind::DoubleEqual)
    }

    fn check_greater(&self, other: &Type) -> Option<Type> {
        self.check_operator_for_array(other, &BinaryOperatorKind::Greater)
    }

    fn check_less(&self, other: &Type) -> Option<Type> {
        self.check_operator_for_array(other, &BinaryOperatorKind::Less)
    }

    fn check_and(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type) -> Option<Type> {
        None
    }
}
