use super::builtin::ARRAY_BUILTIN_METHODS;
use crate::lexer::token::BinaryOperatorKind;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::core::ConcretizationContext;
use crate::scope::function::CallableData;
use crate::scope::interfaces::InterfaceBounds;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::{AbstractNonStructTypes, OperatorCompatiblity};
use crate::{
    constants::common::BOOL,
    types::core::{AbstractType, CoreType, Type},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Array {
    pub element_type: Type,
}

impl Array {
    pub fn new(element_type: Type) -> Array {
        Array { element_type }
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
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Array(array_data) => self.element_type.is_eq(&array_data.element_type),
            CoreType::Any => true,
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        Type::new_with_array(self.element_type.concretize(context))
    }

    fn is_type_bounded_by_interfaces(&self, interface_bounds: &InterfaceBounds) -> bool {
        // TODO - add checks for interfaces which `Array` would implement like `Iterator`, `Index`
        interface_bounds.len() == 0
    }

    fn has_generics(&self) -> bool {
        self.element_type.has_generics()
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
                    return Some(Type::new_with_array(sub_type.clone()));
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

impl AbstractNonStructTypes for Array {
    fn get_concrete_types(&self) -> Vec<Type> {
        vec![self.element_type.clone()]
    }

    fn get_builtin_methods(&self) -> &'static HashMap<&'static str, CallableData> {
        ARRAY_BUILTIN_METHODS.with(|use_default| *use_default)
    }
}
