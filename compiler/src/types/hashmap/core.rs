use super::builtin::HASHMAP_BUILTIN_METHODS;
use crate::{
    constants::common::BOOL,
    lexer::token::BinaryOperatorKind,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::core::ConcretizationContext, function::CallableData, interfaces::InterfaceBounds,
        types::generic_type::GenericTypeDeclarationPlaceCategory,
    },
    types::core::{AbstractNonStructTypes, AbstractType, CoreType, OperatorCompatiblity, Type},
};
use std::{collections::HashMap as StdHashMap, rc::Rc};

#[derive(Debug, Clone)]
pub struct HashMap {
    pub key_type: Type,
    pub value_type: Type,
}

impl HashMap {
    pub fn new(key_type: Type, value_type: Type) -> HashMap {
        HashMap {
            key_type,
            value_type,
        }
    }
}

impl AbstractType for HashMap {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::HashMap(hashmap_data) => {
                self.key_type.is_eq(&hashmap_data.key_type)
                    && self.value_type.is_eq(&hashmap_data.value_type)
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(&self, other_ty: &Type, context: &ConcretizationContext) -> bool {
        match other_ty.0.as_ref() {
            CoreType::HashMap(hashmap_data) => {
                self.key_type
                    .is_structurally_eq(&hashmap_data.key_type, context)
                    && self
                        .value_type
                        .is_structurally_eq(&hashmap_data.value_type, context)
            }
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        let concrete_key_ty = if self.key_type.has_generics() {
            self.key_type.concretize(context)
        } else {
            self.key_type.clone()
        };
        let concrete_value_ty = if self.value_type.has_generics() {
            self.value_type.concretize(context)
        } else {
            self.value_type.clone()
        };
        return Type::new_with_hashmap(concrete_key_ty, concrete_value_ty);
    }

    fn is_type_bounded_by_interfaces(&self, interface_bounds: &InterfaceBounds) -> bool {
        // TODO - add checks for interfaces which `HashMap` would implement like `Iterator`, `Index`
        interface_bounds.len() == 0
    }

    fn has_generics(&self) -> bool {
        self.key_type.has_generics() || self.value_type.has_generics()
    }

    fn try_infer_type(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&Vec<Type>>,
        num_inferred_types: &mut usize,
        has_generics: &mut bool,
        inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        match received_ty.0.as_ref() {
            CoreType::HashMap(hashmap_ty) => {
                if self.key_type.has_generics() {
                    let _ = self.key_type.try_infer_type(
                        &hashmap_ty.key_type,
                        inferred_concrete_types,
                        global_concrete_types,
                        num_inferred_types,
                        has_generics,
                        inference_category,
                    )?;
                } else {
                    if !self.key_type.is_eq(&hashmap_ty.key_type) {
                        return Err(());
                    }
                }
                if self.value_type.has_generics() {
                    let _ = self.value_type.try_infer_type(
                        &hashmap_ty.value_type,
                        inferred_concrete_types,
                        global_concrete_types,
                        num_inferred_types,
                        has_generics,
                        inference_category,
                    )?;
                } else {
                    if !self.value_type.is_eq(&hashmap_ty.value_type) {
                        return Err(());
                    }
                }
                Ok(())
            }
            _ => Err(()),
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

impl AbstractNonStructTypes for HashMap {
    fn get_concrete_types(&self) -> Vec<Type> {
        return vec![self.key_type.clone(), self.value_type.clone()];
    }

    fn get_builtin_methods(&self) -> Rc<StdHashMap<&'static str, CallableData>> {
        HASHMAP_BUILTIN_METHODS.with(|use_default| use_default.clone())
    }
}
