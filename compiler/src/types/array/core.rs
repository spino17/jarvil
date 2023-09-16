use super::builtin::ARRAY_BUILTIN_METHODS;
use crate::lexer::token::BinaryOperatorKind;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::core::ConcretizationContext;
use crate::scope::function::CallableData;
use crate::scope::handler::SymbolDataRegistryTable;
use crate::scope::interfaces::InterfaceBounds;
use crate::scope::types::core::UserDefinedTypeData;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::{AbstractNonStructTypes, OperatorCompatiblity};
use crate::{
    constants::common::BOOL,
    types::core::{AbstractType, CoreType, Type},
};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
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
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Array(other_array) => {
                if self
                    .element_type
                    .check_operator(&other_array.element_type, operator_kind, registry)
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
    fn is_eq(
        &self,
        other_ty: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Array(array_data) => {
                self.element_type.is_eq(&array_data.element_type, registry)
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
        context: &ConcretizationContext,
    ) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Array(array_data) => {
                self.element_type
                    .is_structurally_eq(&array_data.element_type, registry, context)
            }
            _ => false,
        }
    }

    fn concretize(
        &self,
        context: &ConcretizationContext,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Type {
        Type::new_with_array(self.element_type.concretize(context, registry))
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> bool {
        // TODO - add checks for interfaces which `Array` would implement like `Iterator`, `Index`
        interface_bounds.len() == 0
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&Vec<Type>>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Result<(), ()> {
        match received_ty.0.as_ref() {
            CoreType::Array(array_ty) => self.element_type.try_infer_type_or_check_equivalence(
                &array_ty.element_type,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                registry,
            ),
            _ => Err(()),
        }
    }
}

impl ToString for Array {
    fn to_string(&self) -> String {
        format!("[{}]", self.element_type.to_string())
    }
}

impl OperatorCompatiblity for Array {
    fn check_add(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Array(array) => {
                let sub_type = &array.element_type;
                if self.element_type.is_eq(sub_type, registry) {
                    return Some(Type::new_with_array(sub_type.clone()));
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_subtract(
        &self,
        _other: &Type,
        _registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        None
    }

    fn check_multiply(
        &self,
        _other: &Type,
        _registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        // TODO - add case for integer to enable syntax like `[1, 2] * 2`
        None
    }

    fn check_divide(
        &self,
        _other: &Type,
        _registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        None
    }

    fn check_double_equal(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        self.check_operator_for_array(other, &BinaryOperatorKind::DoubleEqual, registry)
    }

    fn check_greater(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        self.check_operator_for_array(other, &BinaryOperatorKind::Greater, registry)
    }

    fn check_less(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        self.check_operator_for_array(other, &BinaryOperatorKind::Less, registry)
    }

    fn check_and(
        &self,
        _other: &Type,
        _registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        None
    }

    fn check_or(
        &self,
        _other: &Type,
        _registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        None
    }
}

impl AbstractNonStructTypes for Array {
    fn get_concrete_types(&self) -> Vec<Type> {
        vec![self.element_type.clone()]
    }

    fn get_builtin_methods(&self) -> Rc<HashMap<&'static str, CallableData>> {
        ARRAY_BUILTIN_METHODS.with(|use_default| use_default.clone())
    }
}
