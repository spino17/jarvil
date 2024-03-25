use crate::core::string_interner::Interner;
use crate::lexer::token::BinaryOperatorKind;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::{ConcreteTypesTuple, ConcretizationContext};
use crate::scope::namespace::Namespace;
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::{CoreType, Type};
use crate::types::traits::{CollectionType, OperatorCompatiblity};
use crate::{constants::common::BOOL, types::traits::TypeLike};

#[derive(Debug, Clone)]
pub struct Array {
    element_type: Type,
}

impl Array {
    pub fn new(element_type: Type) -> Array {
        Array { element_type }
    }

    pub fn element_ty(&self) -> &Type {
        &self.element_type
    }

    fn check_operator_for_array(
        &self,
        other: &Type,
        operator_kind: &BinaryOperatorKind,
        namespace: &Namespace,
    ) -> Option<Type> {
        let CoreType::Array(other_array) = other.core_ty() else {
            return None;
        };
        if self
            .element_type
            .check_operator(&other_array.element_type, operator_kind, namespace)
            .is_some()
        {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }
}

impl TypeLike for Array {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        let CoreType::Array(array_data) = other_ty.core_ty() else {
            return false;
        };
        self.element_type.is_eq(&array_data.element_type, namespace)
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: &ConcretizationContext,
        namespace: &Namespace,
    ) -> bool {
        let CoreType::Array(array_data) = other_ty.core_ty() else {
            return false;
        };
        self.element_type
            .is_structurally_eq(&array_data.element_type, context, namespace)
    }

    fn concretize(&self, context: &ConcretizationContext, namespace: &Namespace) -> Type {
        Type::new_with_array(self.element_type.concretize(context, namespace))
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        _namespace: &Namespace,
    ) -> bool {
        // TODO - add checks for interfaces which `Array` would implement like `Iterator`, `Index`
        interface_bounds.len() == 0
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let CoreType::Array(array_ty) = received_ty.core_ty() else {
            return Err(());
        };
        self.element_type.try_infer_type_or_check_equivalence(
            &array_ty.element_type,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )
    }

    fn to_string(&self, interner: &Interner, namespace: &Namespace) -> String {
        format!("[{}]", self.element_type.to_string(interner, namespace))
    }
}

impl OperatorCompatiblity for Array {
    fn check_add(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Array(array) = other.core_ty() else {
            return None;
        };
        let sub_type = &array.element_type;
        if self.element_type.is_eq(sub_type, namespace) {
            Some(Type::new_with_array(sub_type.clone()))
        } else {
            None
        }
    }

    fn check_subtract(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_multiply(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        // TODO - add case for integer to enable syntax like `[1, 2] * 2`
        None
    }

    fn check_divide(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_double_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        self.check_operator_for_array(other, &BinaryOperatorKind::DoubleEqual, namespace)
    }

    fn check_greater(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        self.check_operator_for_array(other, &BinaryOperatorKind::Greater, namespace)
    }

    fn check_less(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        self.check_operator_for_array(other, &BinaryOperatorKind::Less, namespace)
    }

    fn check_and(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }
}

impl CollectionType for Array {
    fn concrete_types(&self) -> ConcreteTypesTuple {
        ConcreteTypesTuple::new(vec![self.element_type.clone()])
    }
}
