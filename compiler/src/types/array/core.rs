use crate::lexer::token::BinaryOperatorKind;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::{TurbofishTypes, TypeGenericsInstantiationContext};
use crate::scope::namespace::Namespace;
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory;
use crate::scope::traits::InstantiationContext;
use crate::types::core::{CoreType, Type, TypeStringifyContext};
use crate::types::traits::{CollectionType, OperatorCompatiblity};
use crate::{constants::common::BOOL, types::traits::TypeLike};

#[derive(Debug, Clone)]
pub struct Array {
    element_ty: Type,
}

impl Array {
    pub fn new(element_ty: Type) -> Array {
        Array { element_ty }
    }

    pub fn element_ty(&self) -> &Type {
        &self.element_ty
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
            .element_ty
            .check_operator(&other_array.element_ty, operator_kind, namespace)
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

        self.element_ty.is_eq(&array_data.element_ty, namespace)
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        let CoreType::Array(array_data) = other_ty.core_ty() else {
            return false;
        };

        self.element_ty
            .is_structurally_eq(&array_data.element_ty, context, namespace)
    }

    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type {
        Type::new_with_array(self.element_ty.concretize(context, namespace))
    }

    fn is_ty_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        _namespace: &Namespace,
    ) -> bool {
        // TODO - add checks for interfaces which `Array` would implement like `Iterator`, `Index`
        interface_bounds.len() == 0
    }

    fn try_infer_ty_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&TurbofishTypes>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let CoreType::Array(array_ty) = received_ty.core_ty() else {
            return Err(());
        };

        self.element_ty.try_infer_ty_or_check_equivalence(
            &array_ty.element_ty,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )
    }

    fn to_string(&self, context: TypeStringifyContext) -> String {
        format!("[{}]", self.element_ty.to_string(context))
    }
}

impl OperatorCompatiblity for Array {
    fn check_add(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Array(array) = other.core_ty() else {
            return None;
        };

        let sub_ty = &array.element_ty;

        if self.element_ty.is_eq(sub_ty, namespace) {
            Some(Type::new_with_array(sub_ty.clone()))
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
    fn concrete_types(&self) -> TurbofishTypes {
        TurbofishTypes::new(vec![self.element_ty.clone()])
    }
}
