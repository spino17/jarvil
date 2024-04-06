use super::core::{Type, TypeStringifyContext};
use crate::constants::common::BOOL;
use crate::core::string_interner::StrId;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::{TurbofishTypes, TypeGenericsInstantiationContext};
use crate::scope::namespace::Namespace;
use crate::scope::symbol::core::SymbolIndex;
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::core::UserDefinedTypeData;
use crate::scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory;
use crate::scope::traits::InstantiationContext;

pub trait TypeLike {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool;
    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool;
    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type;
    fn try_infer_ty_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&TurbofishTypes>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()>;
    fn is_ty_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        namespace: &Namespace,
    ) -> bool;
    fn to_string(&self, context: TypeStringifyContext) -> String;
}

pub trait UserDefinedType {
    fn symbol_index(&self) -> SymbolIndex<UserDefinedTypeData>;
    fn concrete_types(&self) -> Option<&TurbofishTypes>;
    fn name(&self) -> StrId;
}

pub trait CollectionType {
    fn concrete_types(&self) -> TurbofishTypes;
}

pub trait OperatorCompatiblity {
    fn check_add(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_subtract(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_multiply(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_divide(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_double_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_greater(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_less(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_and(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_or(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_not_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        if self.check_double_equal(other, namespace).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }
    fn check_greater_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        if self.check_greater(other, namespace).is_some()
            && self.check_double_equal(other, namespace).is_some()
        {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }
    fn check_less_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        if self.check_less(other, namespace).is_some()
            && self.check_double_equal(other, namespace).is_some()
        {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }
}
