use super::arena::{TypeId, TypesArena};
use super::traits::TypeLike;
use crate::constants::common::{AND, UNKNOWN, UNSET};
use crate::types::core::CoreType;
use crate::{
    core::string_interner::Interner,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        symbol::interfaces::InterfaceBounds,
        symbol::types::generic_type::GenericTypeDeclarationPlaceCategory,
    },
};

impl TypeLike for TypeId {
    fn is_eq(&self, other_ty: TypeId, arena: &TypesArena) -> bool {
        match arena.core_ty_ref(*self) {
            CoreType::Atomic(atomic_ty) => todo!(),
            CoreType::Struct(struct_ty) => todo!(),
            CoreType::Lambda(lambda_ty) => todo!(),
            CoreType::Array(array_ty) => todo!(),
            CoreType::Tuple(tuple_ty) => todo!(),
            CoreType::HashMap(hashmap_ty) => todo!(),
            CoreType::Generic(generic_ty) => todo!(),
            CoreType::Enum(enum_ty) => todo!(),
            CoreType::Void => matches!(arena.core_ty_ref(other_ty), CoreType::Void),
            CoreType::Unknown => false,
            CoreType::Unset => false,
            CoreType::Any => true,
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: TypeId,
        context: &ConcretizationContext,
        arena: &TypesArena,
    ) -> bool {
        match arena.core_ty_ref(*self) {
            CoreType::Atomic(atomic_ty) => todo!(),
            CoreType::Struct(struc_ty) => todo!(),
            CoreType::Enum(enum_ty) => todo!(),
            CoreType::Lambda(lambda_ty) => todo!(),
            CoreType::Array(array_ty) => todo!(),
            CoreType::Tuple(tuple_ty) => todo!(),
            CoreType::HashMap(hashmap_ty) => todo!(),
            CoreType::Generic(generic_ty) => todo!(),
            CoreType::Void => matches!(arena.core_ty_ref(other_ty), CoreType::Void),
            CoreType::Unknown | CoreType::Unset | CoreType::Any => unreachable!(),
        }
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: TypeId,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        arena: &TypesArena,
    ) -> Result<(), ()> {
        match arena.core_ty_ref(*self) {
            CoreType::Struct(struct_ty) => todo!(),
            CoreType::Enum(enum_ty) => todo!(),
            CoreType::Lambda(lambda_ty) => todo!(),
            CoreType::Array(array_ty) => todo!(),
            CoreType::Tuple(tuple_ty) => todo!(),
            CoreType::HashMap(hashmap_ty) => todo!(),
            CoreType::Generic(generic_ty) => todo!(),
            CoreType::Atomic(_)
            | CoreType::Unknown
            | CoreType::Void
            | CoreType::Unset
            | CoreType::Any => {
                if !self.is_eq(received_ty, arena) {
                    return Err(());
                }
                Ok(())
            }
        }
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        arena: &TypesArena,
    ) -> bool {
        if interface_bounds.len() == 0 {
            return true;
        }
        match arena.core_ty_ref(*self) {
            CoreType::Struct(struct_ty) => {
                todo!()
            }
            CoreType::Generic(generic_ty) => {
                todo!()
            }
            CoreType::Array(array_ty) => todo!(),
            CoreType::HashMap(hashmap_ty) => {
                todo!()
            }
            CoreType::Tuple(tuple_ty) => todo!(),
            CoreType::Lambda(_)
            | CoreType::Atomic(_)
            | CoreType::Enum(_)
            | CoreType::Any
            | CoreType::Unknown
            | CoreType::Unset
            | CoreType::Void => false,
        }
    }

    fn to_string(&self, interner: &Interner, arena: &TypesArena) -> String {
        match arena.core_ty_ref(*self) {
            CoreType::Atomic(atomic_ty) => todo!(),
            CoreType::Struct(struct_ty) => todo!(),
            CoreType::Enum(enum_ty) => todo!(),
            CoreType::Lambda(lambda_ty) => todo!(),
            CoreType::Array(array_ty) => todo!(),
            CoreType::Tuple(tuple_ty) => todo!(),
            CoreType::HashMap(hashmap_ty) => todo!(),
            CoreType::Generic(generic_ty) => todo!(),
            CoreType::Unknown => UNKNOWN.to_string(),
            CoreType::Void => "()".to_string(),
            CoreType::Unset => UNSET.to_string(),
            CoreType::Any => AND.to_string(),
        }
    }
}
