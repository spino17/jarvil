use super::arena::{TypeId, TypesArena};
use crate::{
    core::string_interner::Interner,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        interfaces::InterfaceBounds,
        types::generic_type::GenericTypeDeclarationPlaceCategory,
    },
};

pub trait TypeLike {
    fn is_eq(&self, other_ty: TypeId, arena: &TypesArena) -> bool;
    fn is_structurally_eq(
        &self,
        other_ty: TypeId,
        context: &ConcretizationContext,
        arena: &TypesArena,
    ) -> bool;
    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        arena: &TypesArena,
    ) -> bool;
    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: TypeId,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        arena: &TypesArena,
    ) -> Result<(), ()>;
    fn to_string(&self, interner: &Interner, arena: &TypesArena) -> String;
}

pub trait TypesOperatorCompatiblity {
    fn check_add(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_subtract(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_multiply(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_divide(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_double_equal(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_greater(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_less(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_and(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_or(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId>;
    fn check_not_equal(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId> {
        todo!()
    }
    fn check_greater_equal(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId> {
        todo!()
    }
    fn check_less_equal(&self, id: TypeId, other: TypeId, arena: &TypesArena) -> Option<TypeId> {
        todo!()
    }
}
