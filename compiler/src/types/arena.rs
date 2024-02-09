use super::generic::Generic;
use super::lambda::Lambda;
use super::r#enum::Enum;
use super::{array::core::Array, atomic::Atomic, core::CoreType};
use crate::types::r#struct::Struct;
use crate::{
    core::string_interner::Interner,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        core::SymbolData,
        function::CallablePrototypeData,
        interfaces::InterfaceBounds,
        types::{core::UserDefinedTypeData, generic_type::GenericTypeDeclarationPlaceCategory},
    },
};

pub trait TypeLike {
    fn is_eq(&self, other_ty: &TypeId, arena: &mut TypesArena) -> bool;
    fn is_structurally_eq(
        &self,
        other_ty: &TypeId,
        context: &ConcretizationContext,
        arena: &mut TypesArena,
    ) -> bool;
    fn concretize(&self, context: &ConcretizationContext, arena: &mut TypesArena) -> TypeId;
    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &TypeId,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        arena: &mut TypesArena,
    ) -> Result<(), ()>;
    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        arena: &mut TypesArena,
    ) -> bool;
    fn to_string(&self, interner: &Interner, arena: &mut TypesArena) -> String;
    fn check_add(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_subtract(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_multiply(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_divide(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_double_equal(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_greater(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_less(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_and(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_or(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_not_equal(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
    fn check_greater_equal(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
    fn check_less_equal(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeId(usize);

impl TypeLike for TypeId {
    fn is_eq(&self, other_ty: &TypeId, arena: &mut TypesArena) -> bool {
        todo!()
    }

    fn is_structurally_eq(
        &self,
        other_ty: &TypeId,
        context: &ConcretizationContext,
        arena: &mut TypesArena,
    ) -> bool {
        todo!()
    }

    fn concretize(&self, context: &ConcretizationContext, arena: &mut TypesArena) -> TypeId {
        todo!()
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &TypeId,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        arena: &mut TypesArena,
    ) -> Result<(), ()> {
        todo!()
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        arena: &mut TypesArena,
    ) -> bool {
        todo!()
    }

    fn to_string(&self, interner: &Interner, arena: &mut TypesArena) -> String {
        todo!()
    }

    fn check_add(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_subtract(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_multiply(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_divide(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_double_equal(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_greater(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_less(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_and(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_or(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }

    fn check_not_equal(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
    fn check_greater_equal(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
    fn check_less_equal(&self, other: &TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
}

#[derive(Debug)]
struct TypeObject {
    ty: CoreType,
    is_concretization_required: bool,
}

impl TypeObject {
    fn new(core_ty: CoreType, is_concretization_required: bool) -> TypeObject {
        TypeObject {
            ty: core_ty,
            is_concretization_required,
        }
    }
}

#[derive(Debug, Default)]
pub struct TypesArena {
    arena: Vec<TypeObject>,
}

impl TypesArena {
    fn add(&mut self, ty: TypeObject) -> TypeId {
        let id = self.arena.len();
        self.arena.push(ty);
        TypeId(id)
    }

    pub fn new_with_atomic(&mut self, name: &str) -> TypeId {
        self.add(TypeObject::new(CoreType::Atomic(Atomic::new(name)), false))
    }

    pub fn new_with_array(&mut self, element_type: TypeId) -> TypeId {
        todo!()
    }

    pub fn new_with_tuple(&mut self, types: Vec<TypeId>) -> TypeId {
        todo!()
    }

    pub fn new_with_hashmap(&mut self, key_type: TypeId, value_type: TypeId) -> TypeId {
        todo!()
    }

    // user-defined-types
    pub fn new_with_struct(
        &mut self,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        concrete_types: Option<ConcreteTypesTuple>,
    ) -> TypeId {
        self.add(TypeObject::new(
            CoreType::Struct(Struct::new(symbol_data, concrete_types)),
            false,
        ))
    }

    pub fn new_with_enum(
        &mut self,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        concrete_types: Option<ConcreteTypesTuple>,
    ) -> TypeId {
        self.add(TypeObject::new(
            CoreType::Enum(Enum::new(symbol_data, concrete_types)),
            false,
        ))
    }

    pub fn new_with_lambda_named(
        &mut self,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        concrete_types: Option<ConcreteTypesTuple>,
    ) -> TypeId {
        self.add(TypeObject::new(
            CoreType::Lambda(Lambda::new_with_named(symbol_data, concrete_types)),
            false,
        ))
    }

    pub fn new_with_lambda_unnamed(&mut self, function_prototype: CallablePrototypeData) -> TypeId {
        self.add(TypeObject::new(
            CoreType::Lambda(Lambda::new_with_unnamed(function_prototype)),
            false,
        ))
    }

    pub fn new_with_generic(&mut self, symbol_data: &SymbolData<UserDefinedTypeData>) -> TypeId {
        self.add(TypeObject::new(
            CoreType::Generic(Generic::new(symbol_data)),
            false,
        ))
    }

    pub fn new_with_unknown(&mut self) -> TypeId {
        self.add(TypeObject::new(CoreType::Unknown, false))
    }

    pub fn new_with_unset(&mut self) -> TypeId {
        self.add(TypeObject::new(CoreType::Unset, false))
    }

    pub fn new_with_void(&mut self) -> TypeId {
        self.add(TypeObject::new(CoreType::Void, false))
    }

    pub fn new_with_any(&mut self) -> TypeId {
        self.add(TypeObject::new(CoreType::Any, false))
    }
}
