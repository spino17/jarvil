use super::generic::Generic;
use super::lambda::Lambda;
use super::r#enum::Enum;
use super::{atomic::Atomic, core::CoreType};
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
    fn concretize(&self, context: &ConcretizationContext, arena: &mut TypesArena) -> TypeId;
    fn to_string(&self, interner: &Interner, arena: &TypesArena) -> String;
}

pub trait TypesOperatorCompatiblity {
    fn check_add(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_subtract(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_multiply(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_divide(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_double_equal(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_greater(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_less(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_and(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_or(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId>;
    fn check_not_equal(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
    fn check_greater_equal(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
    fn check_less_equal(&self, other: TypeId, arena: &mut TypesArena) -> Option<TypeId> {
        todo!()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeId(usize);

impl TypeLike for TypeId {
    fn is_eq(&self, other_ty: TypeId, arena: &TypesArena) -> bool {
        todo!()
    }

    fn is_structurally_eq(
        &self,
        other_ty: TypeId,
        context: &ConcretizationContext,
        arena: &TypesArena,
    ) -> bool {
        todo!()
    }

    fn concretize(&self, context: &ConcretizationContext, arena: &mut TypesArena) -> TypeId {
        todo!()
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
        todo!()
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        arena: &TypesArena,
    ) -> bool {
        todo!()
    }

    fn to_string(&self, interner: &Interner, arena: &TypesArena) -> String {
        todo!()
    }
}

impl TypeId {
    pub fn is_void(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_string(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_array(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_bool(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_int(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_float(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_enum(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_numeric(&self, arena: &mut TypesArena) -> bool {
        self.is_int(arena) || self.is_float(arena)
    }

    pub fn is_lambda(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_hashmap(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_immutable(&self, arena: &TypesArena) -> bool {
        self.is_string(arena) || self.is_tuple(arena)
    }

    pub fn is_tuple(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_hashable(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_unknown(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn is_unset(&self, arena: &TypesArena) -> bool {
        todo!()
    }

    pub fn set_concretization_required_flag(&mut self, arena: &mut TypesArena) {
        todo!()
    }

    pub fn is_concretization_required(&self, arena: &TypesArena) -> bool {
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
    arena: Vec<Box<TypeObject>>,
}

impl TypesArena {
    fn add(&mut self, ty: TypeObject) -> TypeId {
        let id = self.arena.len();
        self.arena.push(Box::new(ty));
        TypeId(id)
    }

    fn get_core_ty_ref(&self, id: TypeId) -> &CoreType {
        let id = id.0;
        &self.arena[id].ty
    }

    fn get_core_ty_mut_ref(&mut self, id: TypeId) -> &mut CoreType {
        let id = id.0;
        &mut self.arena[id].ty
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
