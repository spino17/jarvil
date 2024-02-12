use crate::constants::common::{BOOL, FLOAT, INT, STRING};
use crate::lexer::token::BinaryOperatorKind;
use crate::scope::{
    concrete::ConcreteTypesTuple, core::SymbolData, function::CallablePrototypeData,
    types::core::UserDefinedTypeData,
};
use crate::types::atomic::Atomic;
use crate::types::core::CoreType;
use crate::types::generic::Generic;
use crate::types::lambda::Lambda;
use crate::types::r#enum::Enum;
use crate::types::r#struct::Struct;

#[derive(Debug, Clone, Copy)]
pub struct TypeId(usize);

impl TypeId {
    pub fn new(index: usize) -> TypeId {
        TypeId(index)
    }

    pub fn arena_index(&self) -> usize {
        self.0
    }

    pub fn set_concretization_required_flag(&mut self, arena: &mut TypesArena) {
        arena.arena[self.arena_index()].is_concretization_required = true;
    }

    pub fn is_concretization_required(&self, arena: &TypesArena) -> bool {
        arena.arena[self.arena_index()].is_concretization_required
    }

    pub fn check_operator(
        &self,
        other: TypeId,
        op_kind: &BinaryOperatorKind,
        arena: &TypesArena,
    ) -> Option<TypeId> {
        let id = *self;
        let ty = arena.get_core_ty_ref(id);
        match op_kind {
            BinaryOperatorKind::Add => {}
            BinaryOperatorKind::Subtract => {}
            BinaryOperatorKind::Multiply => {}
            BinaryOperatorKind::Divide => {}
            BinaryOperatorKind::Less => {}
            BinaryOperatorKind::LessEqual => {}
            BinaryOperatorKind::Greater => {}
            BinaryOperatorKind::GreaterEqual => {}
            BinaryOperatorKind::DoubleEqual => {}
            BinaryOperatorKind::NotEqual => {}
            BinaryOperatorKind::And => {}
            BinaryOperatorKind::Or => {}
        }
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

#[derive(Debug)]
pub struct TypesArena {
    arena: Vec<Box<TypeObject>>,
}

impl TypesArena {
    pub fn new() -> TypesArena {
        let arena = TypesArena {
            arena: vec![
                // cache the atomic and null-like types
                Box::new(TypeObject::new(CoreType::Atomic(Atomic::new(INT)), false)), // 0
                Box::new(TypeObject::new(CoreType::Atomic(Atomic::new(FLOAT)), false)), // 1
                Box::new(TypeObject::new(CoreType::Atomic(Atomic::new(BOOL)), false)), // 2
                Box::new(TypeObject::new(
                    CoreType::Atomic(Atomic::new(STRING)),
                    false,
                )), // 3
                Box::new(TypeObject::new(CoreType::Unknown, false)),                  // 4
                Box::new(TypeObject::new(CoreType::Unset, false)),                    // 5
                Box::new(TypeObject::new(CoreType::Void, false)),                     // 6
                Box::new(TypeObject::new(CoreType::Any, false)),                      // 7
            ],
        };
        arena
    }

    fn add(&mut self, ty: TypeObject) -> TypeId {
        let id = self.arena.len();
        self.arena.push(Box::new(ty));
        TypeId(id)
    }

    pub fn get_core_ty_ref(&self, id: TypeId) -> &CoreType {
        &self.arena[id.arena_index()].ty
    }

    pub fn get_core_ty_mut_ref(&mut self, id: TypeId) -> &mut CoreType {
        &mut self.arena[id.arena_index()].ty
    }

    // basic-types
    pub fn new_with_atomic(&mut self, atomic_ty_kind: Atomic) -> TypeId {
        match atomic_ty_kind {
            Atomic::Int => TypeId::int(),
            Atomic::Float => TypeId::float(),
            Atomic::Bool => TypeId::bool(),
            Atomic::String => TypeId::string(),
        }
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

    // null-like types
    pub fn new_with_unknown(&mut self) -> TypeId {
        TypeId::unknown()
    }

    pub fn new_with_unset(&mut self) -> TypeId {
        TypeId::unset()
    }

    pub fn new_with_void(&mut self) -> TypeId {
        TypeId::void()
    }

    pub fn new_with_any(&mut self) -> TypeId {
        TypeId::any()
    }
}
