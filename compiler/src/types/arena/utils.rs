use super::arena::{TypeId, TypesArena};
use crate::types::core::CoreType;

impl TypeId {
    // constructors for few cached types!
    pub fn int() -> TypeId {
        TypeId::new(0)
    }

    pub fn float() -> TypeId {
        TypeId::new(1)
    }

    pub fn bool() -> TypeId {
        TypeId::new(2)
    }

    pub fn string() -> TypeId {
        TypeId::new(3)
    }

    pub fn unknown() -> TypeId {
        TypeId::new(4)
    }

    pub fn unset() -> TypeId {
        TypeId::new(5)
    }

    pub fn void() -> TypeId {
        TypeId::new(6)
    }

    pub fn any() -> TypeId {
        TypeId::new(7)
    }

    pub fn is_int(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_int()
    }

    pub fn is_float(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_float()
    }

    pub fn is_bool(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_bool()
    }

    pub fn is_string(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_string()
    }

    pub fn is_array(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_array()
    }

    pub fn is_tuple(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_tuple()
    }

    pub fn is_hashmap(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_hashmap()
    }

    pub fn is_enum(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_enum()
    }

    pub fn is_lambda(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_lambda()
    }

    pub fn is_unknown(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_unknown()
    }

    pub fn is_unset(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_unset()
    }

    pub fn is_void(&self, arena: &TypesArena) -> bool {
        arena.core_ty_ref(*self).is_void()
    }

    pub fn is_numeric(&self, arena: &mut TypesArena) -> bool {
        self.is_int(arena) || self.is_float(arena)
    }

    pub fn is_immutable(&self, arena: &TypesArena) -> bool {
        self.is_string(arena) || self.is_tuple(arena)
    }

    pub fn is_hashable(&self, arena: &TypesArena) -> bool {
        // `int`, `float`, `str` and `tuple` with hashable sub_types are only hashable types
        match arena.core_ty_ref(*self) {
            CoreType::Atomic(atomic) => atomic.is_int() || atomic.is_string() || atomic.is_float(),
            CoreType::Tuple(tuple) => {
                for ty in &tuple.sub_types {
                    if !ty.is_hashable() {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}
