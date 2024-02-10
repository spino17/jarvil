use super::arena::{TypeId, TypesArena};
use crate::{scope::concrete::ConcretizationContext, types::core::CoreType};

impl TypeId {
    pub fn concretize(&self, context: &ConcretizationContext, arena: &mut TypesArena) -> TypeId {
        let ty = arena.get_core_ty_ref(*self);
        match ty {
            CoreType::Struct(struct_type) => {
                todo!()
            }
            CoreType::Enum(enum_type) => {
                todo!()
            }
            CoreType::Lambda(lambda_type) => {
                todo!()
            }
            CoreType::Array(array_type) => {
                todo!()
            }
            CoreType::Tuple(tuple_type) => {
                todo!()
            }
            CoreType::HashMap(hashmap_type) => {
                todo!()
            }
            CoreType::Generic(generic_type) => {
                todo!()
            }
            CoreType::Atomic(_)
            | CoreType::Unknown
            | CoreType::Void
            | CoreType::Unset
            | CoreType::Any => *self,
        }
    }
}
