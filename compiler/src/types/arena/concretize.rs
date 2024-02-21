use super::arena::{TypeId, TypesArena};
use crate::{scope::concrete::ConcretizationContext, types::core::CoreType};

impl TypeId {
    pub fn concretize(&self, context: &ConcretizationContext, arena: &mut TypesArena) -> TypeId {
        let ty = arena.core_ty_ref(*self);
        match ty {
            CoreType::Struct(struct_ty) => {
                todo!()
            }
            CoreType::Enum(enum_ty) => {
                todo!()
            }
            CoreType::Lambda(lambda_ty) => {
                todo!()
            }
            CoreType::Array(array_ty) => {
                todo!()
            }
            CoreType::Tuple(tuple_ty) => {
                todo!()
            }
            CoreType::HashMap(hashmap_ty) => {
                todo!()
            }
            CoreType::Generic(generic_ty) => {
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
