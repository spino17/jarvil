use super::core::Type;
use crate::{
    core::string_interner::{Interner, StrId},
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        namespace::Namespace,
        scope::ScopeIndex,
        symbol::{core::SymbolIndex, types::generic_type::GenericTypeDeclarationPlaceCategory},
    },
    types::core::AbstractType,
};

pub fn unbounded_generic_type_with_declaration_index(index: usize, interner: &Interner) -> Type {
    match index {
        0 => Type::new_with_generic(SymbolIndex::new(ScopeIndex::side(), interner.intern("T"))),
        1 => Type::new_with_generic(SymbolIndex::new(ScopeIndex::side(), interner.intern("U"))),
        _ => unreachable!(),
    }
}

pub fn try_infer_types_from_tuple(
    base_types_tuple: &Vec<Type>,
    generics_containing_types_tuple: &Vec<Type>,
    inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
    global_concrete_types: Option<&ConcreteTypesTuple>,
    num_inferred_types: &mut usize,
    inference_category: GenericTypeDeclarationPlaceCategory,
    namespace: &Namespace,
) -> Result<(), ()> {
    if base_types_tuple.len() != generics_containing_types_tuple.len() {
        return Err(());
    }
    for (index, generics_containing_ty) in generics_containing_types_tuple.iter().enumerate() {
        let base_ty = &base_types_tuple[index];
        generics_containing_ty.try_infer_type_or_check_equivalence(
            base_ty,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )?;
    }
    Ok(())
}

pub trait UserDefinedType {
    fn concrete_types(&self) -> Option<&ConcreteTypesTuple>;
    fn name(&self) -> StrId;
}

pub fn user_defined_ty_compare_fn<
    T: UserDefinedType,
    F: Fn(&Type, &Type, &ConcretizationContext, &Namespace) -> bool,
>(
    base: &T,
    other: &T,
    ty_cmp_func: F,
    context: &ConcretizationContext,
    namespace: &Namespace,
) -> bool {
    if base.name() != other.name() {
        return false;
    }
    let Some(self_concrete_types) = base.concrete_types() else {
        return true;
    };
    let Some(other_concrete_types) = other.concrete_types() else {
        unreachable!()
    };
    let self_len = self_concrete_types.len();
    let other_len = other_concrete_types.len();
    debug_assert!(self_len == other_len);
    for i in 0..self_len {
        if !ty_cmp_func(
            &self_concrete_types[i],
            &other_concrete_types[i],
            context,
            namespace,
        ) {
            return false;
        }
    }
    true
}
