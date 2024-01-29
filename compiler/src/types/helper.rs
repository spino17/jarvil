use super::core::Type;
use crate::{
    core::string_interner::{Interner, StrId},
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        core::SymbolData,
        interfaces::InterfaceBounds,
        types::{
            core::UserDefinedTypeData,
            generic_type::{GenericTypeData, GenericTypeDeclarationPlaceCategory},
        },
    },
    types::core::AbstractType,
};
use text_size::TextRange;

pub fn get_unbounded_generic_type_with_declaration_index(
    index: usize,
    interner: &mut Interner,
) -> Type {
    Type::new_with_generic(&SymbolData::new(
        interner.intern("T"),
        UserDefinedTypeData::Generic(GenericTypeData {
            category: GenericTypeDeclarationPlaceCategory::InStruct,
            index,
            interface_bounds: InterfaceBounds::new(vec![]),
        }),
        TextRange::default(),
        None,
    ))
}

pub fn try_infer_types_from_tuple(
    base_types_tuple: &Vec<Type>,
    generics_containing_types_tuple: &Vec<Type>,
    inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
    global_concrete_types: Option<&ConcreteTypesTuple>,
    num_inferred_types: &mut usize,
    inference_category: GenericTypeDeclarationPlaceCategory,
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
        )?;
    }
    Ok(())
}

pub trait StructEnumType {
    fn get_concrete_types(&self) -> Option<&ConcreteTypesTuple>;
    fn get_name(&self) -> StrId;
}

pub fn struct_enum_compare_fn<
    T: StructEnumType,
    F: Fn(&Type, &Type, &ConcretizationContext) -> bool,
>(
    base: &T,
    other: &T,
    ty_cmp_func: F,
    context: &ConcretizationContext,
) -> bool {
    if base.get_name() != other.get_name() {
        return false;
    }
    let Some(self_concrete_types) = base.get_concrete_types() else {
        return true;
    };
    let Some(other_concrete_types) = other.get_concrete_types() else {
        unreachable!()
    };
    let self_len = self_concrete_types.len();
    let other_len = other_concrete_types.len();
    debug_assert!(self_len == other_len);
    for i in 0..self_len {
        if !ty_cmp_func(&self_concrete_types[i], &other_concrete_types[i], context) {
            return false;
        }
    }
    true
}
