use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use crate::{
    core::string_interner::{Interner, StrId},
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        core::SymbolData,
        symbol::interfaces::InterfaceBounds,
        symbol::types::{
            core::UserDefinedTypeData, generic_type::GenericTypeDeclarationPlaceCategory,
        },
    },
};

#[derive(Debug, Clone)]
pub struct Generic {
    pub semantic_data: SymbolData<UserDefinedTypeData>,
}

impl Generic {
    pub fn new(symbol_data: &SymbolData<UserDefinedTypeData>) -> Generic {
        Generic {
            semantic_data: symbol_data.clone(),
        }
    }

    pub fn name(&self) -> StrId {
        self.semantic_data.identifier_name()
    }
}

impl AbstractType for Generic {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Generic(generic_data) => self.name() == generic_data.name(),
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(&self, other_ty: &Type, context: &ConcretizationContext) -> bool {
        let is_other_ty_generic = match other_ty.0.as_ref() {
            CoreType::Generic(generic_data) => Some(generic_data),
            _ => None,
        };
        let self_symbol_data = self.semantic_data.get_core_ref();
        let self_generic_data = self_symbol_data.get_generic_data_ref();
        let self_index = self_generic_data.index;
        let self_category = &self_generic_data.category;
        match self_category {
            GenericTypeDeclarationPlaceCategory::InCallable => match is_other_ty_generic {
                Some(generic_data) => {
                    let other_symbol_data = generic_data.semantic_data.get_core_ref();
                    let other_generic_data = other_symbol_data.get_generic_data_ref();
                    let other_index = other_generic_data.index;
                    let other_category = &other_generic_data.category;
                    match other_category {
                        GenericTypeDeclarationPlaceCategory::InCallable => {
                            self_index == other_index
                        }
                        GenericTypeDeclarationPlaceCategory::InStruct => false,
                    }
                }
                None => false,
            },
            GenericTypeDeclarationPlaceCategory::InStruct => match is_other_ty_generic {
                Some(_) => false,
                None => {
                    let concrete_types = match context.struct_concrete_types {
                        Some(concrete_types) => concrete_types,
                        None => unreachable!(),
                    };
                    let concrete_self_ty = &concrete_types[self_index];
                    concrete_self_ty.is_eq(other_ty)
                }
            },
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        let symbol_data = self.semantic_data.get_core_ref();
        let generic_data = symbol_data.get_generic_data_ref();
        let index = generic_data.index;
        let category = &generic_data.category;
        match category {
            GenericTypeDeclarationPlaceCategory::InStruct => match context.struct_concrete_types {
                Some(concrete_types) => concrete_types[index].clone(),
                None => unreachable!(),
            },
            GenericTypeDeclarationPlaceCategory::InCallable => {
                match context.function_local_concrete_types {
                    Some(concrete_types) => concrete_types[index].clone(),
                    None => unreachable!(),
                }
            }
        }
    }

    fn is_type_bounded_by_interfaces(&self, interface_bounds: &InterfaceBounds) -> bool {
        let symbol_data = self.semantic_data.get_core_ref();
        let ty_interface_bounds = &symbol_data.get_generic_data_ref().interface_bounds;
        interface_bounds.is_subset(ty_interface_bounds)
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        let symbol_data = self.semantic_data.get_core_ref();
        let generic_data_ref = symbol_data.get_generic_data_ref();
        let index = generic_data_ref.index;
        let decl_place = generic_data_ref.category;
        if inference_category == decl_place {
            let entry_ty = &mut inferred_concrete_types[index];
            match entry_ty {
                InferredConcreteTypesEntry::Uninferred => {
                    *entry_ty = InferredConcreteTypesEntry::Inferred(received_ty.clone());
                    *num_inferred_types += 1;
                    Ok(())
                }
                InferredConcreteTypesEntry::Inferred(present_ty) => {
                    if !present_ty.is_eq(received_ty) {
                        return Err(());
                    }
                    Ok(())
                }
            }
        } else {
            debug_assert!(decl_place == GenericTypeDeclarationPlaceCategory::InStruct);
            let global_concrete_types = match global_concrete_types {
                Some(concrete_types) => concrete_types,
                None => unreachable!(),
            };
            let expected_ty = &global_concrete_types[index];
            if !expected_ty.is_eq(received_ty) {
                return Err(());
            }
            Ok(())
        }
    }

    fn to_string(&self, interner: &Interner) -> String {
        let symbol_data = self.semantic_data.get_core_ref();
        let generic_data = symbol_data.get_generic_data_ref();
        let interface_bounds = &generic_data.interface_bounds;
        format!(
            "{}{}",
            interner.lookup(self.name()),
            interface_bounds.to_string(interner)
        )
    }
}

impl OperatorCompatiblity for Generic {
    // TODO - add implementations of below methods based on the interfaces bounding the generic type
    fn check_add(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_subtract(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_multiply(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_divide(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_double_equal(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_greater(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_less(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_and(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type) -> Option<Type> {
        None
    }
}
