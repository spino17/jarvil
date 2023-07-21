use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use crate::scope::{
    concrete::core::ConcretizationContext,
    core::SymbolData,
    types::{core::UserDefinedTypeData, generic_type::GenericTypeDeclarationPlaceCategory},
};

#[derive(Debug)]
pub struct Generic {
    pub semantic_data: SymbolData<UserDefinedTypeData>,
}

impl Generic {
    pub fn new(symbol_data: &SymbolData<UserDefinedTypeData>) -> Generic {
        Generic {
            semantic_data: symbol_data.clone(),
        }
    }
}

impl AbstractType for Generic {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Generic(generic_data) => {
                let self_symbol_data = self.semantic_data.get_core_ref();
                let self_generic_data = self_symbol_data.get_generic_data_ref();
                let self_interface_bounds = &self_generic_data.interface_bounds;
                let self_len = self_interface_bounds.len();

                let other_symbol_data = generic_data.semantic_data.get_core_ref();
                let other_generic_data = other_symbol_data.get_generic_data_ref();
                let other_interface_bounds = &other_generic_data.interface_bounds;
                let other_len = other_interface_bounds.len();

                if self_len != other_len {
                    return false;
                }
                for i in 0..self_len {
                    if !self_interface_bounds[i].is_eq(&other_interface_bounds[i]) {
                        return false;
                    }
                }
                return true;
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        let symbol_data = self.semantic_data.get_core_ref();
        let generic_data = symbol_data.get_generic_data_ref();
        let index = generic_data.index;
        let category = &generic_data.category;
        match category {
            GenericTypeDeclarationPlaceCategory::InStruct => {
                return context.struct_concrete_types[index].clone()
            }
            GenericTypeDeclarationPlaceCategory::InCallable => {
                return context.function_local_concrete_types[index].clone()
            }
        }
    }
}

impl ToString for Generic {
    fn to_string(&self) -> String {
        todo!()
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
