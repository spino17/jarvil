use std::rc::Rc;

use super::core::{AbstractType, CoreType, OperatorCompatiblity, ToType, Type};
use crate::scope::{
    concrete::core::ConcretizationContext,
    core::SymbolData,
    interfaces::InterfaceBounds,
    types::{core::UserDefinedTypeData, generic_type::GenericTypeDeclarationPlaceCategory},
};

#[derive(Debug, Clone)]
pub struct Generic {
    pub name: String,
    pub semantic_data: SymbolData<UserDefinedTypeData>,
}

impl Generic {
    pub fn new(name: String, symbol_data: &SymbolData<UserDefinedTypeData>) -> Generic {
        Generic {
            name,
            semantic_data: symbol_data.clone(),
        }
    }
}

impl AbstractType for Generic {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Generic(generic_data) => {
                if self.name == generic_data.name {
                    true
                } else {
                    false
                }
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

    fn is_type_bounded_by_interfaces(&self, interface_bounds: &InterfaceBounds) -> bool {
        let symbol_data = self.semantic_data.get_core_ref();
        let ty_interface_bounds = &symbol_data.get_generic_data_ref().interface_bounds;
        return interface_bounds.is_subset(ty_interface_bounds);
    }

    fn has_generics(&self) -> bool {
        unreachable!()
    }

    fn try_infer_type(
        &self,
        generics_containing_ty: &Type,
        inferred_concrete_types: &mut Vec<crate::parser::type_checker::InferredConcreteTypesEntry>,
        num_inferred_types: &mut usize,
        generic_ty_decl_place: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        match generics_containing_ty.0.as_ref() {
            CoreType::Generic(generic_ty) => {
                todo!()
            }
            _ => Err(()),
        }
    }
}

impl ToType for Generic {
    fn get_type(&self) -> Type {
        Type(Rc::new(CoreType::Generic(self.clone())))
    }
}

impl ToString for Generic {
    fn to_string(&self) -> String {
        let symbol_data = self.semantic_data.get_core_ref();
        let generic_data = symbol_data.get_generic_data_ref();
        let interface_bounds = &generic_data.interface_bounds;
        format!("{}{}", self.name, interface_bounds.to_string())
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
