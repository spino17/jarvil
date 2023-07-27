use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use crate::scope::{
    concrete::core::ConcretizationContext,
    core::SymbolData,
    types::{core::UserDefinedTypeData, generic_type::GenericTypeDeclarationPlaceCategory},
};

#[derive(Debug)]
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
}

impl ToString for Generic {
    fn to_string(&self) -> String {
        let mut s = format!("{}{{", self.name);
        let symbol_data = self.semantic_data.get_core_ref();
        let generic_data = symbol_data.get_generic_data_ref();
        let interface_bounds = &generic_data.interface_bounds;
        let len = interface_bounds.len();
        if len > 0 {
            s.push_str(&interface_bounds[0].to_string());
        }
        for i in 1..len {
            s.push_str(" + ");
            s.push_str(&interface_bounds[i].to_string());
        }
        s.push('}');
        s
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
