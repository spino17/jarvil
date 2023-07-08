use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use crate::scope::{core::SymbolData, types::core::UserDefinedTypeData};

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
            CoreType::Generic(base_generic_data) => {
                match &*self.semantic_data.0 .0.as_ref().borrow() {
                    UserDefinedTypeData::Generic(self_generic_data_ref) => {
                        match &*base_generic_data.semantic_data.0 .0.as_ref().borrow() {
                            // The generic types equivalence is computed structurally by checking if both
                            // are bounded by the same set of interfaces
                            UserDefinedTypeData::Generic(base_generic_data_ref) => {
                                let self_interface_bounds = &self_generic_data_ref.interface_bounds;
                                let base_interface_bounds = &base_generic_data_ref.interface_bounds;
                                let self_len = self_interface_bounds.len();
                                let base_len = base_interface_bounds.len();
                                if self_len != base_len {
                                    return false;
                                }
                                for i in 0..self_len {
                                    if !self_interface_bounds[i].is_eq(&base_interface_bounds[i]) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            CoreType::Any => true,
            _ => false,
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
