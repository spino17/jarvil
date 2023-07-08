use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use crate::scope::{
    concrete::core::{ConcreteSymbolData, ConcreteTypesRegistryKey},
    core::SymbolData,
    types::core::UserDefinedTypeData,
};

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub semantic_data: ConcreteSymbolData<UserDefinedTypeData>,
}

impl Struct {
    pub fn new(
        name: String,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        index: Option<ConcreteTypesRegistryKey>,
    ) -> Struct {
        Struct {
            name,
            semantic_data: ConcreteSymbolData {
                symbol_data: symbol_data.clone(),
                index,
            },
        }
    }
}

impl AbstractType for Struct {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Struct(struct_data) => {
                if struct_data.name.eq(&self.name) {
                    match self.semantic_data.index {
                        Some(self_key) => match struct_data.semantic_data.index {
                            Some(other_key) => {
                                match &*self.semantic_data.symbol_data.0 .0.as_ref().borrow() {
                                    UserDefinedTypeData::Struct(self_struct_data) => {
                                        match &*struct_data
                                            .semantic_data
                                            .symbol_data
                                            .0
                                             .0
                                            .as_ref()
                                            .borrow()
                                        {
                                            UserDefinedTypeData::Struct(other_struct_data) => {
                                                let self_concrete_types =
                                                    self_struct_data.get_concrete_types(self_key);
                                                let other_concrete_types =
                                                    other_struct_data.get_concrete_types(other_key);
                                                let self_len = self_concrete_types.len();
                                                let other_len = other_concrete_types.len();
                                                assert!(self_len == other_len);
                                                for i in 0..self_len {
                                                    if !self_concrete_types[i]
                                                        .is_eq(&other_concrete_types[i])
                                                    {
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
                            None => unreachable!(),
                        },
                        None => return true,
                    }
                }
                return false;
            }
            CoreType::Any => true,
            _ => false,
        }
    }
}

impl ToString for Struct {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

// TODO: operator compatiblity for struct types can be defined using interfaces
// This is called `operator-overloading`
impl OperatorCompatiblity for Struct {
    fn check_add(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Add` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_subtract(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Subtract` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_multiply(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Multiply` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_divide(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Divide` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_double_equal(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Equal` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_greater(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Greater` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_less(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Less` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_and(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `And` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_or(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Or` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }
}
