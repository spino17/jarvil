use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use crate::scope::{
    concrete::core::{ConcreteSymbolData, ConcreteTypesRegistryKey, ConcretizationContext},
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
                                let self_symbol_data = self.semantic_data.get_core_ref();
                                let self_struct_data = self_symbol_data.get_struct_data_ref();
                                let self_concrete_types =
                                    &self_struct_data.get_concrete_types(self_key).0;
                                let self_len = self_concrete_types.len();

                                let other_symbol_data = struct_data.semantic_data.get_core_ref();
                                let other_struct_data = other_symbol_data.get_struct_data_ref();
                                let other_concrete_types =
                                    &other_struct_data.get_concrete_types(other_key).0;
                                let other_len = other_concrete_types.len();

                                assert!(self_len == other_len);
                                for i in 0..self_len {
                                    if !self_concrete_types[i].is_eq(&other_concrete_types[i]) {
                                        return false;
                                    }
                                }
                                return true;
                                /*
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
                                                let self_concrete_types = &self_struct_data
                                                    .get_concrete_types(self_key)
                                                    .0;
                                                let other_concrete_types = &other_struct_data
                                                    .get_concrete_types(other_key)
                                                    .0;
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
                                 */
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

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        let index = match self.semantic_data.index {
            Some(key) => key,
            None => unreachable!(),
        };
        assert!(self
            .semantic_data
            .symbol_data
            .is_generics_present_in_tuple_at_index(Some(index)));
        let symbol_data = self.semantic_data.get_core_ref();
        let struct_data = symbol_data.get_struct_data_ref();
        let concrete_types = &struct_data.get_concrete_types(index).0;
        let mut concretized_concrete_types = concrete_types.clone();
        for (index, ty) in concrete_types.iter().enumerate() {
            if ty.has_generics() {
                concretized_concrete_types[index] = ty.concretize(context);
            }
        }
        let new_key = self
            .semantic_data
            .symbol_data
            .register_concrete_types(Some(concretized_concrete_types), false);
        return Type::new_with_struct(
            self.name.to_string(),
            &self.semantic_data.symbol_data,
            new_key,
            false,
        );
    }
}

impl ToString for Struct {
    fn to_string(&self) -> String {
        let mut s = self.name.to_string();
        match self.semantic_data.index {
            Some(index) => {
                s.push('<');
                let symbol_data = self.semantic_data.get_core_ref();
                let struct_data = symbol_data.get_struct_data_ref();
                let concrete_types = struct_data.get_concrete_types(index);
                s.push_str(&concrete_types.to_string());
                s.push('>');
                return s;
            }
            None => return s,
        }
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
