use std::borrow::Borrow;

use super::core::OperatorCompatiblity;
use crate::scope::concrete::{ConcreteSymbolData, ConcreteTypesRegistryKey};
use crate::scope::core::SymbolData;
use crate::scope::types::core::UserDefinedTypeData;
use crate::types::core::{AbstractType, CoreType, Type};

#[derive(Debug)]
pub struct Lambda {
    pub name: Option<String>,
    pub semantic_data: ConcreteSymbolData<UserDefinedTypeData>,
}

impl Lambda {
    pub fn new(
        name: Option<String>,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        index: Option<ConcreteTypesRegistryKey>,
    ) -> Lambda {
        Lambda {
            name,
            semantic_data: ConcreteSymbolData {
                symbol_data: symbol_data.clone(),
                index,
            },
        }
    }
}

impl AbstractType for Lambda {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::Lambda(lambda_data) => {
                // Lambda type has structural equivalence checks unlike struct type which is only compared by it's name
                // This structural equivalence is important because we can have lambda types which are not named for example:
                // let x = (...) -> <Type>: block would have `x` to be of type `Lambda` with no name but symbol_data.
                match &*self.semantic_data.symbol_data.0.as_ref().borrow() {
                    UserDefinedTypeData::Lambda(self_data_ref) => {
                        match &*lambda_data.semantic_data.symbol_data.0.as_ref().borrow() {
                            UserDefinedTypeData::Lambda(base_data_ref) => {
                                let (self_param_types, self_return_type) = (
                                    &self_data_ref.meta_data.prototype.params,
                                    &self_data_ref.meta_data.prototype.return_type,
                                );
                                let (base_param_types, base_return_type) = (
                                    &base_data_ref.meta_data.prototype.params,
                                    &base_data_ref.meta_data.prototype.return_type,
                                );
                                let self_params_len = self_param_types.len();
                                let base_params_len = base_param_types.len();
                                if self_params_len != base_params_len {
                                    return false;
                                }
                                if !self_return_type.is_eq(&base_return_type) {
                                    return false;
                                }
                                for index in 0..self_params_len {
                                    if !self_param_types[index].is_eq(&base_param_types[index]) {
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

    fn stringify(&self) -> String {
        match &self.name {
            Some(name) => {
                todo!()
            }
            None => unreachable!(),
        }
    }
}

impl ToString for Lambda {
    fn to_string(&self) -> String {
        match &self.name {
            Some(name) => format!("{}", name),
            None => match &*self.semantic_data.symbol_data.0.as_ref().borrow() {
                UserDefinedTypeData::Lambda(data) => {
                    let self_param_types = &data.meta_data.prototype.params;
                    let self_return_type = &data.meta_data.prototype.return_type;
                    let mut params_str = "".to_string();
                    let mut flag = false;
                    for param in self_param_types {
                        if flag {
                            params_str.push_str(", ")
                        }
                        params_str.push_str(&format!("{}", param.to_string()));
                        flag = true;
                    }
                    format!("lambda({}) -> {}", params_str, self_return_type)
                }
                _ => unreachable!(),
            },
        }
    }
}

impl OperatorCompatiblity for Lambda {
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
