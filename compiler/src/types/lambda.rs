use super::core::OperatorCompatiblity;
use crate::scope::concrete::core::{ConcreteSymbolData, ConcreteTypesRegistryKey};
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
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Lambda(lambda_data) => {
                // Lambda type has structural equivalence checks unlike struct type which is only compared by it's name
                // This structural equivalence is important because we can have lambda types which are not named for example:
                // let x = (...) -> <Type>: block would have `x` to be of type `Lambda` with no name but symbol_data.

                // TODO - once generics gets integrated we have concrete types attached to the lambda type
                // to enable structural equivalence of lambda type we have to get the concretized version of
                // function prototype which then we would compare like non-generic lambda types.
                match &*self.semantic_data.symbol_data.0 .0.as_ref().borrow() {
                    UserDefinedTypeData::Lambda(self_data_ref) => {
                        match &*lambda_data.semantic_data.symbol_data.0 .0.as_ref().borrow() {
                            UserDefinedTypeData::Lambda(other_data_ref) => {
                                return self_data_ref.meta_data.is_eq(&other_data_ref.meta_data);
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

impl ToString for Lambda {
    fn to_string(&self) -> String {
        match &self.name {
            Some(name) => format!("{}", name),
            None => match &*self.semantic_data.symbol_data.0 .0.as_ref().borrow() {
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
