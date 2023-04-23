use crate::scope::core::SymbolData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::types::core::{AbstractType, CoreType, Type};

use super::core::OperatorCompatiblity;

#[derive(Debug)]
pub struct Lambda {
    pub name: Option<String>,
    pub symbol_data: SymbolData<UserDefinedTypeData>,
}

impl Lambda {
    pub fn new(name: Option<String>, symbol_data: &SymbolData<UserDefinedTypeData>) -> Lambda {
        Lambda {
            name,
            symbol_data: symbol_data.clone(),
        }
    }
}

impl AbstractType for Lambda {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::LAMBDA(lambda_data) => {
                // Lambda type has structural equivalence checks unlike struct type which is only compared by it's name
                // This structural equivalence is important because we can have lambda types which are not named for example:
                // let x = (...) -> <Type>: block would have `x` to be of type `Lambda` with no name but symbol_data.
                let self_func_data = match &*self.symbol_data.0.as_ref().borrow() {
                    UserDefinedTypeData::LAMBDA(lambda_data) => lambda_data.func_data.clone(),
                    _ => unreachable!(
                        "lambda type should have reference to a lambda variant symbol entry"
                    ),
                };
                let base_func_data = match &*lambda_data.symbol_data.0.as_ref().borrow() {
                    UserDefinedTypeData::LAMBDA(lambda_data) => lambda_data.func_data.clone(),
                    _ => unreachable!(
                        "lambda type should have reference to a lambda variant symbol entry"
                    ),
                };
                let self_params_len = self_func_data.params.len();
                let base_params_len = base_func_data.params.len();
                if self_params_len != base_params_len {
                    return false;
                }
                let is_return_type_eq = self_func_data
                    .return_type
                    .is_eq(&base_func_data.return_type);
                if !is_return_type_eq {
                    return false;
                }
                for index in 0..self_params_len {
                    if !self_func_data.params.as_ref()[index]
                        .1
                        .is_eq(&base_func_data.params.as_ref()[index].1)
                    {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}

impl ToString for Lambda {
    fn to_string(&self) -> String {
        match &self.name {
            Some(name) => format!("{}", name),
            None => {
                let self_func_data = match &*self.symbol_data.0.as_ref().borrow() {
                    UserDefinedTypeData::LAMBDA(lambda_data) => lambda_data.func_data.clone(),
                    _ => unreachable!(
                        "lambda type should have reference to a lambda variant symbol entry"
                    ),
                };
                let mut params_str = String::from("");
                let mut flag = false;
                for param in self_func_data.params.as_ref() {
                    if flag {
                        params_str.push_str(", ")
                    }
                    params_str.push_str(&format!("{}", param.1.to_string()));
                    flag = true;
                }
                format!("lambda({}) -> {}", params_str, self_func_data.return_type)
            }
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
