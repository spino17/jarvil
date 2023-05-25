use super::core::OperatorCompatiblity;
use crate::scope::core::SymbolData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::types::core::{AbstractType, CoreType, Type};

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
                let (self_param_types, self_return_type) =
                    match &*self.symbol_data.0.as_ref().borrow() {
                        UserDefinedTypeData::LAMBDA(lambda_data) => (
                            lambda_data.param_types.clone(),
                            lambda_data.return_type.clone(),
                        ),
                        _ => unreachable!(
                            "lambda type should have reference to a lambda variant symbol entry"
                        ),
                    };
                let (base_param_types, base_return_type) =
                    match &*lambda_data.symbol_data.0.as_ref().borrow() {
                        UserDefinedTypeData::LAMBDA(lambda_data) => (
                            lambda_data.param_types.clone(),
                            lambda_data.return_type.clone(),
                        ),
                        _ => unreachable!(
                            "lambda type should have reference to a lambda variant symbol entry"
                        ),
                    };
                let self_params_len = self_param_types.len();
                let base_params_len = base_param_types.len();
                if self_params_len != base_params_len {
                    return false;
                }
                if !self_return_type.is_eq(&base_return_type) {
                    return false;
                }
                for index in 0..self_params_len {
                    if !self_param_types.as_ref()[index].is_eq(&base_param_types.as_ref()[index]) {
                        return false;
                    }
                }
                true
            }
            CoreType::ANY => true,
            _ => false,
        }
    }
}

impl ToString for Lambda {
    fn to_string(&self) -> String {
        match &self.name {
            Some(name) => format!("{}", name),
            None => {
                let (self_param_types, self_return_type) =
                    match &*self.symbol_data.0.as_ref().borrow() {
                        UserDefinedTypeData::LAMBDA(lambda_data) => (
                            lambda_data.param_types.clone(),
                            lambda_data.return_type.clone(),
                        ),
                        _ => unreachable!(
                            "lambda type should have reference to a lambda variant symbol entry"
                        ),
                    };
                let mut params_str = "".to_string();
                let mut flag = false;
                for param in self_param_types.as_ref() {
                    if flag {
                        params_str.push_str(", ")
                    }
                    params_str.push_str(&format!("{}", param.to_string()));
                    flag = true;
                }
                format!("lambda({}) -> {}", params_str, self_return_type)
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
