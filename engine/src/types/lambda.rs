use crate::scope::core::SymbolData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::types::core::{AbstractType, Type, CoreType};

#[derive(Debug)]
pub struct Lambda {
    pub name: Option<String>,
    pub symbol_data: SymbolData<UserDefinedTypeData>,
}
impl AbstractType for Lambda {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::LAMBDA(lambda_data) => {
                // Lambda type has structural equivalence checks unlike struct type which is only compared by it's name
                // This structural equivalence is important because we can have lambda types which are not named for example:
                // let x = (...) -> <Type>: block would have `x` to be of type `Lambda` with no name but symbol_data.
                let self_func_data 
                = match self.symbol_data.0.as_ref().borrow().as_ref().expect("symbol_data should be set") {
                    UserDefinedTypeData::LAMBDA(lambda_data) => lambda_data.func_data.clone(),
                    _ => unreachable!("lambda type should have reference to a lambda variant symbol entry"),
                };
                let base_func_data 
                = match lambda_data.symbol_data.0.as_ref().borrow().as_ref().expect("symbol_data should be set") {
                    UserDefinedTypeData::LAMBDA(lambda_data) => lambda_data.func_data.clone(),
                    _ => unreachable!("lambda type should have reference to a lambda variant symbol entry"),
                };
                let self_params_len = self_func_data.params.len();
                let base_params_len = base_func_data.params.len();
                if self_params_len != base_params_len {
                    return false
                }
                if (self_func_data.return_type.is_none() && base_func_data.return_type.is_some())
                || (self_func_data.return_type.is_some() && base_func_data.return_type.is_none()) {
                    return false
                }
                let is_return_type_eq = match self_func_data.return_type.as_ref() {
                    Some(self_return_type) => {
                        match base_func_data.return_type.as_ref() {
                            Some(base_return_type) => {
                                self_return_type.is_eq(base_return_type)
                            },
                            _ => unreachable!("both lambda types should have a return type")
                        }
                    },
                    None => {
                        match base_func_data.return_type.as_ref() {
                            None => {
                                true
                            }
                            _ => unreachable!("both lambda types should not have return type"),
                        }
                    }
                };
                if !is_return_type_eq {
                    return false
                }
                for index in 0..self_params_len {
                    if !self_func_data.params.as_ref()[index].1.is_eq(&base_func_data.params.as_ref()[index].1) {
                        return false
                    }
                }
                true
            },
            _ => false
        }
    }
}
impl ToString for Lambda {
    fn to_string(&self) -> String {
        match &self.name {
            Some(name) => format!("{}", name),
            None => {
                let self_func_data 
                = match self.symbol_data.0.as_ref().borrow().as_ref().expect("symbol_data should be set") {
                    UserDefinedTypeData::LAMBDA(lambda_data) => lambda_data.func_data.clone(),
                    _ => unreachable!("lambda type should have reference to a lambda variant symbol entry"),
                };
                let mut params_str = String::from("");
                let mut flag = false;
                for param in self_func_data.params.as_ref() {
                    if flag {
                        params_str.push_str(",")
                    }
                    params_str.push_str(&format!("{}", param.1.to_string()));
                    flag = true;
                }
                let mut return_type_str = String::from("");
                match self_func_data.return_type.as_ref() {
                    Some(return_type) => return_type_str.push_str(&format!("{}", return_type.to_string())),
                    None => return_type_str.push_str("void"),
                }
                format!("func({}) -> ({})", params_str, return_type_str)
            }
        }
    }
}