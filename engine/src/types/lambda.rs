use std::rc::Rc;
use crate::scope::function::FunctionData;
use crate::types::core::{AbstractType, Type, CoreType};

#[derive(Debug)]
pub struct Lambda {
    pub name: Option<Rc<String>>,
    pub function_data: FunctionData,
}

impl AbstractType for Lambda {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::LAMBDA(lambda_data) => {
                let self_params_len = self.function_data.params.len();
                let base_params_len = lambda_data.function_data.params.len();
                if self_params_len != base_params_len {
                    return false
                }
                if (self.function_data.return_type.is_none() && lambda_data.function_data.return_type.is_some())
                || (self.function_data.return_type.is_some() && lambda_data.function_data.return_type.is_none()) {
                    return false
                }
                let is_return_type_eq = match self.function_data.return_type.as_ref() {
                    Some(self_return_type) => {
                        match lambda_data.function_data.return_type.as_ref() {
                            Some(base_return_type) => {
                                self_return_type.is_eq(base_return_type)
                            },
                            _ => unreachable!("both lambda types should have a return type")
                        }
                    },
                    None => {
                        match lambda_data.function_data.return_type.as_ref() {
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
                    if !self.function_data.params.as_ref()[index].1.is_eq(&lambda_data.function_data.params.as_ref()[index].1) {
                        return false
                    }
                }
                true
            },
            _ => false
        }
    }

    fn string(&self) -> Rc<String> {
        match &self.name {
            Some(name) => Rc::new(format!("{}", name)),
            None => {
                let mut params_str = String::from("");
                let mut flag = false;
                for param in self.function_data.params.as_ref() {
                    if flag {
                        params_str.push_str(",")
                    }
                    params_str.push_str(&format!("{}", param.1.string()));
                    flag = true;
                }
                let mut return_type_str = String::from("");
                match self.function_data.return_type.as_ref() {
                    Some(return_type) => return_type_str.push_str(&format!("{}", return_type.string())),
                    None => return_type_str.push_str("void"),
                }
                Rc::new(format!("func({}) -> ({})", params_str, return_type_str))
            }
        }
    }
}