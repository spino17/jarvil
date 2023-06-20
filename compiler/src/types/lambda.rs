use super::core::OperatorCompatiblity;
use crate::scope::function::FunctionData;
use crate::types::core::{AbstractType, CoreType, Type};

#[derive(Debug)]
pub struct Lambda {
    pub name: Option<String>,
    pub meta_data: FunctionData,
    pub generic_type_args: Vec<Type>,
}

impl Lambda {
    pub fn new(name: Option<String>, params: &Vec<Type>, return_type: &Type) -> Lambda {
        Lambda {
            name,
            meta_data: FunctionData {
                // NOTE: Below is traditionally an expensive clone but in our case,
                // mostly `params.len()` is less (that is number of arguments in a function definition)
                // so we avoid runtime overhead of using `Rc` which ideally should be used if length is large
                // for example: in `BlockNode`, see `stmts` field.
                params: params.clone(),
                return_type: return_type.clone(),
            },
            generic_type_args: vec![],
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
                let (self_param_types, self_return_type) =
                    (&self.meta_data.params, &self.meta_data.return_type);
                let (base_param_types, base_return_type) = (
                    &lambda_data.meta_data.params,
                    &lambda_data.meta_data.return_type,
                );
                let self_params_len = self_param_types.len();
                let base_params_len = base_param_types.len();
                if self_params_len != base_params_len {
                    return false;
                }
                if !self_return_type.is_eq(base_return_type) {
                    return false;
                }
                for index in 0..self_params_len {
                    if !self_param_types[index].is_eq(&base_param_types[index]) {
                        return false;
                    }
                }
                true
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn stringify(&self) -> String {
        match &self.name {
            Some(name) => {
                let mut s = name.to_string();
                let len = self.generic_type_args.len();
                if len > 0 {
                    s.push_str("_la_");
                    s.push_str(&self.generic_type_args[0].stringify());
                    for i in 1..len {
                        s.push_str("_comma_");
                        s.push_str(&self.generic_type_args[i].stringify());
                    }
                    s.push_str("_ra");
                }
                return s;
            }
            None => unreachable!(),
        }
    }
}

impl ToString for Lambda {
    fn to_string(&self) -> String {
        match &self.name {
            Some(name) => format!("{}", name),
            None => {
                let (self_param_types, self_return_type) =
                    (&self.meta_data.params, &self.meta_data.return_type);
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
