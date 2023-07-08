use super::core::OperatorCompatiblity;
use crate::scope::concrete::core::{ConcreteSymbolData, ConcreteTypesRegistryKey};
use crate::scope::core::SymbolData;
use crate::scope::function::CallablePrototypeData;
use crate::scope::types::core::UserDefinedTypeData;
use crate::types::core::{AbstractType, CoreType, Type};

#[derive(Debug)]
pub enum Lambda {
    Named((String, ConcreteSymbolData<UserDefinedTypeData>)), // (name, semantic data)
    Unnamed(CallablePrototypeData),
}

impl Lambda {
    pub fn new_with_named(
        name: String,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        index: Option<ConcreteTypesRegistryKey>,
    ) -> Self {
        Lambda::Named((
            name,
            ConcreteSymbolData {
                symbol_data: symbol_data.clone(),
                index,
            },
        ))
    }

    pub fn new_with_unnamed(func_prototype: CallablePrototypeData) -> Self {
        Lambda::Unnamed(func_prototype)
    }
}

impl AbstractType for Lambda {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Lambda(other_data) => {
                // Lambda type has structural equivalence checks unlike struct type which is only compared by it's name
                // This structural equivalence is important because we can have lambda types which are not named for example:
                // let x = (...) -> <Type>: block would have `x` to be of type `Lambda` with no name but symbol_data.

                // TODO - once generics gets integrated we have concrete types attached to the lambda type
                // to enable structural equivalence of lambda type we have to get the concretized version of
                // function prototype which then we would compare like non-generic lambda types.
                /*
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
                }*/
                match self {
                    Lambda::Named((_, self_named)) => match other_data {
                        Lambda::Named((_, other_named)) => todo!(),
                        Lambda::Unnamed(other_unnamed) => todo!(),
                    },
                    Lambda::Unnamed(self_unnamed) => match other_data {
                        Lambda::Named((_, other_named)) => todo!(),
                        Lambda::Unnamed(other_unnamed) => todo!(),
                    },
                }
            }
            CoreType::Any => true,
            _ => false,
        }
    }
}

impl ToString for Lambda {
    fn to_string(&self) -> String {
        match self {
            Lambda::Named((name, _)) => name.to_string(),
            Lambda::Unnamed(unnamed) => {
                let self_param_types = &unnamed.params;
                let self_return_type = &unnamed.return_type;
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
