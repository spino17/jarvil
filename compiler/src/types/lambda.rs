use super::core::OperatorCompatiblity;
use crate::scope::concrete::core::{
    ConcreteSymbolData, ConcreteTypesRegistryKey, ConcretizationContext,
};
use crate::scope::core::SymbolData;
use crate::scope::function::{CallablePrototypeData, PrototypeConcretizationResult};
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
                // Lambda type has structural equivalence unlike struct types which are only compared by it's name.
                match self {
                    Lambda::Named((_, self_named)) => {
                        let self_symbol_data = self_named.get_core_ref();
                        let self_data = self_symbol_data.get_lambda_data_ref();
                        match self_data.get_concrete_prototype(self_named.index) {
                            PrototypeConcretizationResult::Concretized(self_concrete_prototype) => {
                                match other_data {
                                    Lambda::Named((_, other_named)) => {
                                        let other_symbol_data =
                                            other_named.symbol_data.get_core_ref();
                                        let other_data = other_symbol_data.get_lambda_data_ref();
                                        match other_data.get_concrete_prototype(other_named.index) {
                                            PrototypeConcretizationResult::Concretized(
                                                other_concrete_prototype,
                                            ) => {
                                                return other_concrete_prototype
                                                    .is_eq(&self_concrete_prototype)
                                            }
                                            PrototypeConcretizationResult::UnConcretized(
                                                other_prototype,
                                            ) => {
                                                return other_prototype
                                                    .is_eq(&self_concrete_prototype)
                                            }
                                        }
                                    }
                                    Lambda::Unnamed(other_prototype) => {
                                        self_concrete_prototype.is_eq(other_prototype)
                                    }
                                }
                            }
                            PrototypeConcretizationResult::UnConcretized(self_prototype) => {
                                match other_data {
                                    Lambda::Named((_, other_named)) => {
                                        let other_symbol_data =
                                            other_named.symbol_data.get_core_ref();
                                        let other_data = other_symbol_data.get_lambda_data_ref();
                                        match other_data.get_concrete_prototype(other_named.index) {
                                            PrototypeConcretizationResult::Concretized(
                                                other_concrete_prototype,
                                            ) => {
                                                return other_concrete_prototype
                                                    .is_eq(self_prototype)
                                            }
                                            PrototypeConcretizationResult::UnConcretized(
                                                other_prototype,
                                            ) => return other_prototype.is_eq(self_prototype),
                                        }
                                    }
                                    Lambda::Unnamed(other_prototype) => {
                                        return self_prototype.is_eq(other_prototype)
                                    }
                                }
                            }
                        }
                    }
                    Lambda::Unnamed(self_prototype) => match other_data {
                        Lambda::Named((_, other_named)) => {
                            let other_symbol_data = other_named.get_core_ref();
                            let other_data = other_symbol_data.get_lambda_data_ref();
                            match other_data.get_concrete_prototype(other_named.index) {
                                PrototypeConcretizationResult::Concretized(
                                    other_concrete_prototype,
                                ) => return other_concrete_prototype.is_eq(self_prototype),
                                PrototypeConcretizationResult::UnConcretized(other_prototype) => {
                                    return other_prototype.is_eq(self_prototype)
                                }
                            }
                        }
                        Lambda::Unnamed(other_prototype) => {
                            return self_prototype.is_eq(other_prototype)
                        }
                    },
                }
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        match self {
            Lambda::Named(named) => {
                let index = match named.1.index {
                    Some(key) => key,
                    None => unreachable!(),
                };
                assert!(named
                    .1
                    .symbol_data
                    .is_generics_present_in_tuple_at_index(Some(index)));
                let symbol_data = named.1.get_core_ref();
                let lambda_data = symbol_data.get_lambda_data_ref();
                let concrete_types = &lambda_data.get_concrete_types(index).0;
                let mut concretized_concrete_types = concrete_types.clone();
                for (index, ty) in concrete_types.iter().enumerate() {
                    if ty.has_generics() {
                        concretized_concrete_types[index] = ty.concretize(context);
                    }
                }
                let new_key = named
                    .1
                    .symbol_data
                    .register_concrete_types(Some(concretized_concrete_types), false);
                return Type::new_with_lambda_named(
                    named.0.to_string(),
                    &named.1.symbol_data,
                    new_key,
                    false,
                );
            }
            Lambda::Unnamed(_) => unreachable!(),
        }
    }
}

impl ToString for Lambda {
    fn to_string(&self) -> String {
        match self {
            Lambda::Named((name, semantic_data)) => {
                let mut s = name.to_string();
                match semantic_data.index {
                    Some(index) => {
                        s.push('<');
                        let symbol_data = semantic_data.get_core_ref();
                        let lambda_data = symbol_data.get_lambda_data_ref();
                        let concrete_types = lambda_data.get_concrete_types(index);
                        s.push_str(&concrete_types.to_string());
                        s.push('>');
                        return s;
                    }
                    None => return s,
                }
            }
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
