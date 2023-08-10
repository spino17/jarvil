use super::core::OperatorCompatiblity;
use super::helper::try_infer_types_from_tuple;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::core::{
    ConcreteSymbolData, ConcreteTypesRegistryKey, ConcretizationContext,
};
use crate::scope::core::AbstractSymbolMetaData;
use crate::scope::core::SymbolData;
use crate::scope::function::CallablePrototypeData;
use crate::scope::interfaces::InterfaceBounds;
use crate::scope::types::core::UserDefinedTypeData;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
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
                        let self_prototype_result =
                            self_data.get_concrete_prototype(self_named.index);
                        let self_prototype_ref = self_prototype_result.get_prototype_ref();
                        match other_data {
                            Lambda::Named((_, other_named)) => {
                                let other_symbol_data = other_named.symbol_data.get_core_ref();
                                let other_data = other_symbol_data.get_lambda_data_ref();
                                let other_prototype_result =
                                    other_data.get_concrete_prototype(other_named.index);
                                let other_prototype_ref =
                                    other_prototype_result.get_prototype_ref();
                                return other_prototype_ref.is_eq(self_prototype_ref);
                            }
                            Lambda::Unnamed(other_prototype) => {
                                return self_prototype_ref.is_eq(other_prototype)
                            }
                        }
                    }
                    Lambda::Unnamed(self_prototype) => match other_data {
                        Lambda::Named((_, other_named)) => {
                            let other_symbol_data = other_named.get_core_ref();
                            let other_data = other_symbol_data.get_lambda_data_ref();
                            let other_prototype_result =
                                other_data.get_concrete_prototype(other_named.index);
                            let other_prototype_ref = other_prototype_result.get_prototype_ref();
                            return other_prototype_ref.is_eq(self_prototype);
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
                );
            }
            Lambda::Unnamed(_) => unreachable!(),
        }
    }

    fn is_type_bounded_by_interfaces(&self, _interface_bounds: &InterfaceBounds) -> bool {
        unreachable!()
    }

    fn has_generics(&self) -> bool {
        match self {
            Lambda::Named((_, concrete_symbol_data)) => {
                let index = concrete_symbol_data.index;
                concrete_symbol_data
                    .symbol_data
                    .is_generics_present_in_tuple_at_index(index)
            }
            Lambda::Unnamed(_) => false,
        }
    }

    fn try_infer_type(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        num_inferred_types: &mut usize,
        generic_ty_decl_place: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        match received_ty.0.as_ref() {
            CoreType::Lambda(lambda_ty) => match self {
                Lambda::Named((self_name, self_concrete_symbol_data)) => match lambda_ty {
                    Lambda::Named((other_name, other_concrete_symbol_data)) => {
                        if self_name == other_name {
                            match self_concrete_symbol_data.index {
                                Some(self_index) => {
                                    let self_symbol_data =
                                        self_concrete_symbol_data.symbol_data.get_core_ref();
                                    let self_lambda_data = self_symbol_data.get_lambda_data_ref();
                                    let generics_containing_types_tuple =
                                        &self_lambda_data.get_concrete_types(self_index).0;

                                    let other_index = other_concrete_symbol_data.index.unwrap();
                                    let other_symbol_data =
                                        other_concrete_symbol_data.symbol_data.get_core_ref();
                                    let other_lambda_data = other_symbol_data.get_lambda_data_ref();
                                    let base_types_tuple =
                                        &other_lambda_data.get_concrete_types(other_index).0;
                                    try_infer_types_from_tuple(
                                        base_types_tuple,
                                        generics_containing_types_tuple,
                                        inferred_concrete_types,
                                        num_inferred_types,
                                        generic_ty_decl_place,
                                    )
                                }
                                None => return Ok(()),
                            }
                        } else {
                            return Err(());
                        }
                    }
                    Lambda::Unnamed(_) => unreachable!(),
                },
                Lambda::Unnamed(_) => unreachable!(),
            },
            _ => Err(()),
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
