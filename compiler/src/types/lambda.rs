use super::core::OperatorCompatiblity;
use crate::ast::ast::{ExpressionNode, SymbolSeparatedSequenceNode};
use crate::core::string_interner::{Interner, StrId};
use crate::parser::type_checker::{
    InferredConcreteTypesEntry, PrototypeEquivalenceCheckError, TypeChecker,
};
use crate::scope::concrete::{ConcreteTypesTuple, ConcretizationContext};
use crate::scope::core::SymbolData;
use crate::scope::function::CallablePrototypeData;
use crate::scope::interfaces::InterfaceBounds;
use crate::scope::types::core::UserDefinedTypeData;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::{AbstractType, CoreType, Type};

#[derive(Debug)]
pub struct NamedLambdaCore {
    symbol_data: SymbolData<UserDefinedTypeData>,
    concrete_types: Option<ConcreteTypesTuple>,
}

#[derive(Debug)]
pub enum Lambda {
    Named(NamedLambdaCore),
    Unnamed(CallablePrototypeData),
}

impl Lambda {
    pub fn new_with_named(
        symbol_data: &SymbolData<UserDefinedTypeData>,
        concrete_types: Option<ConcreteTypesTuple>,
    ) -> Self {
        Lambda::Named(NamedLambdaCore {
            symbol_data: symbol_data.clone(),
            concrete_types,
        })
    }

    pub fn new_with_unnamed(func_prototype: CallablePrototypeData) -> Self {
        Lambda::Unnamed(func_prototype)
    }

    pub fn try_name(&self) -> Option<StrId> {
        match self {
            Lambda::Named(semantic_data) => Some(semantic_data.symbol_data.identifier_name()),
            Lambda::Unnamed(_) => None,
        }
    }

    // Type-Checking exclusive method
    pub fn is_received_params_valid(
        &self,
        type_checker: &mut TypeChecker,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PrototypeEquivalenceCheckError> {
        match self {
            Lambda::Named(semantic_data) => {
                let concrete_types = &semantic_data.concrete_types;
                let symbol_data = semantic_data.symbol_data.get_core_ref();
                let lambda_data = symbol_data.get_lambda_data_ref();
                let prototype_result = lambda_data.get_concrete_prototype(concrete_types.as_ref());
                let prototype_ref = prototype_result.get_prototype_ref();
                let expected_param_types = &prototype_ref.params;
                let return_type = &prototype_ref.return_type;
                type_checker.check_params_type_and_count(expected_param_types, received_params)?;
                Ok(return_type.clone())
            }
            Lambda::Unnamed(unnamed_lambda) => {
                let expected_param_types = &unnamed_lambda.params;
                let return_type = &unnamed_lambda.return_type;
                type_checker.check_params_type_and_count(expected_param_types, received_params)?;
                Ok(return_type.clone())
            }
        }
    }
}

impl AbstractType for Lambda {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Lambda(other_data) => {
                // Lambda type has structural equivalence unlike struct types which are only compared by it's name.
                match self {
                    Lambda::Named(self_named) => {
                        let self_symbol_data = self_named.symbol_data.get_core_ref();
                        let self_data = self_symbol_data.get_lambda_data_ref();
                        let self_concrete_types = &self_named.concrete_types;
                        let self_prototype_result =
                            self_data.get_concrete_prototype(self_concrete_types.as_ref());
                        let self_prototype_ref = self_prototype_result.get_prototype_ref();
                        match other_data {
                            Lambda::Named(other_named) => {
                                let other_symbol_data = other_named.symbol_data.get_core_ref();
                                let other_data = other_symbol_data.get_lambda_data_ref();
                                let other_concrete_types = &other_named.concrete_types;
                                let other_prototype_result = other_data
                                    .get_concrete_prototype(other_concrete_types.as_ref());
                                let other_prototype_ref =
                                    other_prototype_result.get_prototype_ref();
                                other_prototype_ref.is_eq(self_prototype_ref)
                            }
                            Lambda::Unnamed(other_prototype) => {
                                self_prototype_ref.is_eq(other_prototype)
                            }
                        }
                    }
                    Lambda::Unnamed(self_prototype) => match other_data {
                        Lambda::Named(other_named) => {
                            let other_symbol_data = other_named.symbol_data.get_core_ref();
                            let other_data = other_symbol_data.get_lambda_data_ref();
                            let other_concrete_types = &other_named.concrete_types;
                            let other_prototype_result =
                                other_data.get_concrete_prototype(other_concrete_types.as_ref());
                            let other_prototype_ref = other_prototype_result.get_prototype_ref();
                            other_prototype_ref.is_eq(self_prototype)
                        }
                        Lambda::Unnamed(other_prototype) => self_prototype.is_eq(other_prototype),
                    },
                }
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(&self, other_ty: &Type, context: &ConcretizationContext) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Lambda(other_data) => match self {
                Lambda::Named(self_concrete_symbol_data) => match other_data {
                    Lambda::Named(other_concrete_symbol_data) => {
                        if self_concrete_symbol_data.symbol_data.identifier_name()
                            == other_concrete_symbol_data.symbol_data.identifier_name()
                        {
                            match &self_concrete_symbol_data.concrete_types {
                                Some(self_concrete_types) => {
                                    let self_len = self_concrete_types.len();

                                    let other_concrete_types =
                                        match &other_concrete_symbol_data.concrete_types {
                                            Some(concrete_types) => concrete_types,
                                            None => unreachable!(),
                                        };
                                    let other_len = other_concrete_types.len();

                                    debug_assert!(self_len == other_len);
                                    for i in 0..self_len {
                                        if !self_concrete_types[i]
                                            .is_structurally_eq(&other_concrete_types[i], context)
                                        {
                                            return false;
                                        }
                                    }
                                    true
                                }
                                None => true,
                            }
                        } else {
                            false
                        }
                    }
                    Lambda::Unnamed(_) => unreachable!(),
                },
                Lambda::Unnamed(_) => unreachable!(),
            },
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        match self {
            Lambda::Named(concrete_symbol_data) => match &concrete_symbol_data.concrete_types {
                Some(concrete_types) => {
                    let mut concretized_concrete_types = vec![];
                    for ty in concrete_types.iter() {
                        concretized_concrete_types.push(ty.concretize(context));
                    }
                    Type::new_with_lambda_named(
                        &concrete_symbol_data.symbol_data,
                        Some(ConcreteTypesTuple::new(concretized_concrete_types)),
                    )
                }
                None => Type::new_with_lambda_named(&concrete_symbol_data.symbol_data, None),
            },
            Lambda::Unnamed(prototype) => Type::new_with_lambda_unnamed(prototype.clone()),
        }
    }

    fn is_type_bounded_by_interfaces(&self, _interface_bounds: &InterfaceBounds) -> bool {
        unreachable!()
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        match received_ty.0.as_ref() {
            CoreType::Lambda(lambda_ty) => match self {
                Lambda::Named(self_named) => {
                    let self_symbol_data = self_named.symbol_data.get_core_ref();
                    let self_data = self_symbol_data.get_lambda_data_ref();
                    let self_concrete_types = &self_named.concrete_types;
                    let self_prototype_result =
                        self_data.get_concrete_prototype(self_concrete_types.as_ref());
                    let self_prototype_ref = self_prototype_result.get_prototype_ref();
                    match lambda_ty {
                        Lambda::Named(other_named) => {
                            let other_symbol_data = other_named.symbol_data.get_core_ref();
                            let other_data = other_symbol_data.get_lambda_data_ref();
                            let other_concrete_types = &other_named.concrete_types;
                            let other_prototype_result =
                                other_data.get_concrete_prototype(other_concrete_types.as_ref());
                            let other_prototype_ref = other_prototype_result.get_prototype_ref();
                            self_prototype_ref.try_infer_type(
                                other_prototype_ref,
                                inferred_concrete_types,
                                global_concrete_types,
                                num_inferred_types,
                                inference_category,
                            )
                        }
                        Lambda::Unnamed(other_prototype) => self_prototype_ref.try_infer_type(
                            other_prototype,
                            inferred_concrete_types,
                            global_concrete_types,
                            num_inferred_types,
                            inference_category,
                        ),
                    }
                }
                Lambda::Unnamed(_) => unreachable!(),
            },
            _ => Err(()),
        }
    }

    fn to_string(&self, interner: &Interner) -> String {
        match self {
            Lambda::Named(semantic_data) => {
                let mut s = interner
                    .lookup(semantic_data.symbol_data.identifier_name())
                    .to_string();
                match &semantic_data.concrete_types {
                    Some(concrete_types) => {
                        s.push('<');
                        s.push_str(&concrete_types.to_string(interner));
                        s.push('>');
                        s
                    }
                    None => s,
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
                    params_str.push_str(&format!("{}", param.to_string(interner)));
                    flag = true;
                }
                format!(
                    "lambda({}) -> {}",
                    params_str,
                    self_return_type.to_string(interner)
                )
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
