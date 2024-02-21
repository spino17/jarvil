use super::core::OperatorCompatiblity;
use super::helper::UserDefinedType;
use crate::ast::ast::{ExpressionNode, SymbolSeparatedSequenceNode};
use crate::core::string_interner::{Interner, StrId};
use crate::parser::type_checker::{
    InferredConcreteTypesEntry, PrototypeEquivalenceCheckError, TypeChecker,
};
use crate::scope::concrete::{ConcreteTypesTuple, ConcretizationContext};
use crate::scope::namespace::Namespace;
use crate::scope::symbol::core::SymbolIndex;
use crate::scope::symbol::function::CallablePrototypeData;
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::core::UserDefinedTypeData;
use crate::scope::symbol::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::{AbstractType, CoreType, Type};
use crate::types::helper::user_defined_ty_compare_fn;

#[derive(Debug)]
pub struct NamedLambdaCore {
    symbol_index: SymbolIndex<UserDefinedTypeData>,
    concrete_types: Option<ConcreteTypesTuple>,
}

impl UserDefinedType for NamedLambdaCore {
    fn concrete_types(&self) -> Option<&ConcreteTypesTuple> {
        self.concrete_types.as_ref()
    }

    fn name(&self) -> StrId {
        self.symbol_index.identifier_name()
    }
}

#[derive(Debug)]
pub enum Lambda {
    Named(NamedLambdaCore),
    Unnamed(CallablePrototypeData),
}

impl Lambda {
    pub fn new_with_named(
        symbol_index: SymbolIndex<UserDefinedTypeData>,
        concrete_types: Option<ConcreteTypesTuple>,
    ) -> Self {
        Lambda::Named(NamedLambdaCore {
            symbol_index,
            concrete_types,
        })
    }

    pub fn new_with_unnamed(func_prototype: CallablePrototypeData) -> Self {
        Lambda::Unnamed(func_prototype)
    }

    pub fn try_name(&self) -> Option<StrId> {
        match self {
            Lambda::Named(named_lambda) => Some(named_lambda.symbol_index.identifier_name()),
            Lambda::Unnamed(_) => None,
        }
    }

    // Type-Checking exclusive method
    pub fn is_received_params_valid(
        &self,
        type_checker: &TypeChecker,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PrototypeEquivalenceCheckError> {
        match self {
            Lambda::Named(named_lambda) => {
                let concrete_types = &named_lambda.concrete_types;
                let ty_data = type_checker
                    .semantic_db()
                    .get_ty_symbol_ref(named_lambda.symbol_index);
                let lambda_data = ty_data.get_lambda_data_ref();
                let prototype_result = lambda_data.get_prototype(
                    concrete_types.as_ref(),
                    type_checker.semantic_db().namespace_ref(),
                );
                let expected_param_types = prototype_result.params();
                let return_type = prototype_result.return_ty();
                type_checker.check_params_type_and_count(expected_param_types, received_params)?;
                Ok(return_type.clone())
            }
            Lambda::Unnamed(unnamed_lambda) => {
                let expected_param_types = unnamed_lambda.params();
                let return_type = unnamed_lambda.return_ty();
                type_checker.check_params_type_and_count(expected_param_types, received_params)?;
                Ok(return_type.clone())
            }
        }
    }
}

impl AbstractType for Lambda {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Lambda(other_data) => {
                // Lambda type has structural equivalence unlike struct types which are only compared by it's name.
                match self {
                    Lambda::Named(self_named) => {
                        let self_ty_data = namespace
                            .types_ref()
                            .get_symbol_ref(self_named.symbol_index)
                            .data_ref();
                        let self_data = self_ty_data.get_lambda_data_ref();
                        let self_concrete_types = &self_named.concrete_types;
                        let self_prototype_result =
                            self_data.get_prototype(self_concrete_types.as_ref(), namespace);
                        match other_data {
                            Lambda::Named(other_named) => {
                                let other_ty_data = namespace
                                    .types_ref()
                                    .get_symbol_ref(other_named.symbol_index)
                                    .data_ref();
                                let other_data = other_ty_data.get_lambda_data_ref();
                                let other_concrete_types = &other_named.concrete_types;
                                let other_prototype_result = other_data
                                    .get_prototype(other_concrete_types.as_ref(), namespace);
                                other_prototype_result.is_eq(&self_prototype_result, namespace)
                            }
                            Lambda::Unnamed(other_prototype) => {
                                self_prototype_result.is_eq(other_prototype, namespace)
                            }
                        }
                    }
                    Lambda::Unnamed(self_prototype) => match other_data {
                        Lambda::Named(other_named) => {
                            let other_ty_data = namespace
                                .types_ref()
                                .get_symbol_ref(other_named.symbol_index)
                                .data_ref();
                            let other_data = other_ty_data.get_lambda_data_ref();
                            let other_concrete_types = &other_named.concrete_types;
                            let other_prototype_result =
                                other_data.get_prototype(other_concrete_types.as_ref(), namespace);
                            other_prototype_result.is_eq(self_prototype, namespace)
                        }
                        Lambda::Unnamed(other_prototype) => {
                            self_prototype.is_eq(other_prototype, namespace)
                        }
                    },
                }
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: &ConcretizationContext,
        namespace: &Namespace,
    ) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Lambda(other_data) => match self {
                Lambda::Named(self_named_lambda) => {
                    let Lambda::Named(other_named_lambda) = other_data else {
                        unreachable!()
                    };
                    let ty_cmp_func =
                        |ty1: &Type,
                         ty2: &Type,
                         context: &ConcretizationContext,
                         namespace: &Namespace| {
                            ty1.is_structurally_eq(ty2, context, namespace)
                        };
                    user_defined_ty_compare_fn(
                        self_named_lambda,
                        other_named_lambda,
                        ty_cmp_func,
                        context,
                        namespace,
                    )
                }
                Lambda::Unnamed(_) => unreachable!(),
            },
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext, namespace: &Namespace) -> Type {
        match self {
            Lambda::Named(named_lambda) => {
                let Some(concrete_types) = &named_lambda.concrete_types else {
                    return Type::new_with_lambda_named(named_lambda.symbol_index, None);
                };
                let mut concretized_concrete_types = vec![];
                for ty in concrete_types.iter() {
                    concretized_concrete_types.push(ty.concretize(context, namespace));
                }
                Type::new_with_lambda_named(
                    named_lambda.symbol_index,
                    Some(ConcreteTypesTuple::new(concretized_concrete_types)),
                )
            }
            Lambda::Unnamed(prototype) => Type::new_with_lambda_unnamed(prototype.clone()),
        }
    }

    fn is_type_bounded_by_interfaces(
        &self,
        _interface_bounds: &InterfaceBounds,
        namespace: &Namespace,
    ) -> bool {
        unreachable!()
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        match received_ty.0.as_ref() {
            CoreType::Lambda(lambda_ty) => match self {
                Lambda::Named(self_named) => {
                    let self_ty_data = namespace
                        .types_ref()
                        .get_symbol_ref(self_named.symbol_index)
                        .data_ref();
                    let self_data = self_ty_data.get_lambda_data_ref();
                    let self_concrete_types = &self_named.concrete_types;
                    let self_prototype_result =
                        self_data.get_prototype(self_concrete_types.as_ref(), namespace);
                    match lambda_ty {
                        Lambda::Named(other_named) => {
                            let other_ty_data = namespace
                                .types_ref()
                                .get_symbol_ref(other_named.symbol_index)
                                .data_ref();
                            let other_data = other_ty_data.get_lambda_data_ref();
                            let other_concrete_types = &other_named.concrete_types;
                            let other_prototype_result =
                                other_data.get_prototype(other_concrete_types.as_ref(), namespace);
                            self_prototype_result.try_infer_type(
                                &other_prototype_result,
                                inferred_concrete_types,
                                global_concrete_types,
                                num_inferred_types,
                                inference_category,
                                namespace,
                            )
                        }
                        Lambda::Unnamed(other_prototype) => self_prototype_result.try_infer_type(
                            other_prototype,
                            inferred_concrete_types,
                            global_concrete_types,
                            num_inferred_types,
                            inference_category,
                            namespace,
                        ),
                    }
                }
                Lambda::Unnamed(_) => unreachable!(),
            },
            _ => Err(()),
        }
    }

    fn to_string(&self, interner: &Interner, namespace: &Namespace) -> String {
        match self {
            Lambda::Named(named_lambda) => {
                let mut s = interner.lookup(named_lambda.symbol_index.identifier_name());
                match &named_lambda.concrete_types {
                    Some(concrete_types) => {
                        s.push('<');
                        s.push_str(&concrete_types.to_string(interner, namespace));
                        s.push('>');
                        s
                    }
                    None => s,
                }
            }
            Lambda::Unnamed(unnamed) => {
                let self_param_types = unnamed.params();
                let self_return_type = unnamed.return_ty();
                let mut params_str = "".to_string();
                let mut flag = false;
                for param in self_param_types {
                    if flag {
                        params_str.push_str(", ")
                    }
                    params_str.push_str(&param.to_string(interner, namespace));
                    flag = true;
                }
                format!(
                    "lambda({}) -> {}",
                    params_str,
                    self_return_type.to_string(interner, namespace)
                )
            }
        }
    }
}

impl OperatorCompatiblity for Lambda {
    fn check_add(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_subtract(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_multiply(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_divide(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_double_equal(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_greater(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_less(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_and(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }
}
