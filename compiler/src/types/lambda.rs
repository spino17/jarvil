use super::core::TypeStringifyContext;
use super::traits::OperatorCompatiblity;
use super::traits::TypeLike;
use super::traits::UserDefinedType;
use crate::ast::ast::{ExpressionNode, SymbolSeparatedSequenceNode};
use crate::core::string_interner::StrId;
use crate::parser::type_checker::{
    InferredConcreteTypesEntry, JarvilTypeChecker, PrototypeEquivalenceCheckError,
};
use crate::scope::concrete::TurbofishTypes;
use crate::scope::concrete::TypeGenericsInstantiationContext;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::core::SymbolIndex;
use crate::scope::symbol::function::CallablePrototypeData;
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::core::UserDefinedTypeData;
use crate::scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory;
use crate::scope::traits::InstantiationContext;
use crate::types::core::{CoreType, Type};
use crate::types::helper::user_defined_ty_compare_fn;

#[derive(Debug)]
pub struct NamedLambdaCore {
    symbol_index: SymbolIndex<UserDefinedTypeData>,
    concrete_types: Option<TurbofishTypes>,
}

impl UserDefinedType for NamedLambdaCore {
    fn symbol_index(&self) -> SymbolIndex<UserDefinedTypeData> {
        self.symbol_index
    }

    fn concrete_types(&self) -> Option<&TurbofishTypes> {
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
        concrete_types: Option<TurbofishTypes>,
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
        type_checker: &JarvilTypeChecker,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PrototypeEquivalenceCheckError> {
        match self {
            Lambda::Named(named_lambda) => {
                let concrete_types = &named_lambda.concrete_types;
                let ty_data = type_checker
                    .semantic_db()
                    .ty_symbol_ref(named_lambda.symbol_index);
                let lambda_data = ty_data.lambda_data_ref();
                let context = TypeGenericsInstantiationContext::new(concrete_types.as_ref());
                let prototype_result =
                    lambda_data.prototype(type_checker.semantic_db().namespace_ref(), context);
                let expected_param_types = prototype_result.params();
                let return_ty = prototype_result.return_ty();
                type_checker.check_params_ty_and_count(expected_param_types, received_params)?;
                Ok(return_ty.clone())
            }
            Lambda::Unnamed(unnamed_lambda) => {
                let expected_param_types = unnamed_lambda.params();
                let return_ty = unnamed_lambda.return_ty();
                type_checker.check_params_ty_and_count(expected_param_types, received_params)?;
                Ok(return_ty.clone())
            }
        }
    }
}

impl TypeLike for Lambda {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        let CoreType::Lambda(other_data) = other_ty.core_ty() else {
            return false;
        };
        // Lambda type has structural equivalence unlike struct types which are only compared by it's name.
        match self {
            Lambda::Named(self_named) => {
                let self_ty_data = namespace
                    .types_ref()
                    .symbol_ref(self_named.symbol_index)
                    .data_ref();
                let self_data = self_ty_data.lambda_data_ref();
                let self_concrete_types = &self_named.concrete_types;
                let self_context =
                    TypeGenericsInstantiationContext::new(self_concrete_types.as_ref());
                let self_prototype_result = self_data.prototype(namespace, self_context);
                match other_data {
                    Lambda::Named(other_named) => {
                        let other_ty_data = namespace
                            .types_ref()
                            .symbol_ref(other_named.symbol_index)
                            .data_ref();
                        let other_data = other_ty_data.lambda_data_ref();
                        let other_concrete_types = &other_named.concrete_types;
                        let other_context =
                            TypeGenericsInstantiationContext::new(other_concrete_types.as_ref());
                        let other_prototype_result = other_data.prototype(namespace, other_context);
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
                        .symbol_ref(other_named.symbol_index)
                        .data_ref();
                    let other_data = other_ty_data.lambda_data_ref();
                    let other_concrete_types = &other_named.concrete_types;
                    let other_context =
                        TypeGenericsInstantiationContext::new(other_concrete_types.as_ref());
                    let other_prototype_result = other_data.prototype(namespace, other_context);
                    other_prototype_result.is_eq(self_prototype, namespace)
                }
                Lambda::Unnamed(other_prototype) => {
                    self_prototype.is_eq(other_prototype, namespace)
                }
            },
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        match other_ty.core_ty() {
            CoreType::Lambda(other_data) => match self {
                Lambda::Named(self_named_lambda) => {
                    let Lambda::Named(other_named_lambda) = other_data else {
                        unreachable!()
                    };
                    let ty_cmp_func =
                        |ty1: &Type,
                         ty2: &Type,
                         context: TypeGenericsInstantiationContext,
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

    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type {
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
                    Some(TurbofishTypes::new(concretized_concrete_types)),
                )
            }
            Lambda::Unnamed(prototype) => Type::new_with_lambda_unnamed(prototype.clone()),
        }
    }

    fn is_ty_bounded_by_interfaces(
        &self,
        _interface_bounds: &InterfaceBounds,
        _namespace: &Namespace,
    ) -> bool {
        unreachable!()
    }

    fn try_infer_ty_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&TurbofishTypes>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let CoreType::Lambda(lambda_ty) = received_ty.core_ty() else {
            return Err(());
        };
        let Lambda::Named(self_named) = self else {
            unreachable!()
        };
        let self_ty_data = namespace
            .types_ref()
            .symbol_ref(self_named.symbol_index)
            .data_ref();
        let self_data = self_ty_data.lambda_data_ref();
        let self_concrete_types = &self_named.concrete_types;
        let self_context = TypeGenericsInstantiationContext::new(self_concrete_types.as_ref());
        let self_prototype_result = self_data.prototype(namespace, self_context);
        match lambda_ty {
            Lambda::Named(other_named) => {
                let other_ty_data = namespace
                    .types_ref()
                    .symbol_ref(other_named.symbol_index)
                    .data_ref();
                let other_data = other_ty_data.lambda_data_ref();
                let other_concrete_types = &other_named.concrete_types;
                let other_context =
                    TypeGenericsInstantiationContext::new(other_concrete_types.as_ref());
                let other_prototype_result = other_data.prototype(namespace, other_context);
                self_prototype_result.try_infer_ty(
                    &other_prototype_result,
                    inferred_concrete_types,
                    global_concrete_types,
                    num_inferred_types,
                    inference_category,
                    namespace,
                )
            }
            Lambda::Unnamed(other_prototype) => self_prototype_result.try_infer_ty(
                other_prototype,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            ),
        }
    }

    fn to_string(&self, context: TypeStringifyContext) -> String {
        match self {
            Lambda::Named(named_lambda) => {
                let mut s = context
                    .interner()
                    .lookup(named_lambda.symbol_index.identifier_name());
                match &named_lambda.concrete_types {
                    Some(concrete_types) => {
                        s.push('<');
                        s.push_str(&concrete_types.to_string(context));
                        s.push('>');
                        s
                    }
                    None => s,
                }
            }
            Lambda::Unnamed(unnamed) => {
                let self_param_types = unnamed.params();
                let self_return_ty = unnamed.return_ty();
                let mut params_str = "".to_string();
                let mut flag = false;
                for param in self_param_types {
                    if flag {
                        params_str.push_str(", ")
                    }
                    params_str.push_str(&param.to_string(context));
                    flag = true;
                }
                format!(
                    "lambda({}) -> {}",
                    params_str,
                    self_return_ty.to_string(context)
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
