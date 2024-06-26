use super::core::{SymbolDataEntry, SymbolIndex};
use crate::scope::concrete::{
    MethodGenericsInstantiationContext, TurbofishTypes, TypeGenericsInstantiationContext,
};
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::helper::check_concrete_types_bounded_by_interfaces;
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory;
use crate::scope::symbol::types::generic_ty::GenericTypeParams;
use crate::scope::traits::IsInitialized;
use crate::scope::traits::{AbstractSymbol, InstantiationContext};
use crate::types::core::{Type, TypeStringifyContext};
use crate::{
    ast::ast::{ExpressionNode, SymbolSeparatedSequenceNode},
    core::common::RefOrOwned,
    parser::type_checker::{
        InferredConcreteTypesEntry, JarvilTypeChecker, PrototypeEquivalenceCheckError,
    },
    types::traits::TypeLike,
};
use std::vec;
use text_size::TextRange;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CallableKind {
    Function,
    Method,
    LambdaType,
}

#[derive(Debug, Clone)]
pub struct CallablePrototypeData {
    params: Vec<Type>,
    return_ty: Type,
}

impl CallablePrototypeData {
    pub fn new(params: Vec<Type>, return_ty: Type) -> CallablePrototypeData {
        CallablePrototypeData { params, return_ty }
    }

    pub fn params(&self) -> &Vec<Type> {
        &self.params
    }

    pub fn return_ty(&self) -> &Type {
        &self.return_ty
    }

    fn compare<F: Fn(&Type, &Type, TypeGenericsInstantiationContext, &Namespace) -> bool>(
        &self,
        other: &CallablePrototypeData,
        cmp_func: F,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        let (self_param_types, self_return_ty) = (&self.params, &self.return_ty);
        let (other_param_types, other_return_ty) = (&other.params, &other.return_ty);
        let self_params_len = self_param_types.len();
        let other_params_len = other_param_types.len();

        if self_params_len != other_params_len {
            return false;
        }

        if !cmp_func(self_return_ty, other_return_ty, context, namespace) {
            return false;
        }

        for index in 0..self_params_len {
            if !cmp_func(
                &self_param_types[index],
                &other_param_types[index],
                context,
                namespace,
            ) {
                return false;
            }
        }

        true
    }

    pub fn is_eq(&self, other: &CallablePrototypeData, namespace: &Namespace) -> bool {
        let cmp_func = |ty1: &Type,
                        ty2: &Type,
                        _context: TypeGenericsInstantiationContext,
                        namespace: &Namespace| ty1.is_eq(ty2, namespace);

        self.compare(
            other,
            cmp_func,
            TypeGenericsInstantiationContext::default(),
            namespace,
        )
    }

    pub fn is_structurally_eq(
        &self,
        other: &CallablePrototypeData,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        let cmp_func =
            |ty1: &Type,
             ty2: &Type,
             context: TypeGenericsInstantiationContext,
             namespace: &Namespace| { ty1.is_structurally_eq(ty2, context, namespace) };

        self.compare(other, cmp_func, context, namespace)
    }

    pub fn try_infer_ty(
        &self,
        other: &CallablePrototypeData,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&TurbofishTypes>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let (self_param_types, self_return_ty) = (&self.params, &self.return_ty);
        let (other_param_types, other_return_ty) = (&other.params, &other.return_ty);
        let self_params_len = self_param_types.len();
        let other_params_len = other_param_types.len();

        if self_params_len != other_params_len {
            return Err(());
        }

        self_return_ty.try_infer_ty_or_check_equivalence(
            other_return_ty,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )?;

        for index in 0..self_params_len {
            let _ = &self_param_types[index].try_infer_ty_or_check_equivalence(
                &other_param_types[index],
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            )?;
        }

        Ok(())
    }

    // Type-Checking exclusive method
    pub fn is_received_params_valid(
        &self,
        type_checker: &JarvilTypeChecker,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PrototypeEquivalenceCheckError> {
        let expected_params = &self.params;
        let return_ty = &self.return_ty;

        type_checker.check_params_ty_and_count(expected_params, received_params)?;

        Ok(return_ty.clone())
    }
}

impl Default for CallablePrototypeData {
    fn default() -> Self {
        CallablePrototypeData {
            params: vec![],
            return_ty: Type::new_with_unset(),
        }
    }
}

#[derive(Debug)]
pub struct CallableData {
    prototype: CallablePrototypeData,
    kind: CallableKind,
    generics: Option<GenericTypeParams>,
}

impl CallableData {
    pub fn new(
        params: Vec<Type>,
        return_ty: Type,
        kind: CallableKind,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        CallableData {
            prototype: CallablePrototypeData { params, return_ty },
            kind,
            generics: generics_spec,
        }
    }

    pub fn default_for_kind(kind: CallableKind) -> Self {
        CallableData {
            prototype: CallablePrototypeData::default(),
            kind,
            generics: Option::default(),
        }
    }

    pub fn set_meta_data(&mut self, params: Vec<Type>, return_ty: Type, kind: CallableKind) {
        self.prototype.params = params;
        self.prototype.return_ty = return_ty;
        self.kind = kind;
    }

    pub fn set_generics(&mut self, generics_spec: Option<GenericTypeParams>) {
        self.generics = generics_spec;
    }

    pub fn generics(&self) -> Option<&GenericTypeParams> {
        self.generics.as_ref()
    }

    pub fn structural_prototype(&self) -> &CallablePrototypeData {
        &self.prototype
    }

    pub fn concretized_return_ty(
        &self,
        namespace: &Namespace,
        context: MethodGenericsInstantiationContext,
    ) -> Type {
        self.prototype.return_ty().concretize(context, namespace)
    }

    pub fn concretized_prototype(
        &self,
        namespace: &Namespace,
        context: MethodGenericsInstantiationContext,
    ) -> RefOrOwned<'_, CallablePrototypeData> {
        if context.is_empty() {
            return RefOrOwned::Ref(&self.prototype);
        }
        let concrete_return_ty = self.concretized_return_ty(namespace, context);
        let concrete_params = self
            .prototype
            .params
            .iter()
            .map(|ty| ty.concretize(context, namespace))
            .collect();

        RefOrOwned::Owned(CallablePrototypeData::new(
            concrete_params,
            concrete_return_ty,
        ))
    }
}

impl IsInitialized for CallableData {
    fn is_initialized(&self) -> bool {
        true
    }
}

#[derive(Debug)]
pub enum PartialCallableDataPrototypeCheckError {
    PrototypeEquivalenceCheckFailed(PrototypeEquivalenceCheckError),
    GenericTypeArgsCheckFailed(GenericTypeArgsCheckError),
}

impl From<PrototypeEquivalenceCheckError> for PartialCallableDataPrototypeCheckError {
    fn from(value: PrototypeEquivalenceCheckError) -> Self {
        PartialCallableDataPrototypeCheckError::PrototypeEquivalenceCheckFailed(value)
    }
}

impl From<GenericTypeArgsCheckError> for PartialCallableDataPrototypeCheckError {
    fn from(value: GenericTypeArgsCheckError) -> Self {
        PartialCallableDataPrototypeCheckError::GenericTypeArgsCheckFailed(value)
    }
}

#[derive(Debug)]
pub struct PartialConcreteCallableDataRef<'a> {
    callable_data: &'a CallableData,
    context: TypeGenericsInstantiationContext<'a>,
}

impl<'a> PartialConcreteCallableDataRef<'a> {
    pub fn new(
        callable_data: &'a CallableData,
        context: TypeGenericsInstantiationContext<'a>,
    ) -> Self {
        PartialConcreteCallableDataRef {
            callable_data,
            context,
        }
    }

    // Type-Checking exclusive method
    pub fn is_received_params_valid(
        &self,
        type_checker: &JarvilTypeChecker,
        local_concrete_types: Option<TurbofishTypes>,
        local_concrete_ty_ranges: Option<Vec<TextRange>>,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PartialCallableDataPrototypeCheckError> {
        let generic_ty_decls = &self.callable_data.generics;

        match local_concrete_types {
            Some(local_concrete_types) => match generic_ty_decls {
                Some(generic_ty_decls) => {
                    generic_ty_decls.check_concrete_types_bounded_by(
                        &local_concrete_types,
                        match &local_concrete_ty_ranges {
                            Some(ty_ranges) => ty_ranges,
                            None => unreachable!(),
                        },
                        type_checker.err_logging_context(),
                    )?;

                    let context = self
                        .context
                        .attach_local_method_context(Some(&local_concrete_types));
                    let concrete_prototype = self
                        .callable_data
                        .concretized_prototype(type_checker.semantic_db().namespace_ref(), context);
                    let return_ty = concrete_prototype
                        .is_received_params_valid(type_checker, received_params)?;

                    Ok(return_ty)
                }
                None => Err(
                    PartialCallableDataPrototypeCheckError::GenericTypeArgsCheckFailed(
                        GenericTypeArgsCheckError::GenericTypeArgsNotExpected,
                    ),
                ),
            },
            None => match generic_ty_decls {
                Some(generic_ty_decls) => {
                    let local_concrete_types = type_checker.infer_concrete_types_from_arguments(
                        generic_ty_decls,
                        &self.callable_data.prototype,
                        self.context.ty_generics_instantiation_args(),
                        received_params,
                        GenericTypeDeclarationPlaceCategory::InCallable,
                    )?;
                    let context = self
                        .context
                        .attach_local_method_context(Some(&local_concrete_types));

                    Ok(self
                        .callable_data
                        .concretized_return_ty(type_checker.semantic_db().namespace_ref(), context))
                }
                None => {
                    let concrete_prototype = self.callable_data.concretized_prototype(
                        type_checker.semantic_db().namespace_ref(),
                        self.context.into_method_context(),
                    );
                    let return_ty = concrete_prototype
                        .is_received_params_valid(type_checker, received_params)?;

                    Ok(return_ty)
                }
            },
        }
    }
}

#[derive(Debug)]
pub struct FunctionSymbolData(SymbolIndex<CallableData>);

impl AbstractSymbol for FunctionSymbolData {
    type SymbolTy = CallableData;

    fn symbol_index(&self) -> SymbolIndex<Self::SymbolTy> {
        self.0
    }

    fn entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Function(self.0)
    }

    fn check_generic_ty_args(
        &self,
        concrete_types: Option<&TurbofishTypes>,
        ty_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        context: TypeStringifyContext,
    ) -> Result<(), GenericTypeArgsCheckError> {
        debug_assert!(is_concrete_types_none_allowed);

        let func_data = context
            .namespace()
            .funcs_ref()
            .symbol_ref(self.0)
            .data_ref();
        let generic_ty_decls = func_data.generics();

        check_concrete_types_bounded_by_interfaces(
            generic_ty_decls,
            concrete_types,
            ty_ranges,
            true,
            context,
        )
    }

    fn mangled_name(&self, namespace: &Namespace) -> MangledIdentifierName<CallableData> {
        self.0.mangled_name(namespace.funcs_ref())
    }
}

impl From<SymbolIndex<CallableData>> for FunctionSymbolData {
    fn from(value: SymbolIndex<CallableData>) -> Self {
        FunctionSymbolData(value)
    }
}
