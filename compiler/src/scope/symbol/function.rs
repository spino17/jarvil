use super::core::{SymbolDataEntry, SymbolIndex};
use crate::core::common::LongShortRef;
use crate::scope::concrete::{ConcreteTypesTuple, ConcretizationContext};
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::helper::check_concrete_types_bounded_by_interfaces;
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::scope::symbol::types::generic_type::GenericTypeParams;
use crate::scope::traits::AbstractSymbol;
use crate::types::core::{Type, TypeLongShortRef};
use crate::{
    ast::ast::{ExpressionNode, SymbolSeparatedSequenceNode},
    core::common::RefOrOwned,
    parser::type_checker::{
        InferredConcreteTypesEntry, PrototypeEquivalenceCheckError, TypeChecker,
    },
    types::traits::TypeLike,
};
use crate::{core::string_interner::Interner, scope::traits::IsInitialized};
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
    return_type: Type,
    is_concretization_required: Option<(Vec<usize>, bool)>, // (indexes of params that has generics, is_concretization required for return_type)
}

impl CallablePrototypeData {
    pub fn new(
        params: Vec<Type>,
        return_type: Type,
        is_concretization_required: Option<(Vec<usize>, bool)>,
    ) -> CallablePrototypeData {
        CallablePrototypeData {
            params,
            return_type,
            is_concretization_required,
        }
    }

    pub fn params(&self) -> &Vec<Type> {
        &self.params
    }

    pub fn return_ty(&self) -> &Type {
        &self.return_type
    }

    fn compare<F: Fn(&Type, &Type, &ConcretizationContext, &Namespace) -> bool>(
        &self,
        other: &CallablePrototypeData,
        cmp_func: F,
        context: &ConcretizationContext,
        namespace: &Namespace,
    ) -> bool {
        let (self_param_types, self_return_type) = (&self.params, &self.return_type);
        let (other_param_types, other_return_type) = (&other.params, &other.return_type);
        let self_params_len = self_param_types.len();
        let other_params_len = other_param_types.len();
        if self_params_len != other_params_len {
            return false;
        }
        if !cmp_func(self_return_type, other_return_type, context, namespace) {
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
                        _context: &ConcretizationContext,
                        namespace: &Namespace| ty1.is_eq(ty2, namespace);
        self.compare(
            other,
            cmp_func,
            &ConcretizationContext::default(),
            namespace,
        )
    }

    pub fn is_structurally_eq(
        &self,
        other: &CallablePrototypeData,
        context: &ConcretizationContext,
        namespace: &Namespace,
    ) -> bool {
        let cmp_func =
            |ty1: &Type, ty2: &Type, context: &ConcretizationContext, namespace: &Namespace| {
                ty1.is_structurally_eq(ty2, context, namespace)
            };
        self.compare(other, cmp_func, context, namespace)
    }

    pub fn try_infer_type(
        &self,
        other: &CallablePrototypeData,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let (self_param_types, self_return_type) = (&self.params, &self.return_type);
        let (other_param_types, other_return_type) = (&other.params, &other.return_type);
        let self_params_len = self_param_types.len();
        let other_params_len = other_param_types.len();
        if self_params_len != other_params_len {
            return Err(());
        }
        self_return_type.try_infer_type_or_check_equivalence(
            other_return_type,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )?;
        for index in 0..self_params_len {
            let _ = &self_param_types[index].try_infer_type_or_check_equivalence(
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
        type_checker: &TypeChecker,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PrototypeEquivalenceCheckError> {
        let expected_params = &self.params;
        let return_type = &self.return_type;
        type_checker.check_params_type_and_count(expected_params, received_params)?;
        Ok(return_type.clone())
    }
}

impl Default for CallablePrototypeData {
    fn default() -> Self {
        CallablePrototypeData {
            params: vec![],
            return_type: Type::new_with_unset(),
            is_concretization_required: None,
        }
    }
}

#[derive(Debug)]
pub struct CallablePrototypeRef<'a>(RefOrOwned<'a, CallablePrototypeData>);

impl<'a> CallablePrototypeRef<'a> {
    pub fn return_ty<'b>(&'b self) -> TypeLongShortRef<'a, 'b> {
        match &self.0 {
            RefOrOwned::Ref(prototype) => TypeLongShortRef::Long(&prototype.return_type),
            RefOrOwned::Owned(prototype) => TypeLongShortRef::Short(&prototype.return_type),
        }
    }

    pub fn params<'b>(&'b self) -> LongShortRef<'a, 'b, Vec<Type>> {
        match &self.0 {
            RefOrOwned::Ref(prototype) => LongShortRef::Long(&prototype.params),
            RefOrOwned::Owned(prototype) => LongShortRef::Short(&prototype.params),
        }
    }

    pub fn core_prototype(&self) -> &RefOrOwned<'a, CallablePrototypeData> {
        &self.0
    }

    pub fn is_received_params_valid<'b>(
        &'b self,
        type_checker: &TypeChecker,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<TypeLongShortRef<'a, 'b>, PrototypeEquivalenceCheckError> {
        type_checker.check_params_type_and_count(&self.params(), received_params)?;
        Ok(self.return_ty())
        /*
        match &self.0 {
            RefOrOwned::Ref(prototype) => {
                let expected_params = &prototype.params;
                let return_type = &prototype.return_type;
                type_checker.check_params_type_and_count(expected_params, received_params)?;
                Ok(TypeLongShortRef::Long(return_type))
            }
            RefOrOwned::Owned(prototype) => {
                let expected_params = &prototype.params;
                let return_type = &prototype.return_type;
                type_checker.check_params_type_and_count(expected_params, received_params)?;
                Ok(TypeLongShortRef::Short(return_type))
            }
        }*/
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
        return_type: Type,
        kind: CallableKind,
        is_concretization_required: Option<(Vec<usize>, bool)>,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        CallableData {
            prototype: CallablePrototypeData {
                params,
                return_type,
                is_concretization_required,
            },
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

    pub fn set_meta_data(
        &mut self,
        params: Vec<Type>,
        return_type: Type,
        kind: CallableKind,
        is_concretization_required: Option<(Vec<usize>, bool)>,
    ) {
        self.prototype.params = params;
        self.prototype.return_type = return_type;
        self.prototype.is_concretization_required = is_concretization_required;
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
        global_concrete_types: Option<&ConcreteTypesTuple>,
        local_concrete_types: Option<&ConcreteTypesTuple>,
        namespace: &Namespace,
    ) -> RefOrOwned<Type> {
        let unconcrete_return_ty = self.prototype.return_ty();
        if unconcrete_return_ty.is_concretization_required() {
            RefOrOwned::Owned(unconcrete_return_ty.concretize(
                &ConcretizationContext::new(global_concrete_types, local_concrete_types),
                namespace,
            ))
        } else {
            RefOrOwned::Ref(unconcrete_return_ty)
        }
    }

    pub fn concretized_prototype(
        &self,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        local_concrete_types: Option<&ConcreteTypesTuple>,
        namespace: &Namespace,
    ) -> RefOrOwned<'_, CallablePrototypeData> {
        if global_concrete_types.is_none() && local_concrete_types.is_none() {
            debug_assert!(self.prototype.is_concretization_required.is_none());
            return RefOrOwned::Ref(&self.prototype);
        }
        let context = ConcretizationContext::new(global_concrete_types, local_concrete_types);
        let Some((generics_containing_params_indexes, is_concretization_required_for_return_type)) =
            &self.prototype.is_concretization_required
        else {
            return RefOrOwned::Ref(&self.prototype);
        };
        let mut concrete_params = self.prototype.params.clone();
        let mut concrete_return_type = self.prototype.return_type.clone();
        for index in generics_containing_params_indexes {
            concrete_params[*index] = self.prototype.params[*index].concretize(&context, namespace);
        }
        if *is_concretization_required_for_return_type {
            concrete_return_type = self.prototype.return_type.concretize(&context, namespace);
        }
        RefOrOwned::Owned(CallablePrototypeData::new(
            concrete_params,
            concrete_return_type,
            None,
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
    concrete_types: Option<&'a ConcreteTypesTuple>,
}

impl<'a> PartialConcreteCallableDataRef<'a> {
    pub fn new(
        callable_data: &'a CallableData,
        concrete_types: Option<&'a ConcreteTypesTuple>,
    ) -> Self {
        PartialConcreteCallableDataRef {
            callable_data,
            concrete_types,
        }
    }

    // Type-Checking exclusive method
    pub fn is_received_params_valid(
        &self,
        type_checker: &TypeChecker,
        local_concrete_types: Option<ConcreteTypesTuple>,
        local_concrete_ty_ranges: Option<Vec<TextRange>>,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PartialCallableDataPrototypeCheckError> {
        let generic_type_decls = &self.callable_data.generics;
        match local_concrete_types {
            Some(local_concrete_types) => match generic_type_decls {
                Some(generic_type_decls) => {
                    generic_type_decls.check_concrete_types_bounded_by(
                        &local_concrete_types,
                        match &local_concrete_ty_ranges {
                            Some(type_ranges) => type_ranges,
                            None => unreachable!(),
                        },
                        type_checker.semantic_db().interner(),
                        type_checker.semantic_db().namespace_ref(),
                    )?;
                    let concrete_prototype = self.callable_data.concretized_prototype(
                        self.concrete_types,
                        Some(&local_concrete_types),
                        type_checker.semantic_db().namespace_ref(),
                    );
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
            None => match generic_type_decls {
                Some(generic_type_decls) => {
                    let local_concrete_types = type_checker.infer_concrete_types_from_arguments(
                        generic_type_decls,
                        &self.callable_data.prototype,
                        self.concrete_types,
                        received_params,
                        GenericTypeDeclarationPlaceCategory::InCallable,
                    )?;
                    let unconcrete_return_ty = &self.callable_data.prototype.return_type;
                    let concrete_return_ty = if unconcrete_return_ty.is_concretization_required() {
                        unconcrete_return_ty.concretize(
                            &ConcretizationContext::new(
                                self.concrete_types,
                                Some(&local_concrete_types),
                            ),
                            type_checker.semantic_db().namespace_ref(),
                        )
                    } else {
                        unconcrete_return_ty.clone()
                    };
                    Ok(concrete_return_ty)
                }
                None => {
                    let concrete_prototype = self.callable_data.concretized_prototype(
                        self.concrete_types,
                        None,
                        type_checker.semantic_db().namespace_ref(),
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

    fn symbol_index(&self) -> SymbolIndex<Self::SymbolTy>
    where
        <Self as AbstractSymbol>::SymbolTy: IsInitialized,
    {
        self.0
    }

    fn entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Function(self.0)
    }

    fn check_generic_type_args(
        &self,
        concrete_types: Option<&ConcreteTypesTuple>,
        type_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        interner: &Interner,
        namespace: &Namespace,
    ) -> Result<(), GenericTypeArgsCheckError> {
        debug_assert!(is_concrete_types_none_allowed);
        let function_data = namespace.functions_ref().symbol_ref(self.0).data_ref();
        let generic_type_decls = function_data.generics();
        check_concrete_types_bounded_by_interfaces(
            generic_type_decls,
            concrete_types,
            type_ranges,
            true,
            interner,
            namespace,
        )
    }

    fn mangled_name(&self, namespace: &Namespace) -> MangledIdentifierName<CallableData> {
        self.0.mangled_name(namespace.functions_ref())
    }
}

impl From<SymbolIndex<CallableData>> for FunctionSymbolData {
    fn from(value: SymbolIndex<CallableData>) -> Self {
        FunctionSymbolData(value)
    }
}
