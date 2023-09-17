use super::{
    concrete::{
        core::{ConcreteTypesRegistryKey, ConcretizationContext},
        registry::{ConcreteTypesRegistryCore, GenericsSpecAndConcreteTypesRegistry},
    },
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
    errors::GenericTypeArgsCheckError,
    handler::SymbolDataRegistryTable,
    types::core::UserDefinedTypeData,
};
use crate::{
    ast::ast::{ExpressionNode, SymbolSeparatedSequenceNode},
    parser::type_checker::{
        InferredConcreteTypesEntry, PrototypeEquivalenceCheckError, TypeChecker,
    },
    types::core::AbstractType,
};
use crate::{scope::types::generic_type::GenericTypeDeclarationPlaceCategory, types::core::Type};
use std::vec;
use text_size::TextRange;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CallableKind {
    Function,
    Method,
    LambdaType,
}

#[derive(Debug)]
pub enum PrototypeConcretizationResult<'a> {
    UnConcretized(&'a CallablePrototypeData),
    Concretized(CallablePrototypeData),
}

impl<'a> PrototypeConcretizationResult<'a> {
    pub fn get_prototype_ref(&'a self) -> &'a CallablePrototypeData {
        match self {
            PrototypeConcretizationResult::UnConcretized(prototype) => prototype,
            PrototypeConcretizationResult::Concretized(prototype) => prototype,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallablePrototypeData {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub is_concretization_required: Option<(Vec<usize>, bool)>, // (indexes of params that has generics, is_concretization required for return_type)
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

    fn compare<
        F: Fn(
            &Type,
            &Type,
            &mut SymbolDataRegistryTable<UserDefinedTypeData>,
            &ConcretizationContext,
        ) -> bool,
    >(
        &self,
        other: &CallablePrototypeData,
        cmp_func: F,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
        context: &ConcretizationContext,
    ) -> bool {
        let (self_param_types, self_return_type) = (&self.params, &self.return_type);
        let (other_param_types, other_return_type) = (&other.params, &other.return_type);
        let self_params_len = self_param_types.len();
        let other_params_len = other_param_types.len();
        if self_params_len != other_params_len {
            return false;
        }
        if !cmp_func(&self_return_type, &other_return_type, registry, context) {
            return false;
        }
        for index in 0..self_params_len {
            if !cmp_func(
                &self_param_types[index],
                &other_param_types[index],
                registry,
                context,
            ) {
                return false;
            }
        }
        return true;
    }

    pub fn is_eq(
        &self,
        other: &CallablePrototypeData,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> bool {
        let cmp_func =
            |ty1: &Type,
             ty2: &Type,
             registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
             _context: &ConcretizationContext| ty1.is_eq(ty2, registry);
        self.compare(other, cmp_func, registry, &ConcretizationContext::default())
    }

    pub fn is_structurally_eq(
        &self,
        other: &CallablePrototypeData,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
        context: &ConcretizationContext,
    ) -> bool {
        let cmp_func = |ty1: &Type,
                        ty2: &Type,
                        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
                        context: &ConcretizationContext| {
            ty1.is_structurally_eq(ty2, registry, context)
        };
        self.compare(other, cmp_func, registry, context)
    }

    pub fn try_infer_type(
        &self,
        other: &CallablePrototypeData,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&Vec<Type>>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Result<(), ()> {
        let (self_param_types, self_return_type) = (&self.params, &self.return_type);
        let (other_param_types, other_return_type) = (&other.params, &other.return_type);
        let self_params_len = self_param_types.len();
        let other_params_len = other_param_types.len();
        if self_params_len != other_params_len {
            return Err(());
        }
        let _ = self_return_type.try_infer_type_or_check_equivalence(
            other_return_type,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            registry,
        )?;
        for index in 0..self_params_len {
            let _ = &self_param_types[index].try_infer_type_or_check_equivalence(
                &other_param_types[index],
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                registry,
            )?;
        }
        Ok(())
    }

    fn concretize_prototype_core(
        &self,
        context: &ConcretizationContext,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> PrototypeConcretizationResult {
        match &self.is_concretization_required {
            Some((
                generics_containing_params_indexes,
                is_concretization_required_for_return_type,
            )) => {
                let mut concrete_params = self.params.clone();
                let mut concrete_return_type = self.return_type.clone();
                for index in generics_containing_params_indexes {
                    concrete_params[*index] = self.params[*index].concretize(context, registry);
                }
                if *is_concretization_required_for_return_type {
                    concrete_return_type = self.return_type.concretize(context, registry);
                }
                return PrototypeConcretizationResult::Concretized(CallablePrototypeData::new(
                    concrete_params,
                    concrete_return_type,
                    None,
                ));
            }
            None => return PrototypeConcretizationResult::UnConcretized(self),
        }
    }

    pub fn concretize_prototype(
        &self,
        global_concrete_types: Option<&Vec<Type>>,
        local_concrete_types: Option<&Vec<Type>>,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> PrototypeConcretizationResult {
        return self.concretize_prototype_core(
            &ConcretizationContext::new(global_concrete_types, local_concrete_types),
            registry,
        );
    }

    // Type-Checking exclusive method
    pub fn is_received_params_valid(
        &self,
        type_checker: &mut TypeChecker,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PrototypeEquivalenceCheckError> {
        let expected_params = &self.params;
        let return_type = &self.return_type;
        let _ = type_checker.check_params_type_and_count(expected_params, received_params)?;
        return Ok(return_type.clone());
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
pub struct CallableData {
    pub prototype: CallablePrototypeData,
    pub kind: CallableKind,
    pub generics: GenericsSpecAndConcreteTypesRegistry,
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
            generics: GenericsSpecAndConcreteTypesRegistry::new(generics_spec),
        }
    }

    pub fn default_for_kind(kind: CallableKind) -> Self {
        CallableData {
            prototype: CallablePrototypeData::default(),
            kind,
            generics: GenericsSpecAndConcreteTypesRegistry::default(),
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
        self.generics.generics_spec = generics_spec;
    }
}

impl AbstractConcreteTypesHandler for CallableData {
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
    concrete_types: Option<&'a Vec<Type>>,
}

impl<'a> PartialConcreteCallableDataRef<'a> {
    pub fn new(callable_data: &'a CallableData, concrete_types: Option<&'a Vec<Type>>) -> Self {
        PartialConcreteCallableDataRef {
            callable_data,
            concrete_types,
        }
    }

    pub fn get_from_registry_key(
        callable_data: &'a CallableData,
        registry: &'a ConcreteTypesRegistryCore,
        key: Option<ConcreteTypesRegistryKey>,
    ) -> PartialConcreteCallableDataRef<'a> {
        let concrete_types = match key {
            Some(key) => Some(&registry.get_concrete_types_at_key(key).0),
            None => None,
        };
        return PartialConcreteCallableDataRef::new(callable_data, concrete_types);
    }

    // Type-Checking exclusive method
    pub fn is_received_params_valid(
        &self,
        type_checker: &mut TypeChecker,
        local_concrete_types: Option<Vec<Type>>,
        local_concrete_ty_ranges: Option<Vec<TextRange>>,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, PartialCallableDataPrototypeCheckError> {
        let generic_type_decls = &self.callable_data.generics.generics_spec;
        match local_concrete_types {
            Some(local_concrete_types) => match generic_type_decls {
                Some(generic_type_decls) => {
                    let _ = generic_type_decls.check_concrete_types_bounded_by(
                        &local_concrete_types,
                        match &local_concrete_ty_ranges {
                            Some(type_ranges) => type_ranges,
                            None => unreachable!(),
                        },
                        &mut type_checker.semantic_state_db,
                    )?;
                    let concrete_prototype = self.callable_data.prototype.concretize_prototype(
                        self.concrete_types,
                        Some(&local_concrete_types),
                        &mut type_checker.semantic_state_db.type_registry_table,
                    );
                    let prototype_ref = concrete_prototype.get_prototype_ref();
                    let return_ty =
                        prototype_ref.is_received_params_valid(type_checker, received_params)?;
                    return Ok(return_ty);
                }
                None => {
                    return Err(
                        PartialCallableDataPrototypeCheckError::GenericTypeArgsCheckFailed(
                            GenericTypeArgsCheckError::GenericTypeArgsNotExpected,
                        ),
                    )
                }
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
                            &mut type_checker.semantic_state_db.type_registry_table,
                        )
                    } else {
                        unconcrete_return_ty.clone()
                    };
                    return Ok(concrete_return_ty);
                }
                None => {
                    let return_ty = self
                        .callable_data
                        .prototype
                        .is_received_params_valid(type_checker, received_params)?;
                    return Ok(return_ty);
                }
            },
        }
    }
}
