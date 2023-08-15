use super::{
    concrete::{
        core::{ConcreteTypesRegistryKey, ConcreteTypesTuple, ConcretizationContext},
        registry::{ConcreteTypesRegistryCore, GenericsSpecAndConcreteTypesRegistry},
    },
    core::{AbstractConcreteTypesHandler, AbstractSymbolMetaData, GenericTypeParams},
    errors::GenericTypeArgsCheckError,
};
use crate::{
    ast::ast::{ExpressionNode, SymbolSeparatedSequenceNode},
    parser::type_checker::{PrototypeEquivalenceCheckError, TypeChecker},
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

    pub fn is_eq(&self, other: &CallablePrototypeData) -> bool {
        let (self_param_types, self_return_type) = (&self.params, &self.return_type);
        let (other_param_types, other_return_type) = (&other.params, &other.return_type);
        let self_params_len = self_param_types.len();
        let other_params_len = other_param_types.len();
        if self_params_len != other_params_len {
            return false;
        }
        if !self_return_type.is_eq(&other_return_type) {
            return false;
        }
        for index in 0..self_params_len {
            if !self_param_types[index].is_eq(&other_param_types[index]) {
                return false;
            }
        }
        return true;
    }

    fn concretize_prototype_core(
        &self,
        context: &ConcretizationContext,
    ) -> PrototypeConcretizationResult {
        match &self.is_concretization_required {
            Some((
                generics_containing_params_indexes,
                is_concretization_required_for_return_type,
            )) => {
                let mut concrete_params = self.params.clone();
                let mut concrete_return_type = self.return_type.clone();
                for index in generics_containing_params_indexes {
                    concrete_params[*index] = self.params[*index].concretize(context);
                }
                if *is_concretization_required_for_return_type {
                    concrete_return_type = self.return_type.concretize(context);
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
    ) -> PrototypeConcretizationResult {
        return self.concretize_prototype_core(&ConcretizationContext::new(
            global_concrete_types,
            local_concrete_types,
        ));
    }

    // Type-Checking exclusive method
    pub fn is_received_params_valid(
        &self,
        type_checker: &TypeChecker,
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

    pub fn is_eq(&self, other: &CallableData) -> bool {
        todo!()
    }
}

impl AbstractConcreteTypesHandler for CallableData {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        has_generics: bool,
    ) -> ConcreteTypesRegistryKey {
        return self
            .generics
            .concrete_types_registry
            .register_concrete_types(concrete_types, has_generics);
    }

    fn is_generics_present_in_tuple_at_index(&self, index: ConcreteTypesRegistryKey) -> bool {
        self.generics
            .concrete_types_registry
            .get_concrete_types_at_key(index)
            .1
    }

    fn has_generics(&self) -> bool {
        self.generics.generics_spec.is_some()
    }

    fn is_initialized(&self) -> bool {
        true
    }
}

impl AbstractSymbolMetaData for CallableData {
    fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        return self
            .generics
            .concrete_types_registry
            .get_concrete_types_at_key(key);
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
        type_checker: &TypeChecker,
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
                    )?;
                    let concrete_prototype = self
                        .callable_data
                        .prototype
                        .concretize_prototype(self.concrete_types, Some(&local_concrete_types));
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
                    let (local_concrete_types, _) = type_checker
                        .infer_concrete_types_from_arguments(
                            generic_type_decls,
                            &self.callable_data.prototype,
                            self.concrete_types,
                            received_params,
                            GenericTypeDeclarationPlaceCategory::InCallable,
                        )?;
                    let unconcrete_return_ty = &self.callable_data.prototype.return_type;
                    let concrete_return_ty = if unconcrete_return_ty.has_generics() {
                        unconcrete_return_ty.concretize(&ConcretizationContext::new(
                            self.concrete_types,
                            Some(&local_concrete_types),
                        ))
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
