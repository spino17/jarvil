use super::{
    concrete::{
        core::{ConcreteTypesRegistryKey, ConcreteTypesTuple, ConcretizationContext},
        registry::GenericsSpecAndConcreteTypesRegistry,
    },
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use std::vec;

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

#[derive(Debug)]
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

    pub fn concretize_prototype_core(
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
        concrete_types: &Vec<Type>,
    ) -> PrototypeConcretizationResult {
        return self
            .concretize_prototype_core(&ConcretizationContext::new(&vec![], concrete_types));
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
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.prototype.params = params;
        self.prototype.return_type = return_type;
        self.prototype.is_concretization_required = is_concretization_required;
        self.generics.generics_spec = generics_spec;
        self.kind = kind;
    }

    pub fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        return self
            .generics
            .concrete_types_registry
            .get_concrete_types_at_key(key);
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
}
