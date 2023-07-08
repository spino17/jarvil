use super::{
    concrete::core::ConcreteTypesRegistryKey,
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use std::vec;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CallableKind {
    Function,
    Method,
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
    pub is_concretization_required: bool,
}

impl CallablePrototypeData {
    pub fn new(
        params: Vec<Type>,
        return_type: Type,
        is_concretization_required: bool,
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

    pub fn concretize_method_prototype(
        &self,
        struct_concrete_types: &Vec<Type>,
        method_concrete_types: &Vec<Type>,
        kind: CallableKind,
    ) -> PrototypeConcretizationResult {
        assert!(kind == CallableKind::Method);
        if !self.is_concretization_required {
            return PrototypeConcretizationResult::UnConcretized(self);
        }
        todo!()
    }

    pub fn concretize_prototype(
        &self,
        concrete_types: &Vec<Type>,
    ) -> PrototypeConcretizationResult {
        if !self.is_concretization_required {
            return PrototypeConcretizationResult::UnConcretized(self);
        }
        todo!()
    }
}

impl Default for CallablePrototypeData {
    fn default() -> Self {
        CallablePrototypeData {
            params: vec![],
            return_type: Type::new_with_unset(),
            is_concretization_required: false,
        }
    }
}

#[derive(Debug)]
pub struct CallableData {
    pub prototype: CallablePrototypeData,
    pub kind: CallableKind,
    pub generics: Option<GenericTypeParams>,
}

impl CallableData {
    pub fn new(
        params: Vec<Type>,
        return_type: Type,
        kind: CallableKind,
        is_concretization_required: bool,
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
            generics: None,
        }
    }

    pub fn set_data(
        &mut self,
        params: Vec<Type>,
        return_type: Type,
        kind: CallableKind,
        is_concretization_required: bool,
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.prototype.params = params;
        self.prototype.return_type = return_type;
        self.prototype.is_concretization_required = is_concretization_required;
        self.generics = generics_spec;
        self.kind = kind;
    }
}

impl AbstractConcreteTypesHandler for CallableData {
    fn register_concrete_types(&mut self, _concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        unreachable!()
    }

    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}
