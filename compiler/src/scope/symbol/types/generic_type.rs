use crate::scope::namespace::Namespace;
use crate::types::core::AbstractType;
use crate::{
    core::string_interner::{Interner, StrId},
    scope::{
        concrete::ConcreteTypesTuple, errors::GenericTypeArgsCheckError,
        symbol::interfaces::InterfaceBounds,
    },
    types::core::Type,
};
use text_size::TextRange;

pub enum GenericTypePropertyQueryResult<T> {
    Ok(T),
    AmbigiousPropertyResolution(Vec<String>),
    None,
}

#[derive(Debug)]
pub struct GenericTypeData {
    pub index: usize, // index in the sequence of all generic type params in declaration
    pub category: GenericTypeDeclarationPlaceCategory,
    pub interface_bounds: InterfaceBounds,
}

impl GenericTypeData {
    pub fn new(
        index: usize,
        category: GenericTypeDeclarationPlaceCategory,
        interface_bounds: InterfaceBounds,
    ) -> Self {
        GenericTypeData {
            index,
            category,
            interface_bounds,
        }
    }

    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn try_field(
        &self,
        field_name: &StrId,
        interner: &Interner,
        namespace: &Namespace,
    ) -> GenericTypePropertyQueryResult<(Type, TextRange)> {
        let mut property_containing_interface_objs: Vec<String> = vec![];
        let mut result: Option<(Type, TextRange)> = None;
        for (interface_obj, _) in &self.interface_bounds.interfaces {
            let concrete_symbol_index = &interface_obj.0.as_ref().1;
            let interface_data = &namespace
                .interfaces
                .get_symbol_ref(concrete_symbol_index.index)
                .data;
            let concrete_types = &concrete_symbol_index.concrete_types;
            match interface_data.try_field(field_name, concrete_types.as_ref(), namespace) {
                Some((ty, decl_range)) => {
                    property_containing_interface_objs
                        .push(interface_obj.to_string(interner, namespace));
                    if result.is_none() {
                        result = Some((ty, decl_range));
                    }
                }
                None => continue,
            }
        }
        if property_containing_interface_objs.len() > 1 {
            return GenericTypePropertyQueryResult::AmbigiousPropertyResolution(
                property_containing_interface_objs,
            );
        }
        match result {
            Some(val) => GenericTypePropertyQueryResult::Ok(val),
            None => GenericTypePropertyQueryResult::None,
        }
    }

    pub fn has_method(
        &self,
        method_name: &StrId,
        interner: &Interner,
        namespace: &Namespace,
    ) -> GenericTypePropertyQueryResult<usize> {
        let mut property_containing_interface_objs: Vec<String> = vec![];
        let mut result: Option<usize> = None;
        for (index, (interface_obj, _)) in self.interface_bounds.interfaces.iter().enumerate() {
            let concrete_symbol_index = &interface_obj.0.as_ref().1;
            let interface_data = &namespace
                .interfaces
                .get_symbol_ref(concrete_symbol_index.index)
                .data;
            if interface_data.has_method(method_name) {
                property_containing_interface_objs
                    .push(interface_obj.to_string(interner, namespace));
                if result.is_none() {
                    result = Some(index);
                }
            }
        }
        if property_containing_interface_objs.len() > 1 {
            return GenericTypePropertyQueryResult::AmbigiousPropertyResolution(
                property_containing_interface_objs,
            );
        }
        match result {
            Some(val) => GenericTypePropertyQueryResult::Ok(val),
            None => GenericTypePropertyQueryResult::None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum GenericTypeDeclarationPlaceCategory {
    InStruct,
    InCallable,
}

#[derive(Debug)]
pub struct GenericTypeParams(pub Vec<(StrId, InterfaceBounds, TextRange)>);

impl GenericTypeParams {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn check_concrete_types_bounded_by(
        &self,
        concrete_types: &ConcreteTypesTuple,
        type_ranges: &Vec<TextRange>,
        interner: &Interner,
        namespace: &Namespace,
    ) -> Result<(), GenericTypeArgsCheckError> {
        let expected_len = self.len();
        let received_len = concrete_types.len();
        if expected_len != received_len {
            return Err(GenericTypeArgsCheckError::GenericTypeArgsCountMismatched(
                received_len,
                expected_len,
            ));
        }
        let mut incorrectly_bounded_types: Vec<(TextRange, String)> = vec![];
        for (index, (_, interface_bounds, _)) in self.0.iter().enumerate() {
            let ty = &concrete_types[index];
            if !ty.is_type_bounded_by_interfaces(interface_bounds, namespace) {
                incorrectly_bounded_types.push((
                    type_ranges[index],
                    interface_bounds.to_string(interner, namespace),
                ))
            }
        }
        if !incorrectly_bounded_types.is_empty() {
            return Err(
                GenericTypeArgsCheckError::GenericTypeArgsIncorrectlyBounded(
                    incorrectly_bounded_types,
                ),
            );
        }
        Ok(())
    }
}
