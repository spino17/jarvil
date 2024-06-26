use crate::scope::concrete::TypeGenericsInstantiationContext;
use crate::types::core::TypeStringifyContext;
use crate::types::traits::TypeLike;
use crate::{
    core::string_interner::IdentName,
    scope::{
        concrete::TurbofishTypes, errors::GenericTypeArgsCheckError,
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
    index: usize, // index in the sequence of all generic type params in declaration
    category: GenericTypeDeclarationPlaceCategory,
    interface_bounds: InterfaceBounds,
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

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn category(&self) -> GenericTypeDeclarationPlaceCategory {
        self.category
    }

    pub fn interface_bounds(&self) -> &InterfaceBounds {
        &self.interface_bounds
    }

    pub fn try_field(
        &self,
        field_name: &IdentName,
        context: TypeStringifyContext,
    ) -> GenericTypePropertyQueryResult<(Type, TextRange)> {
        let mut property_containing_interface_objs: Vec<String> = vec![];
        let mut result: Option<(Type, TextRange)> = None;
        let namespace = context.namespace();

        for (interface_obj, _) in self.interface_bounds.iter() {
            let concrete_symbol_index = interface_obj.core_symbol();
            let interface_data = namespace
                .interfaces_ref()
                .symbol_ref(concrete_symbol_index.symbol_index())
                .data_ref();
            let instantiation_context =
                TypeGenericsInstantiationContext::new(concrete_symbol_index.concrete_types());

            match interface_data.try_field(field_name, namespace, instantiation_context) {
                Some((ty, decl_range)) => {
                    property_containing_interface_objs.push(interface_obj.to_string(context));

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
        method_name: &IdentName,
        context: TypeStringifyContext,
    ) -> GenericTypePropertyQueryResult<usize> {
        let mut property_containing_interface_objs: Vec<String> = vec![];
        let mut result: Option<usize> = None;
        let namespace = context.namespace();

        for (index, (interface_obj, _)) in self.interface_bounds.iter().enumerate() {
            let concrete_symbol_index = interface_obj.core_symbol();
            let interface_data = namespace
                .interfaces_ref()
                .symbol_ref(concrete_symbol_index.symbol_index())
                .data_ref();

            if interface_data.has_method(method_name) {
                property_containing_interface_objs.push(interface_obj.to_string(context));

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
    InType,
    InCallable,
}

#[derive(Debug)]
pub struct GenericTypeParams(Vec<(IdentName, InterfaceBounds, TextRange)>);

impl GenericTypeParams {
    pub fn new(generic_ty_params_vec: Vec<(IdentName, InterfaceBounds, TextRange)>) -> Self {
        GenericTypeParams(generic_ty_params_vec)
    }

    pub fn interface_bounds(&self, index: usize) -> &InterfaceBounds {
        &self.0[index].1
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn check_concrete_types_bounded_by(
        &self,
        concrete_types: &TurbofishTypes,
        ty_ranges: &Vec<TextRange>,
        context: TypeStringifyContext,
    ) -> Result<(), GenericTypeArgsCheckError> {
        let expected_len = self.len();
        let received_len = concrete_types.len();

        if expected_len != received_len {
            return Err(GenericTypeArgsCheckError::GenericTypeArgsCountMismatched(
                received_len,
                expected_len,
            ));
        }

        let namespace = context.namespace();
        let mut incorrectly_bounded_types: Vec<(TextRange, String)> = vec![];

        for (index, (_, interface_bounds, _)) in self.0.iter().enumerate() {
            let ty = &concrete_types[index];

            if !ty.is_ty_bounded_by_interfaces(interface_bounds, namespace) {
                incorrectly_bounded_types
                    .push((ty_ranges[index], interface_bounds.to_string(context)))
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
