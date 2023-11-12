use text_size::TextRange;

use crate::{
    core::string_interner::{Interner, StrId},
    scope::interfaces::InterfaceBounds,
    types::core::Type,
};

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
        interner: &mut Interner,
    ) -> GenericTypePropertyQueryResult<(Type, TextRange)> {
        let mut property_containing_interface_objs: Vec<String> = vec![];
        let mut result: Option<(Type, TextRange)> = None;
        for (interface_obj, _) in &self.interface_bounds.interfaces {
            let concrete_symbol_data = &interface_obj.0.as_ref().1;
            let interface_data = &*concrete_symbol_data.symbol_data.get_core_ref();
            let concrete_types = &concrete_symbol_data.concrete_types;
            match interface_data.try_field(field_name, concrete_types.as_ref()) {
                Some((ty, decl_range)) => {
                    property_containing_interface_objs.push(interface_obj.to_string(interner));
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

    pub fn try_method(
        &self,
        method_name: &StrId,
        interner: &mut Interner,
    ) -> GenericTypePropertyQueryResult<usize> {
        let mut property_containing_interface_objs: Vec<String> = vec![];
        let mut result: Option<usize> = None;
        for (index, (interface_obj, _)) in self.interface_bounds.interfaces.iter().enumerate() {
            let concrete_symbol_data = &interface_obj.0.as_ref().1;
            let interface_data = &*concrete_symbol_data.symbol_data.get_core_ref();
            if interface_data.has_method(method_name) {
                property_containing_interface_objs.push(interface_obj.to_string(interner));
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
