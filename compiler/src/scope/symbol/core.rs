use super::{
    function::CallableData, interfaces::InterfaceData, types::core::UserDefinedTypeData,
    variables::VariableData,
};
use crate::{
    core::string_interner::StrId,
    scope::{
        concrete::{ConcreteSymbolData, ConcreteTypesTuple},
        core::SymbolData,
        scope::ScopeIndex,
    },
};
use std::marker::PhantomData;
use text_size::TextRange;

#[derive(Debug)]
pub struct Symbol<T> {
    pub ident_name: StrId,
    pub data: T,
    pub decl_line_number: TextRange,
    pub unique_id: Option<IdentDeclId>,
}

impl<T> Symbol<T> {
    pub fn new(
        ident_name: StrId,
        data: T,
        decl_line_number: TextRange,
        unique_id: Option<IdentDeclId>,
    ) -> Self {
        Symbol {
            ident_name,
            data,
            decl_line_number,
            unique_id,
        }
    }

    pub fn decl_line_number(&self) -> TextRange {
        self.decl_line_number
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdentDeclId(usize);

#[derive(Debug, Clone, Copy)]
pub struct SymbolIndex<T> {
    pub scope_index: ScopeIndex,
    pub ident_name: StrId,
    pub phanton: PhantomData<T>,
}

pub enum SymbolDataEntry {
    Variable(SymbolData<VariableData>),
    Function(SymbolData<CallableData>),
    Type(SymbolData<UserDefinedTypeData>),
    Interface(SymbolData<InterfaceData>),
}

#[derive(Debug, Clone)]
pub enum ConcreteSymbolDataEntry {
    Variable(ConcreteSymbolData<VariableData>),
    Function(ConcreteSymbolData<CallableData>),
    Type(ConcreteSymbolData<UserDefinedTypeData>),
    Interface(ConcreteSymbolData<InterfaceData>),
}

impl ConcreteSymbolDataEntry {
    pub fn new(symbol_data: SymbolDataEntry, concrete_types: Option<ConcreteTypesTuple>) -> Self {
        match symbol_data {
            SymbolDataEntry::Variable(variable_symbol_data) => ConcreteSymbolDataEntry::Variable(
                ConcreteSymbolData::new(variable_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Function(func_symbol_data) => ConcreteSymbolDataEntry::Function(
                ConcreteSymbolData::new(func_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Type(type_symbol_data) => ConcreteSymbolDataEntry::Type(
                ConcreteSymbolData::new(type_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Interface(interface_symbol_data) => {
                ConcreteSymbolDataEntry::Interface(ConcreteSymbolData::new(
                    interface_symbol_data,
                    concrete_types,
                ))
            }
        }
    }
}
