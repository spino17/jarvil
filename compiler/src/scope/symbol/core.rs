use super::{
    function::CallableData, interfaces::InterfaceData, types::core::UserDefinedTypeData,
    variables::VariableData,
};
use crate::scope::mangled::MangledIdentifierName;
use crate::{
    core::string_interner::StrId,
    scope::{
        concrete::{ConcreteSymbolIndex, ConcreteTypesTuple},
        scope::{ScopeArena, ScopeIndex},
        traits::IsInitialized,
    },
};
use std::marker::PhantomData;
use text_size::TextRange;

#[derive(Debug)]
pub struct Symbol<T> {
    pub ident_name: StrId,
    pub data: T,
    pub decl_line_number: TextRange,
    pub unique_id: Option<IdentDeclId<T>>,
}

impl<T> Symbol<T> {
    pub fn new(
        ident_name: StrId,
        data: T,
        decl_line_number: TextRange,
        unique_id: Option<IdentDeclId<T>>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IdentDeclId<T> {
    index: usize,
    phantom: PhantomData<T>,
}

impl<T> IdentDeclId<T> {
    pub fn new(index: usize) -> IdentDeclId<T> {
        IdentDeclId {
            index,
            phantom: PhantomData,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Copy)]
pub struct SymbolIndex<T: IsInitialized> {
    pub scope_index: ScopeIndex,
    pub ident_name: StrId,
    pub phanton: PhantomData<T>,
}

impl<T: IsInitialized> Clone for SymbolIndex<T> {
    fn clone(&self) -> Self {
        SymbolIndex {
            scope_index: self.scope_index,
            ident_name: self.ident_name,
            phanton: PhantomData,
        }
    }
}

impl<T: IsInitialized> SymbolIndex<T> {
    pub fn identifier_name(&self) -> StrId {
        self.ident_name
    }

    pub fn declaration_line_number(&self, arena: &ScopeArena<T>) -> TextRange {
        arena.get_symbol_data_ref(*self).decl_line_number()
    }

    pub fn get_index(&self, arena: &ScopeArena<T>) -> Option<IdentDeclId<T>> {
        arena.get_symbol_data_ref(*self).unique_id
    }

    pub fn is_suffix_required(&self, arena: &ScopeArena<T>) -> bool {
        self.get_index(arena).is_some()
    }

    pub fn get_mangled_name(&self, arena: &ScopeArena<T>) -> MangledIdentifierName<T> {
        MangledIdentifierName {
            jarvil_identifer_name: self.identifier_name(),
            unique_id: self.get_index(arena),
        }
    }
}

pub enum SymbolDataEntry {
    Variable(SymbolIndex<VariableData>),
    Function(SymbolIndex<CallableData>),
    Type(SymbolIndex<UserDefinedTypeData>),
    Interface(SymbolIndex<InterfaceData>),
}

#[derive(Debug, Clone)]
pub enum ConcreteSymbolDataEntry {
    Variable(ConcreteSymbolIndex<VariableData>),
    Function(ConcreteSymbolIndex<CallableData>),
    Type(ConcreteSymbolIndex<UserDefinedTypeData>),
    Interface(ConcreteSymbolIndex<InterfaceData>),
}

impl ConcreteSymbolDataEntry {
    pub fn new(symbol_data: SymbolDataEntry, concrete_types: Option<ConcreteTypesTuple>) -> Self {
        match symbol_data {
            SymbolDataEntry::Variable(variable_symbol_data) => ConcreteSymbolDataEntry::Variable(
                ConcreteSymbolIndex::new(variable_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Function(func_symbol_data) => ConcreteSymbolDataEntry::Function(
                ConcreteSymbolIndex::new(func_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Type(type_symbol_data) => ConcreteSymbolDataEntry::Type(
                ConcreteSymbolIndex::new(type_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Interface(interface_symbol_data) => {
                ConcreteSymbolDataEntry::Interface(ConcreteSymbolIndex::new(
                    interface_symbol_data,
                    concrete_types,
                ))
            }
        }
    }
}
