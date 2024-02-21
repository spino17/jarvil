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
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use text_size::TextRange;

#[derive(Debug)]
pub struct Symbol<T> {
    data: T,
    decl_line_number: TextRange,
    unique_id: Option<IdentDeclId<T>>,
}

impl<T> Symbol<T> {
    pub fn new(data: T, decl_line_number: TextRange, unique_id: Option<IdentDeclId<T>>) -> Self {
        Symbol {
            data,
            decl_line_number,
            unique_id,
        }
    }

    pub fn data_ref(&self) -> &T {
        &self.data
    }

    pub fn data_mut_ref(&mut self) -> &mut T {
        &mut self.data
    }

    pub fn decl_line_number(&self) -> TextRange {
        self.decl_line_number
    }
}

#[derive(Debug)]
pub struct IdentDeclId<T> {
    index: usize,
    phantom: PhantomData<T>,
}

impl<T> Copy for IdentDeclId<T> {}

impl<T> Clone for IdentDeclId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for IdentDeclId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for IdentDeclId<T> {}

impl<T> Hash for IdentDeclId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
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

#[derive(Debug)]
pub struct SymbolIndex<T: IsInitialized> {
    scope_index: ScopeIndex,
    ident_name: StrId,
    phanton: PhantomData<T>,
}

impl<T: IsInitialized> SymbolIndex<T> {
    pub fn new(scope_index: ScopeIndex, ident_name: StrId) -> Self {
        SymbolIndex {
            scope_index,
            ident_name,
            phanton: PhantomData,
        }
    }

    pub fn scope_index(&self) -> ScopeIndex {
        self.scope_index
    }

    pub fn ident_name(&self) -> StrId {
        self.ident_name
    }
}

impl<T: IsInitialized> Copy for SymbolIndex<T> {}

impl<T: IsInitialized> Clone for SymbolIndex<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: IsInitialized> SymbolIndex<T> {
    pub fn identifier_name(&self) -> StrId {
        self.ident_name
    }

    pub fn declaration_line_number(&self, arena: &ScopeArena<T>) -> TextRange {
        arena.symbol_ref(*self).decl_line_number()
    }

    pub fn index(&self, arena: &ScopeArena<T>) -> Option<IdentDeclId<T>> {
        arena.symbol_ref(*self).unique_id
    }

    pub fn is_suffix_required(&self, arena: &ScopeArena<T>) -> bool {
        self.index(arena).is_some()
    }

    pub fn mangled_name(&self, arena: &ScopeArena<T>) -> MangledIdentifierName<T> {
        MangledIdentifierName {
            jarvil_identifer_name: self.identifier_name(),
            unique_id: self.index(arena),
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
    pub fn new(symbol_entry: SymbolDataEntry, concrete_types: Option<ConcreteTypesTuple>) -> Self {
        match symbol_entry {
            SymbolDataEntry::Variable(symbol_index) => ConcreteSymbolDataEntry::Variable(
                ConcreteSymbolIndex::new(symbol_index, concrete_types),
            ),
            SymbolDataEntry::Function(symbol_index) => ConcreteSymbolDataEntry::Function(
                ConcreteSymbolIndex::new(symbol_index, concrete_types),
            ),
            SymbolDataEntry::Type(symbol_index) => ConcreteSymbolDataEntry::Type(
                ConcreteSymbolIndex::new(symbol_index, concrete_types),
            ),
            SymbolDataEntry::Interface(symbol_index) => ConcreteSymbolDataEntry::Interface(
                ConcreteSymbolIndex::new(symbol_index, concrete_types),
            ),
        }
    }
}
