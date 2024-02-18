use super::concrete::ConcreteTypesTuple;
use super::errors::GenericTypeArgsCheckError;
use super::namespace::Namespace;
use super::symbol::core::{SymbolDataEntry, SymbolIndex};
use crate::core::string_interner::Interner;
use crate::scope::mangled::MangledIdentifierName;
use text_size::TextRange;

pub trait IsInitialized {
    fn is_initialized(&self) -> bool;
}

pub trait AbstractSymbol {
    type SymbolTy;
    fn symbol_index(&self) -> SymbolIndex<Self::SymbolTy>
    where
        <Self as AbstractSymbol>::SymbolTy: IsInitialized;
    fn get_entry(&self) -> SymbolDataEntry;
    fn check_generic_type_args(
        &self,
        concrete_types: Option<&ConcreteTypesTuple>,
        type_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        interner: &Interner,
        namespace: &Namespace,
    ) -> Result<(), GenericTypeArgsCheckError>;
    fn get_mangled_name(&self, namespace: &Namespace) -> MangledIdentifierName<Self::SymbolTy>;
}
