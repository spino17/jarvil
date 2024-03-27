use super::concrete::TurbofishTypes;
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
    fn entry(&self) -> SymbolDataEntry;
    fn check_generic_type_args(
        &self,
        concrete_types: Option<&TurbofishTypes>,
        type_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        interner: &Interner,
        namespace: &Namespace,
    ) -> Result<(), GenericTypeArgsCheckError>;
    fn mangled_name(&self, namespace: &Namespace) -> MangledIdentifierName<Self::SymbolTy>;
}

pub trait InstantiationContext<'a> {
    fn is_empty(&self) -> bool {
        return true;
    }
    fn ty_generics_instantiation_args(&self) -> Option<&'a TurbofishTypes> {
        return None;
    }
    fn callable_generics_instantiation_args(&self) -> Option<&'a TurbofishTypes> {
        return None;
    }
}
