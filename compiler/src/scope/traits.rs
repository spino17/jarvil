use super::concrete::TurbofishTypes;
use super::errors::GenericTypeArgsCheckError;
use super::namespace::Namespace;
use super::symbol::core::{SymbolDataEntry, SymbolIndex};
use crate::scope::mangled::MangledIdentifierName;
use crate::types::core::TypeStringifyContext;
use text_size::TextRange;

pub trait IsInitialized {
    fn is_initialized(&self) -> bool;
}

pub trait AbstractSymbol {
    type SymbolTy;
    fn symbol_index(&self) -> SymbolIndex<Self::SymbolTy>;
    fn entry(&self) -> SymbolDataEntry;
    fn check_generic_ty_args(
        &self,
        concrete_types: Option<&TurbofishTypes>,
        ty_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        context: TypeStringifyContext,
    ) -> Result<(), GenericTypeArgsCheckError>;
    fn mangled_name(&self, namespace: &Namespace) -> MangledIdentifierName<Self::SymbolTy>;
}

pub trait InstantiationContext<'a> {
    fn is_empty(&self) -> bool {
        true
    }
    fn ty_generics_instantiation_args(&self) -> Option<&'a TurbofishTypes> {
        None
    }
    fn callable_generics_instantiation_args(&self) -> Option<&'a TurbofishTypes> {
        None
    }
}
