//! Traits abstracting over symbol kinds and instantiation contexts.

use super::concrete::TurbofishTypes;
use super::errors::GenericTypeArgsCheckError;
use super::namespace::Namespace;
use super::symbol::core::{SymbolDataEntry, SymbolIndex};
use crate::scope::mangled::MangledIdentifierName;
use crate::types::core::TypeStringifyContext;
use text_size::TextRange;

/// Whether a symbol's metadata has been filled in yet.
///
/// Declarations are inserted into the symbol table before their bodies are
/// resolved, so that mutually recursive references work. Between those two
/// points a symbol exists but knows nothing about itself, and this
/// distinguishes the two states.
pub trait IsInitialized {
    /// Whether this symbol's metadata has been populated.
    fn is_initialized(&self) -> bool;
}

/// Operations common to every kind of symbol, whichever namespace it lives in.
pub trait AbstractSymbol {
    /// The data this symbol carries -- a variable, callable, type or interface.
    type SymbolTy;

    /// A typed handle to this symbol within its namespace.
    fn symbol_index(&self) -> SymbolIndex<Self::SymbolTy>;

    /// The same symbol, erased to the namespace-agnostic enum.
    fn entry(&self) -> SymbolDataEntry;

    /// Validates the type arguments supplied at a use site against this
    /// symbol's generic parameters, checking both count and interface bounds.
    fn check_generic_ty_args(
        &self,
        concrete_types: Option<&TurbofishTypes>,
        ty_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        context: TypeStringifyContext,
    ) -> Result<(), GenericTypeArgsCheckError>;
    /// The name this symbol is emitted under in generated Python.
    fn mangled_name(&self, namespace: &Namespace) -> MangledIdentifierName<Self::SymbolTy>;
}

/// The type arguments in scope while concretizing a generic.
///
/// Two independent sets can apply at once -- those from the type a method is
/// bound to, and those from the method itself -- so implementations say which
/// they supply and default the other to `None`.
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
