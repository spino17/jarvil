use super::core::{SymbolDataEntry, SymbolIndex};
use crate::scope::concrete::TurbofishTypes;
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::namespace::Namespace;
use crate::scope::traits::{AbstractSymbol, IsInitialized};
use crate::types::core::{Type, TypeStringifyContext};
use text_size::TextRange;

#[derive(Debug)]
pub struct VariableData {
    data_ty: Type,
    is_init: bool,
}

impl VariableData {
    pub fn new(variable_ty: &Type, is_init: bool) -> Self {
        VariableData {
            data_ty: variable_ty.clone(),
            is_init,
        }
    }

    pub fn ty(&self) -> &Type {
        &self.data_ty
    }

    pub fn set_data_ty(&mut self, data_ty: &Type) {
        self.data_ty = data_ty.clone();
    }

    pub fn set_is_init(&mut self, is_init: bool) {
        self.is_init = is_init
    }

    pub fn set_data_ty_from_optional_annotation(&mut self, ty: Type) {
        self.is_init = true;
        self.data_ty = ty;
    }
}

impl IsInitialized for VariableData {
    fn is_initialized(&self) -> bool {
        self.is_init
    }
}

impl Default for VariableData {
    fn default() -> Self {
        VariableData {
            data_ty: Type::new_with_unset(),
            is_init: false,
        }
    }
}

#[derive(Debug)]
pub struct VariableSymbolData(SymbolIndex<VariableData>);

impl AbstractSymbol for VariableSymbolData {
    type SymbolTy = VariableData;

    fn symbol_index(&self) -> SymbolIndex<Self::SymbolTy> {
        self.0
    }

    fn entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Variable(self.0)
    }

    fn check_generic_ty_args(
        &self,
        concrete_types: Option<&TurbofishTypes>,
        _ty_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        _context: TypeStringifyContext,
    ) -> Result<(), GenericTypeArgsCheckError> {
        debug_assert!(!is_concrete_types_none_allowed);

        if concrete_types.is_some() {
            return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected);
        }

        Ok(())
    }

    fn mangled_name(&self, namespace: &Namespace) -> MangledIdentifierName<VariableData> {
        self.0.mangled_name(namespace.variables_ref())
    }
}

impl From<SymbolIndex<VariableData>> for VariableSymbolData {
    fn from(value: SymbolIndex<VariableData>) -> Self {
        VariableSymbolData(value)
    }
}
