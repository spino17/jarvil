use crate::core::string_interner::Interner;
use crate::scope::concrete::ConcreteTypesTuple;
use crate::scope::core::SymbolData;
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::semantic_db::SymbolDataEntry;
use crate::scope::traits::{AbstractSymbol, IsInitialized};
use crate::types::core::Type;
use text_size::TextRange;

#[derive(Debug)]
pub struct VariableData {
    pub data_type: Type,
    pub is_init: bool,
}

impl VariableData {
    pub fn new(variable_type: &Type, is_init: bool) -> Self {
        VariableData {
            data_type: variable_type.clone(),
            is_init,
        }
    }

    pub fn set_data_type(&mut self, data_type: &Type) {
        self.data_type = data_type.clone();
    }

    pub fn set_is_init(&mut self, is_init: bool) {
        self.is_init = is_init
    }

    pub fn set_data_type_from_optional_annotation(&mut self, ty: Type) {
        self.is_init = true;
        self.data_type = ty;
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
            data_type: Type::new_with_unset(),
            is_init: false,
        }
    }
}

#[derive(Debug)]
pub struct VariableSymbolData(pub SymbolData<VariableData>);

impl AbstractSymbol for VariableSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Variable(self.0.clone())
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<ConcreteTypesTuple>,
        _type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        _interner: &Interner,
    ) -> Result<(), GenericTypeArgsCheckError> {
        debug_assert!(!is_concrete_types_none_allowed);
        if concrete_types.is_some() {
            return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected);
        }
        Ok(())
    }

    fn get_mangled_name(&self) -> MangledIdentifierName {
        self.0.get_mangled_name()
    }
}

impl From<SymbolData<VariableData>> for VariableSymbolData {
    fn from(value: SymbolData<VariableData>) -> Self {
        VariableSymbolData(value)
    }
}
