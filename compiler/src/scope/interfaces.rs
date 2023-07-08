use super::{
    concrete::{
        core::{ConcreteSymbolData, ConcreteTypesRegistryKey},
        registry::{ConcreteTypesRegistryCore, GenericsSpecAndConcreteTypesRegistry},
    },
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
    function::CallableData,
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Clone)]
pub struct InterfaceObject((String, ConcreteSymbolData<InterfaceData>)); // (name, semantic data)

impl InterfaceObject {
    pub fn new(name: String, concrete_symbol_data: &ConcreteSymbolData<InterfaceData>) -> Self {
        InterfaceObject((name, concrete_symbol_data.clone()))
    }

    pub fn is_eq(&self, other: &InterfaceObject) -> bool {
        if self.0 .0.eq(&other.0 .0) {
            // names of interfaces should be same
            match self.0 .1.index {
                Some(self_index) => match other.0 .1.index {
                    Some(other_index) => {
                        let self_concrete_types =
                            self.0 .1.symbol_data.get_concrete_types_at_key(self_index);
                        let base_concrete_types = other
                            .0
                             .1
                            .symbol_data
                            .get_concrete_types_at_key(other_index);
                        let self_len = self_concrete_types.len();
                        let base_len = base_concrete_types.len();
                        assert!(self_len == base_len);
                        for i in 0..self_len {
                            if !self_concrete_types[i].is_eq(&base_concrete_types[i]) {
                                return false;
                            }
                        }
                        return true;
                    }
                    None => unreachable!(),
                },
                None => return true,
            }
        }
        return false;
    }
}

#[derive(Debug, Default)]
pub struct InterfaceData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub methods: FxHashMap<String, (CallableData, TextRange)>,
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry>,
}

impl InterfaceData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        methods: FxHashMap<String, (CallableData, TextRange)>,
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.generics = match generics_spec {
            Some(generics_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                generics_spec,
                concrete_types_registry: ConcreteTypesRegistryCore::default(),
            }),
            None => None,
        }
    }
}

impl AbstractConcreteTypesHandler for InterfaceData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        match &mut self.generics {
            Some(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            None => unreachable!(),
        }
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        match &self.generics {
            Some(generics_spec) => generics_spec
                .concrete_types_registry
                .get_concrete_types_at_key(key),
            None => unreachable!(),
        }
    }

    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}
