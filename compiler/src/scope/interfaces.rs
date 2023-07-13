use super::{
    concrete::{
        core::{ConcreteSymbolData, ConcreteTypesRegistryKey, ConcreteTypesTuple},
        registry::{ConcreteTypesRegistryCore, GenericsSpecAndConcreteTypesRegistry},
    },
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
    function::CallableData,
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

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

    pub fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        match &self.generics {
            Some(generics) => {
                return generics
                    .concrete_types_registry
                    .get_concrete_types_at_key(key)
            }
            None => unreachable!(),
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

    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceObject(String, ConcreteSymbolData<InterfaceData>); // (name, semantic data)

impl InterfaceObject {
    pub fn new(name: String, concrete_symbol_data: &ConcreteSymbolData<InterfaceData>) -> Self {
        InterfaceObject(name, concrete_symbol_data.clone())
    }

    pub fn is_eq(&self, other: &InterfaceObject) -> bool {
        if self.0.eq(&other.0) {
            // names of interfaces should be same
            match self.1.index {
                Some(self_key) => match other.1.index {
                    Some(other_key) => {
                        let self_ref = &*self.1.get_core_ref();
                        let other_ref = &*other.1.get_core_ref();
                        let self_concrete_types = &self_ref.get_concrete_types(self_key).0;
                        let other_concrete_types = &other_ref.get_concrete_types(other_key).0;
                        let self_len = self_concrete_types.len();
                        let other_len = other_concrete_types.len();
                        assert!(self_len == other_len);
                        for i in 0..self_len {
                            if !self_concrete_types[i].is_eq(&other_concrete_types[i]) {
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

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        todo!()
    }

    pub fn try_method(&self, method_name: &str) -> Option<(&CallableData, TextRange)> {
        todo!()
    }
}

impl ToString for InterfaceObject {
    fn to_string(&self) -> String {
        let mut s = self.0.to_string();
        match self.1.index {
            Some(index) => {
                let interface_data_ref = &*self.1.get_core_ref();
                s.push('<');
                s.push_str(&interface_data_ref.get_concrete_types(index).to_string());
                s.push('>');
                return s;
            }
            None => return s,
        }
    }
}
