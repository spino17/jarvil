use super::{
    concrete::{
        ConcreteTypesRegistryKey, GenericsSpecAndConcreteTypesRegistry, StructConcreteTypesRegistry,
    },
    core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
    function::FunctionData,
};
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct InterfaceData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry<StructConcreteTypesRegistry>>,
}

impl InterfaceData {
    fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        methods: FxHashMap<String, (FunctionData, TextRange)>,
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.generics = match generics_spec {
            Some(generics_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                generics_spec,
                concrete_types_registry: StructConcreteTypesRegistry::default(),
            }),
            None => None,
        }
    }

    pub fn register_method_concrete_types_for_key(
        &mut self,
        key: &ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: &Vec<Type>,
    ) {
        //self.concrete_types_registry
        //    .register_method_concrete_types_for_key(key, method_name, method_concrete_types)
        todo!()
    }
}

impl AbstractConcreteTypesHandler for InterfaceData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        match &mut self.generics {
            Some(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            None => unreachable!(),
        }
    }
}

impl GenericContainingConstructs for InterfaceData {
    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}
