use crate::scope::concrete::core::ConcreteTypesTuple;
use crate::scope::concrete::registry::GenericsSpecAndConcreteTypesRegistry;
use crate::scope::function::{CallableData, CallableKind};
use crate::scope::interfaces::InterfaceObject;
use crate::{
    scope::{
        concrete::core::ConcreteTypesRegistryKey,
        core::{AbstractConcreteTypesHandler, GenericTypeParams},
    },
    types::core::Type,
};
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug)]
pub struct StructTypeData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub constructor: CallableData,
    pub methods: FxHashMap<String, (CallableData, TextRange)>,
    pub class_methods: FxHashMap<String, (CallableData, TextRange)>,
    pub generics: GenericsSpecAndConcreteTypesRegistry,
    pub implementing_interfaces: Option<Vec<InterfaceObject>>,
}

impl StructTypeData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        constructor: Option<(CallableData, TextRange)>,
        methods: FxHashMap<String, (CallableData, TextRange)>,
        class_methods: FxHashMap<String, (CallableData, TextRange)>,
        generics_spec: Option<GenericTypeParams>,
        implementing_interfaces: Option<Vec<InterfaceObject>>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.class_methods = class_methods;
        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
        self.generics.generics_spec = generics_spec;
        self.implementing_interfaces = implementing_interfaces;
    }

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &str) -> Option<(&CallableData, TextRange)> {
        match self.methods.get(method_name) {
            Some(func_data) => Some((&func_data.0, func_data.1)),
            None => None,
        }
    }

    pub fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        return self
            .generics
            .concrete_types_registry
            .get_concrete_types_at_key(key);
    }
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        return self
            .generics
            .concrete_types_registry
            .register_concrete_types(concrete_types);
    }

    fn has_generics(&self) -> bool {
        self.generics.generics_spec.is_some()
    }
}

impl Default for StructTypeData {
    fn default() -> Self {
        StructTypeData {
            fields: FxHashMap::default(),
            constructor: CallableData::default_for_kind(CallableKind::Method),
            methods: FxHashMap::default(),
            class_methods: FxHashMap::default(),
            generics: GenericsSpecAndConcreteTypesRegistry::default(),
            implementing_interfaces: Option::default(),
        }
    }
}
