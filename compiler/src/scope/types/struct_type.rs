use crate::scope::concrete::core::{ConcreteTypesTuple, ConcretizationContext};
use crate::scope::concrete::registry::GenericsSpecAndConcreteTypesRegistry;
use crate::scope::core::AbstractSymbolMetaData;
use crate::scope::function::{CallableData, CallableKind};
use crate::scope::interfaces::InterfaceBounds;
use crate::types::core::AbstractType;
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
    pub implementing_interfaces: Option<InterfaceBounds>,
    pub is_init: bool,
}

impl StructTypeData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        constructor: Option<(CallableData, TextRange)>,
        methods: FxHashMap<String, (CallableData, TextRange)>,
        class_methods: FxHashMap<String, (CallableData, TextRange)>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.class_methods = class_methods;
        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
    }

    pub fn set_generics_and_interfaces(
        &mut self,
        generics_spec: Option<GenericTypeParams>,
        implementing_interfaces: Option<InterfaceBounds>,
    ) {
        self.generics.generics_spec = generics_spec;
        self.implementing_interfaces = implementing_interfaces;
        self.is_init = true;
    }

    pub fn try_field(
        &self,
        field_name: &str,
        key: Option<ConcreteTypesRegistryKey>,
    ) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some((ty, range)) => {
                if ty.has_generics() {
                    match key {
                        Some(key) => {
                            let concrete_types = self.get_concrete_types(key);
                            return Some((
                                ty.concretize(&ConcretizationContext::new(
                                    &concrete_types.0,
                                    &vec![],
                                )),
                                *range,
                            ));
                        }
                        None => unreachable!(),
                    }
                } else {
                    return Some((ty.clone(), *range));
                }
            }
            None => None,
        }
    }

    // TODO - add key: Option<ConcreteTypesRegistryKey> as argument
    pub fn try_method(&self, method_name: &str) -> Option<(&CallableData, TextRange)> {
        match self.methods.get(method_name) {
            // TODO - return &CallableData + Option<&ConcreteTypesTuple> (corrosponding to the `key passed`)
            Some(func_data) => Some((&func_data.0, func_data.1)),
            None => None,
        }
    }

    // TODO - add key: Option<ConcreteTypesRegistryKey> as argument
    pub fn try_class_method(&self, class_method_name: &str) -> Option<(&CallableData, TextRange)> {
        match self.class_methods.get(class_method_name) {
            // TODO - return &CallableData + Option<&ConcreteTypesTuple> (corrosponding to the `key passed`)
            Some(func_data) => Some((&func_data.0, func_data.1)),
            None => None,
        }
    }
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        has_generics: bool,
    ) -> ConcreteTypesRegistryKey {
        return self
            .generics
            .concrete_types_registry
            .register_concrete_types(concrete_types, has_generics);
    }

    fn is_generics_present_in_tuple_at_index(&self, index: ConcreteTypesRegistryKey) -> bool {
        self.generics
            .concrete_types_registry
            .get_concrete_types_at_key(index)
            .1
    }

    fn has_generics(&self) -> bool {
        self.generics.generics_spec.is_some()
    }

    fn is_initialized(&self) -> bool {
        self.is_init
    }
}

impl AbstractSymbolMetaData for StructTypeData {
    fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        return self
            .generics
            .concrete_types_registry
            .get_concrete_types_at_key(key);
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
            is_init: false,
        }
    }
}
