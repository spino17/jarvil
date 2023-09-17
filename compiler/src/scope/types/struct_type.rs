use crate::scope::common::{FieldsMap, MethodsMap};
use crate::scope::concrete::core::{ConcreteTypesTuple, ConcretizationContext};
use crate::scope::concrete::registry::GenericsSpecAndConcreteTypesRegistry;
use crate::scope::core::AbstractSymbolMetaData;
use crate::scope::function::{CallableData, CallableKind, PartialConcreteCallableDataRef};
use crate::scope::interfaces::InterfaceBounds;
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
    fields: FieldsMap,
    pub constructor: CallableData,
    methods: MethodsMap,
    class_methods: MethodsMap,
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
        self.fields = FieldsMap::new(fields);
        self.methods = MethodsMap::new(methods);
        self.class_methods = MethodsMap::new(class_methods);
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
        context: &ConcretizationContext,
    ) -> Option<(Type, TextRange)> {
        self.fields.try_field(field_name, context)
    }

    pub fn try_method<'a>(
        &'a self,
        method_name: &str,
        global_concrete_types: Option<&'a Vec<Type>>,
    ) -> Option<(PartialConcreteCallableDataRef, TextRange)> {
        self.methods.try_method(method_name, global_concrete_types)
    }

    pub fn try_class_method<'a>(
        &'a self,
        class_method_name: &str,
        global_concrete_types: Option<&'a Vec<Type>>,
    ) -> Option<(PartialConcreteCallableDataRef, TextRange)> {
        self.class_methods
            .try_method(class_method_name, global_concrete_types)
    }

    pub fn get_methods_ref(&self) -> &MethodsMap {
        &self.methods
    }
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        return self
            .generics
            .concrete_types_registry
            .register_concrete_types(concrete_types);
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
            fields: FieldsMap::default(),
            constructor: CallableData::default_for_kind(CallableKind::Method),
            methods: MethodsMap::default(),
            class_methods: MethodsMap::default(),
            generics: GenericsSpecAndConcreteTypesRegistry::default(),
            implementing_interfaces: Option::default(),
            is_init: false,
        }
    }
}
