use crate::{
    scope::{
        concrete::{
            callable_registry::CallableConcreteTypesRegistry,
            core::{
                ConcreteTypesRegistryKey, ConcreteTypesTuple, GenericsSpecAndConcreteTypesRegistry,
            },
            struct_registry::{MethodsConcreteTypesRegistry, StructConcreteTypesRegistry},
        },
        core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
        function::FunctionPrototype,
    },
    types::core::Type,
};
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug)]
pub struct MethodData {
    pub prototype: FunctionPrototype,
    pub generics: Option<GenericTypeParams>, // the concrete types registry for methods is handled by the struct registry
}

impl MethodData {
    pub fn new(params: Vec<Type>, return_type: Type, generics: Option<GenericTypeParams>) -> Self {
        MethodData {
            prototype: FunctionPrototype {
                params,
                return_type,
            },
            generics,
        }
    }

    pub fn set_data(
        &mut self,
        params: Vec<Type>,
        return_type: Type,
        generics: Option<GenericTypeParams>,
    ) {
        self.prototype.params = params;
        self.prototype.return_type = return_type;
        self.generics = generics;
    }
}

impl GenericContainingConstructs for MethodData {
    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}

impl Default for MethodData {
    fn default() -> Self {
        MethodData {
            prototype: FunctionPrototype::default(),
            generics: None,
        }
    }
}

#[derive(Debug)]
pub enum GenericContainingLevel {
    HasTypeAndMethodGenerics,
    HasOnlyMethodGenerics,
    NoGenerics,
}

impl Default for GenericContainingLevel {
    fn default() -> Self {
        GenericContainingLevel::NoGenerics
    }
}

#[derive(Debug, Default)]
pub struct StructTypeData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub constructor: MethodData,
    pub methods: FxHashMap<String, (MethodData, TextRange)>,
    pub class_methods: FxHashMap<String, (MethodData, TextRange)>,
    pub generics: StructTypeGenerics<()>,
    pub generics_containing_level: GenericContainingLevel,
}

impl StructTypeData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        constructor: Option<(MethodData, TextRange)>,
        methods: FxHashMap<String, (MethodData, TextRange)>,
        class_methods: FxHashMap<String, (MethodData, TextRange)>,
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.class_methods = class_methods;
        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
        self.generics = StructTypeGenerics::new(generics_spec)
    }

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &str) -> Option<(&MethodData, TextRange)> {
        match self.methods.get(method_name) {
            Some(func_data) => Some((&func_data.0, func_data.1)),
            None => None,
        }
    }

    pub fn register_method_concrete_types(
        &mut self,
        key: Option<ConcreteTypesRegistryKey>,
        method_name: String,
        method_concrete_types: Vec<Type>,
        method_generics_containing_indexes: Vec<usize>,
    ) {
        // TODO - check here whether method types have generics or not
        self.generics.register_method_concrete_types(
            key,
            method_name,
            method_concrete_types,
            method_generics_containing_indexes,
        )
    }
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        // TODO - check here whether type has generics or not
        self.generics
            .register_concrete_types(concrete_types, generics_containing_indexes)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.generics.get_concrete_types_at_key(key)
    }
}

impl GenericContainingConstructs for StructTypeData {
    fn has_generics(&self) -> bool {
        self.generics.has_generics()
    }
}

#[derive(Debug)]
pub enum StructTypeGenerics<U: Default + Clone> {
    HasGenerics(GenericsSpecAndConcreteTypesRegistry<StructConcreteTypesRegistry<U>>),
    NoGenerics(MethodsConcreteTypesRegistry<U>),
}

impl<U: Default + Clone> StructTypeGenerics<U> {
    pub fn new(generics_spec: Option<GenericTypeParams>) -> Self {
        match generics_spec {
            Some(generics_spec) => {
                StructTypeGenerics::HasGenerics(GenericsSpecAndConcreteTypesRegistry {
                    generics_spec,
                    concrete_types_registry: StructConcreteTypesRegistry::default(),
                })
            }
            None => StructTypeGenerics::NoGenerics(MethodsConcreteTypesRegistry::default()),
        }
    }

    pub fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        match self {
            StructTypeGenerics::HasGenerics(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types, generics_containing_indexes)
            }
            StructTypeGenerics::NoGenerics(_) => unreachable!(),
        }
    }

    pub fn register_implementing_struct(&mut self, key: Option<ConcreteTypesRegistryKey>) {
        todo!()
    }

    pub fn register_method_concrete_types(
        &mut self,
        key: Option<ConcreteTypesRegistryKey>,
        method_name: String,
        method_concrete_types: Vec<Type>,
        method_generics_containing_indexes: Vec<usize>,
    ) {
        match key {
            Some(key) => match self {
                StructTypeGenerics::HasGenerics(generics_spec) => generics_spec
                    .concrete_types_registry
                    .register_method_concrete_types_for_key(
                        key,
                        method_name,
                        method_concrete_types,
                        method_generics_containing_indexes,
                    ),
                StructTypeGenerics::NoGenerics(_) => unreachable!(),
            },
            None => match self {
                StructTypeGenerics::HasGenerics(_) => unreachable!(),
                StructTypeGenerics::NoGenerics(methods_concrete_types_registry) => {
                    methods_concrete_types_registry.register_method_concrete_types(
                        method_name,
                        method_concrete_types,
                        method_generics_containing_indexes,
                    )
                }
            },
        }
    }

    pub fn has_generics(&self) -> bool {
        match self {
            StructTypeGenerics::HasGenerics(_) => true,
            StructTypeGenerics::NoGenerics(_) => false,
        }
    }

    pub fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        match self {
            StructTypeGenerics::HasGenerics(generics_spec) => generics_spec
                .concrete_types_registry
                .get_concrete_types_at_key(key),
            StructTypeGenerics::NoGenerics(_) => unreachable!(),
        }
    }
}

impl<U: Default + Clone> Default for StructTypeGenerics<U> {
    fn default() -> Self {
        StructTypeGenerics::NoGenerics(MethodsConcreteTypesRegistry::default())
    }
}
