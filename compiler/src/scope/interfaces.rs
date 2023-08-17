use super::{
    common::{FieldsMap, MethodsMap},
    concrete::{
        core::{ConcreteSymbolData, ConcreteTypesRegistryKey, ConcreteTypesTuple},
        registry::{ConcreteTypesRegistryCore, GenericsSpecAndConcreteTypesRegistry},
    },
    core::{AbstractConcreteTypesHandler, AbstractSymbolMetaData, GenericTypeParams, SymbolData},
    function::{CallableData, PartialConcreteCallableDataRef},
};
use crate::types::core::Type;
use crate::{error, scope::concrete::core::ConcretizationContext, types::core::AbstractType};
use rustc_hash::FxHashMap;
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct InterfaceData {
    fields: FieldsMap,
    methods: MethodsMap,
    pub generics: GenericsSpecAndConcreteTypesRegistry,
    pub is_init: bool,
}

impl InterfaceData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        methods: FxHashMap<String, (CallableData, TextRange)>,
    ) {
        self.fields = FieldsMap::new(fields);
        self.methods = MethodsMap::new(methods);
    }

    pub fn set_generics(&mut self, generics_spec: Option<GenericTypeParams>) {
        self.generics.generics_spec = generics_spec;
        self.is_init = true;
    }

    pub fn try_field(
        &self,
        field_name: &str,
        key: Option<ConcreteTypesRegistryKey>,
    ) -> Option<(Type, TextRange)> {
        self.fields.try_field(field_name, key, self)
    }

    pub fn try_method(
        &self,
        method_name: &str,
        key: Option<ConcreteTypesRegistryKey>,
    ) -> Option<(PartialConcreteCallableDataRef, TextRange)> {
        self.methods
            .try_method(method_name, key, &self.generics.concrete_types_registry)
    }

    pub fn get_partially_concrete_interface_methods(
        &self,
        key: Option<ConcreteTypesRegistryKey>,
    ) -> PartialConcreteInterfaceMethods {
        PartialConcreteInterfaceMethods::get_from_registry_key(
            &self.methods,
            &self.generics.concrete_types_registry,
            key,
        )
    }
}

impl AbstractConcreteTypesHandler for InterfaceData {
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

impl AbstractSymbolMetaData for InterfaceData {
    fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        return self
            .generics
            .concrete_types_registry
            .get_concrete_types_at_key(key);
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceObject(Rc<(String, ConcreteSymbolData<InterfaceData>)>); // (name, semantic data)

impl InterfaceObject {
    pub fn new(
        name: String,
        symbol_data: SymbolData<InterfaceData>,
        index: Option<ConcreteTypesRegistryKey>,
    ) -> Self {
        InterfaceObject(Rc::new((name, ConcreteSymbolData::new(symbol_data, index))))
    }

    pub fn is_eq(&self, other: &InterfaceObject) -> bool {
        if self.0.as_ref().0.eq(&other.0.as_ref().0) {
            // names of interfaces should be same
            match self.0.as_ref().1.index {
                Some(self_key) => match other.0.as_ref().1.index {
                    Some(other_key) => {
                        let self_ref = &*self.0.as_ref().1.get_core_ref();
                        let self_concrete_types = &self_ref.get_concrete_types(self_key).0;
                        let self_len = self_concrete_types.len();

                        let other_ref = &*other.0.as_ref().1.get_core_ref();
                        let other_concrete_types = &other_ref.get_concrete_types(other_key).0;
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
}

impl ToString for InterfaceObject {
    fn to_string(&self) -> String {
        let mut s = self.0.as_ref().0.to_string();
        match self.0.as_ref().1.index {
            Some(index) => {
                let interface_data_ref = &*self.0.as_ref().1.get_core_ref();
                s.push('<');
                s.push_str(&interface_data_ref.get_concrete_types(index).to_string());
                s.push('>');
                return s;
            }
            None => return s,
        }
    }
}

// NOTE - More efficient way to implement interface bounds is to use `HashSet<InterfaceObject>`
// which would require us to make `InterfaceObject` hashable. We already have the notion
// of equality between `InterfaceObject` but to construct a hash function which adheres
// to the following constraint: k1 == k1 => Hash(k1) == Hash(k2) is not at all obvious.
// One approach can be to construct `Vec<String>` (which is hashable) from names of interfaces and types but that
// can contain a lot of collision for example:
// [ExampleInterface<genericType>, AnotherInterface] => ["ExampleInterface", "genericType", "AnotherInterface"]
// [ExampleInterface, genericType, AnotherInterface] => ["ExampleInterface", "genericType", "AnotherInterface"]
// Furthermore the equality of lambda types is structural so that complicates the process.
// To actually realize a hash implementation of `InterfaceObject` requires more advanced name-mangling approach which for now
// we choose to skip.
// Given all the above factors we used the brute force approach (which is fine for small number of interfaces which often times
// is the case).
#[derive(Debug, Default, Clone)]
pub struct InterfaceBounds {
    pub interfaces: Vec<(InterfaceObject, TextRange)>,
}

impl InterfaceBounds {
    pub fn new(interfaces: Vec<(InterfaceObject, TextRange)>) -> Self {
        InterfaceBounds { interfaces }
    }

    pub fn len(&self) -> usize {
        self.interfaces.len()
    }

    pub fn contains(&self, obj: &InterfaceObject) -> Option<TextRange> {
        for (entry, decl_range) in &self.interfaces {
            if entry.is_eq(obj) {
                return Some(*decl_range);
            }
        }
        return None;
    }

    pub fn insert(&mut self, obj: InterfaceObject, decl_range: TextRange) -> Option<TextRange> {
        match self.contains(&obj) {
            Some(previous_decl_range) => Some(previous_decl_range),
            None => {
                self.interfaces.push((obj, decl_range));
                return None;
            }
        }
    }

    pub fn is_subset(&self, other: &InterfaceBounds) -> bool {
        for (self_entry, _) in &self.interfaces {
            if other.contains(&self_entry).is_none() {
                return false;
            }
        }
        return true;
    }

    pub fn is_eq(&self, other: &InterfaceBounds) -> bool {
        self.is_subset(other) && other.is_subset(self)
    }
}

impl ToString for InterfaceBounds {
    fn to_string(&self) -> String {
        let mut s = "{".to_string();
        let len = self.interfaces.len();
        if len > 0 {
            s.push_str(&self.interfaces[0].0.to_string());
        }
        for i in 1..len {
            s.push_str(" + ");
            s.push_str(&self.interfaces[i].0.to_string());
        }
        s.push('}');
        s
    }
}

#[derive(Debug)]
pub enum PartialConcreteInterfaceMethodsCheckError {
    GenericTypesDeclarationExpected(TextRange),
    GenericTypesDeclarationNotExpected(TextRange),
    PrototypeEquivalenceCheckFailed(TextRange),
    GenericTypesDeclarationCheckFailed(TextRange),
}

#[derive(Debug)]
pub struct PartialConcreteInterfaceMethods<'a> {
    methods: &'a MethodsMap,
    concrete_types: Option<&'a Vec<Type>>,
}

impl<'a> PartialConcreteInterfaceMethods<'a> {
    pub fn new(methods: &'a MethodsMap, concrete_types: Option<&'a Vec<Type>>) -> Self {
        PartialConcreteInterfaceMethods {
            methods,
            concrete_types,
        }
    }

    pub fn get_from_registry_key(
        methods: &'a MethodsMap,
        registry: &'a ConcreteTypesRegistryCore,
        key: Option<ConcreteTypesRegistryKey>,
    ) -> PartialConcreteInterfaceMethods<'a> {
        let concrete_types = match key {
            Some(key) => Some(&registry.get_concrete_types_at_key(key).0),
            None => None,
        };
        return PartialConcreteInterfaceMethods::new(methods, concrete_types);
    }

    pub fn is_struct_implements_interface_methods(&self, struct_methods: &MethodsMap) {
        let struct_methods_map_ref = struct_methods.get_methods_ref();
        let mut missing_interface_method_names: Vec<&str> = vec![];
        let mut errors: Vec<(&str, PartialConcreteInterfaceMethodsCheckError)> = vec![];
        for (interface_method_name, (interface_method_callable_data, _)) in
            self.methods.get_methods_ref()
        {
            match struct_methods_map_ref.get(interface_method_name) {
                Some((struct_method_callable_data, range)) => {
                    let interface_method_generic_type_decls =
                        &interface_method_callable_data.generics.generics_spec;
                    let struct_method_generic_type_decls =
                        &struct_method_callable_data.generics.generics_spec;
                    match interface_method_generic_type_decls {
                        Some(interface_method_generic_type_decls) => {
                            match struct_method_generic_type_decls {
                                Some(struct_method_generic_type_decls) => {
                                    // check if number of generic type declarations match
                                    if interface_method_generic_type_decls.len()
                                        != struct_method_generic_type_decls.len()
                                    {
                                        errors.push((interface_method_name, PartialConcreteInterfaceMethodsCheckError::GenericTypesDeclarationCheckFailed(*range)));
                                        continue;
                                    }
                                    // check if the interface bounds each generic type in the declaration match
                                    let generic_type_decls_len =
                                        interface_method_generic_type_decls.len();
                                    for index in 0..generic_type_decls_len {
                                        let interface_generic_bound =
                                            &interface_method_generic_type_decls.0[index].1;
                                        let struct_generic_bound =
                                            &struct_method_generic_type_decls.0[index].1;
                                        if !interface_generic_bound.is_eq(struct_generic_bound) {
                                            errors.push((interface_method_name, PartialConcreteInterfaceMethodsCheckError::GenericTypesDeclarationCheckFailed(*range)));
                                            continue;
                                        }
                                    }
                                    // check if prototypes match
                                    if interface_method_callable_data.prototype.is_structurally_eq(
                                        &struct_method_callable_data.prototype,
                                        &ConcretizationContext::new(self.concrete_types, None),
                                    ) {
                                        errors.push((interface_method_name, PartialConcreteInterfaceMethodsCheckError::PrototypeEquivalenceCheckFailed(*range)));
                                        continue;
                                    }
                                }
                                None => {
                                    errors.push((interface_method_name, PartialConcreteInterfaceMethodsCheckError::GenericTypesDeclarationExpected(*range)));
                                    continue;
                                }
                            }
                        }
                        None => match struct_method_generic_type_decls {
                            Some(_) => {
                                errors.push((interface_method_name, PartialConcreteInterfaceMethodsCheckError::GenericTypesDeclarationNotExpected(*range)));
                                continue;
                            }
                            None => {
                                todo!()
                            }
                        },
                    }
                }
                None => missing_interface_method_names.push(interface_method_name),
            }
        }
    }
}
