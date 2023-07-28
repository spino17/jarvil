use super::{
    concrete::{
        core::{ConcreteSymbolData, ConcreteTypesRegistryKey, ConcreteTypesTuple},
        registry::GenericsSpecAndConcreteTypesRegistry,
    },
    core::{AbstractConcreteTypesHandler, GenericTypeParams, SymbolData},
    function::CallableData,
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct InterfaceData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub methods: FxHashMap<String, (CallableData, TextRange)>,
    pub generics: GenericsSpecAndConcreteTypesRegistry,
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
        self.generics.generics_spec = generics_spec;
    }

    pub fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        return self
            .generics
            .concrete_types_registry
            .get_concrete_types_at_key(key);
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

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        todo!()
    }

    pub fn try_method(&self, method_name: &str) -> Option<(&CallableData, TextRange)> {
        todo!()
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
// One approach can be to construct `Vec<String>` from names of interfaces and types but that
// can contain a lot of collision for example:
// [ExampleInterface<genericType>, AnotherInterface] => ["ExampleInterface", "genericType", "AnotherInterface"]
// [ExampleInterface, genericType, AnotherInterface] => ["ExampleInterface", "genericType", "AnotherInterface"]
// Furthermore the equality of lambda types is structural so that complicates the process.
// Given all the above factors we used the brute force approach (which is fine for small number of interfaces which often times
// is the case).
#[derive(Debug)]
pub struct InterfaceBounds {
    interfaces: Vec<InterfaceObject>,
}

impl InterfaceBounds {
    pub fn new(interfaces: Vec<InterfaceObject>) -> Self {
        InterfaceBounds { interfaces }
    }

    pub fn contains(&self, obj: &InterfaceObject) -> bool {
        for entry in &self.interfaces {
            if entry.is_eq(obj) {
                return true;
            }
        }
        return false;
    }

    pub fn insert(&mut self, obj: InterfaceObject) -> bool {
        if self.contains(&obj) {
            return false;
        } else {
            self.interfaces.push(obj);
            return true;
        }
    }

    pub fn is_subset(&self, other: &InterfaceBounds) -> bool {
        for self_entry in &self.interfaces {
            if !other.contains(&self_entry) {
                return false;
            }
        }
        return true;
    }
}
