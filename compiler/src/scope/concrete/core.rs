use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::SymbolData;
use crate::types::core::Type;
use std::cell::Ref;
use std::cell::RefMut;

#[derive(Debug, Clone, Copy)]
pub struct ConcreteTypesRegistryKey(pub usize);

#[derive(Debug, Clone)]
pub struct ConcreteTypesTuple(pub Vec<Type>);

impl ConcreteTypesTuple {
    pub fn new(concrete_types: Vec<Type>) -> Self {
        ConcreteTypesTuple(concrete_types)
    }

    pub fn get_concrete_types(&self) -> &ConcreteTypesTuple {
        self
    }
}

impl ToString for ConcreteTypesTuple {
    fn to_string(&self) -> String {
        let mut s = "".to_string();
        let concrete_types = &self.0;
        let len = concrete_types.len();
        s.push_str(&concrete_types[0].to_string());
        for i in 1..len {
            s.push_str(&format!(", {}", concrete_types[i].to_string()));
        }
        return s;
    }
}

#[derive(Debug)]
pub struct ConcreteSymbolData<T: AbstractConcreteTypesHandler> {
    pub symbol_data: SymbolData<T>,
    pub index: Option<ConcreteTypesRegistryKey>, // This will be `None` for symbol data which does not have any generic type params
}

impl<T: AbstractConcreteTypesHandler> ConcreteSymbolData<T> {
    pub fn new(symbol_data: SymbolData<T>, index: Option<ConcreteTypesRegistryKey>) -> Self {
        ConcreteSymbolData { symbol_data, index }
    }

    pub fn get_core_ref<'a>(&'a self) -> Ref<'a, T> {
        self.symbol_data.get_core_ref::<'a>()
    }

    pub fn get_core_mut_ref<'a>(&'a self) -> RefMut<'a, T> {
        self.symbol_data.get_core_mut_ref::<'a>()
    }
}

impl<T: AbstractConcreteTypesHandler> Clone for ConcreteSymbolData<T> {
    fn clone(&self) -> Self {
        ConcreteSymbolData {
            symbol_data: self.symbol_data.clone(),
            index: self.index,
        }
    }
}

#[derive(Debug, Default)]
pub struct ConcretizationContext<'a> {
    pub struct_concrete_types: Option<&'a Vec<Type>>,
    pub function_local_concrete_types: Option<&'a Vec<Type>>,
}

impl<'a> ConcretizationContext<'a> {
    pub fn new(
        struct_concrete_types: Option<&'a Vec<Type>>,
        function_local_concrete_types: Option<&'a Vec<Type>>,
    ) -> Self {
        ConcretizationContext {
            struct_concrete_types,
            function_local_concrete_types,
        }
    }
}
