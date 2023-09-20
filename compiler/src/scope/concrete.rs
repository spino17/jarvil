use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::SymbolData;
use crate::types::core::Type;
use std::cell::Ref;
use std::cell::RefMut;

#[derive(Debug, Clone)]
pub struct ConcreteTypesTuple(Vec<Type>);

impl ConcreteTypesTuple {
    pub fn new(concrete_types: Vec<Type>) -> Self {
        ConcreteTypesTuple(concrete_types)
    }

    pub fn get_concrete_types(&self) -> &ConcreteTypesTuple {
        self
    }

    pub fn get_core_ref(&self) -> &Vec<Type> {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.0.len()
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
    pub concrete_types: Option<ConcreteTypesTuple>, // This will be `None` for symbol data which does not have any generic type params
}

impl<T: AbstractConcreteTypesHandler> ConcreteSymbolData<T> {
    pub fn new(symbol_data: SymbolData<T>, concrete_types: Option<ConcreteTypesTuple>) -> Self {
        ConcreteSymbolData {
            symbol_data,
            concrete_types,
        }
    }

    pub fn get_core_ref<'a>(&'a self) -> Ref<'a, T> {
        self.symbol_data.get_core_ref::<'a>()
    }

    pub fn get_core_mut_ref<'a>(&'a self) -> RefMut<'a, T> {
        self.symbol_data.get_core_mut_ref::<'a>()
    }

    pub fn get_concrete_types(&self) -> &Option<ConcreteTypesTuple> {
        &self.concrete_types
    }
}

#[derive(Debug, Default)]
pub struct ConcretizationContext<'a> {
    pub struct_concrete_types: Option<&'a ConcreteTypesTuple>,
    pub function_local_concrete_types: Option<&'a ConcreteTypesTuple>,
}

impl<'a> ConcretizationContext<'a> {
    pub fn new(
        struct_concrete_types: Option<&'a ConcreteTypesTuple>,
        function_local_concrete_types: Option<&'a ConcreteTypesTuple>,
    ) -> Self {
        ConcretizationContext {
            struct_concrete_types,
            function_local_concrete_types,
        }
    }
}
