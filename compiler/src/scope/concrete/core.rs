use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::SymbolData;
use crate::types::core::Type;

#[derive(Debug, Clone, Copy)]
pub struct ConcreteTypesRegistryKey(pub usize);

#[derive(Debug, Clone)]
pub struct ConcreteTypesTuple {
    pub concrete_types: Vec<Type>,
}

impl ConcreteTypesTuple {
    pub fn new(concrete_types: Vec<Type>) -> Self {
        ConcreteTypesTuple { concrete_types }
    }

    pub fn get_concrete_types(&self) -> &Vec<Type> {
        &self.concrete_types
    }
}

#[derive(Debug)]
pub struct ConcreteSymbolData<T: AbstractConcreteTypesHandler> {
    pub symbol_data: SymbolData<T>,
    pub index: Option<ConcreteTypesRegistryKey>, // This will be `None` for symbol data which does not have any generic type params
}

impl<T: AbstractConcreteTypesHandler> Clone for ConcreteSymbolData<T> {
    fn clone(&self) -> Self {
        ConcreteSymbolData {
            symbol_data: self.symbol_data.clone(),
            index: self.index,
        }
    }
}

#[derive(Debug)]
pub struct ConcretizationContext<'a> {
    pub struct_concrete_types: &'a Vec<Type>,
    pub function_local_concrete_types: &'a Vec<Type>,
}

impl<'a> ConcretizationContext<'a> {
    pub fn new(
        struct_concrete_types: &'a Vec<Type>,
        function_local_concrete_types: &'a Vec<Type>,
    ) -> Self {
        ConcretizationContext {
            struct_concrete_types,
            function_local_concrete_types,
        }
    }
}
