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
