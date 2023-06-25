use crate::scope::{concrete::ConcreteSymbolData, interfaces::InterfaceData};

#[derive(Debug)]
pub struct GenericTypeData {
    index: usize, // index in the sequence of all generic type params in declaration
    category: GenericTypeDeclarationPlaceCategory,
    interface_bounds: Vec<ConcreteSymbolData<InterfaceData>>,
}

impl GenericTypeData {
    fn new(
        index: usize,
        category: GenericTypeDeclarationPlaceCategory,
        interface_bounds: Vec<ConcreteSymbolData<InterfaceData>>,
    ) -> Self {
        GenericTypeData {
            index,
            category,
            interface_bounds,
        }
    }
}

#[derive(Debug)]
pub enum GenericTypeDeclarationPlaceCategory {
    InStruct,
    InCallable,
}
