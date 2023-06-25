use crate::scope::{concrete::ConcreteSymbolData, interfaces::InterfaceData};

#[derive(Debug)]
pub struct GenericTypeData {
    index: usize, // index in the sequence of all generic type params in declaration
    category: GenericTypeCategory,
    interface_bounds: Vec<ConcreteSymbolData<InterfaceData>>,
}

#[derive(Debug)]
pub enum GenericTypeCategory {
    Struct,
    Callable,
}
