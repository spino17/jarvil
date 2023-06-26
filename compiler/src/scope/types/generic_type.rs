use crate::scope::interfaces::InterfaceObject;

#[derive(Debug)]
pub struct GenericTypeData {
    index: usize, // index in the sequence of all generic type params in declaration
    category: GenericTypeDeclarationPlaceCategory,
    interface_bounds: Vec<InterfaceObject>,
}

impl GenericTypeData {
    fn new(
        index: usize,
        category: GenericTypeDeclarationPlaceCategory,
        interface_bounds: Vec<InterfaceObject>,
    ) -> Self {
        GenericTypeData {
            index,
            category,
            interface_bounds,
        }
    }
}

#[derive(Debug)]
pub enum GenericTypeDeclarationPlaceCategory {  // This information is useful for methods
    InStruct,
    InCallable,
}
