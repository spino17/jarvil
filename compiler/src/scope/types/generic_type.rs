use crate::scope::interfaces::InterfaceObject;

#[derive(Debug)]
pub struct GenericTypeData {
    pub index: usize, // index in the sequence of all generic type params in declaration
    pub category: GenericTypeDeclarationPlaceCategory,
    pub interface_bounds: Vec<InterfaceObject>,
}

impl GenericTypeData {
    pub fn new(
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

    pub fn get_index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Copy, Clone)]
pub enum GenericTypeDeclarationPlaceCategory {
    // This information is useful for methods
    InStruct,
    InCallable,
}
