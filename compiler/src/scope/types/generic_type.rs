use crate::{scope::interfaces::InterfaceObject, types::core::Type};

#[derive(Debug)]
pub struct GenericTypeData {
    pub index: usize, // index in the sequence of all generic type params in declaration
    pub category: GenericTypeDeclarationPlaceCategory,
    pub interface_bounds: Vec<InterfaceObject>,
    pub concrete_types: Vec<Type>,
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
            concrete_types: vec![]
        }
    }

    fn add_concrete_type(&mut self, ty: &Type) {
        self.concrete_types.push(ty.clone());
    }
}

#[derive(Debug)]
pub enum GenericTypeDeclarationPlaceCategory {
    // This information is useful for methods
    InStruct,
    InCallable,
}
