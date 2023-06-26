use crate::{scope::interfaces::InterfaceObject, types::core::Type};

#[derive(Debug)]
pub struct GenericTypeData {
    pub index: usize, // index in the sequence of all generic type params in declaration
    pub category: GenericTypeDeclarationPlaceCategory,
    pub interface_bounds: Vec<InterfaceObject>,
    pub concrete_types: Vec<Type>,
    pub generics_containing_types_indexes: Vec<usize>,  // if this is zero then this generic has being implemented by all concrete types
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
            concrete_types: vec![],
            generics_containing_types_indexes: vec![],
        }
    }

    fn add_concrete_type(&mut self, ty: &Type) {
        self.concrete_types.push(ty.clone());
    }

    fn is_concretization_required(&self) -> bool {
        if self.generics_containing_types_indexes.len() == 0 {
            return false
        }
        return false
    }

    fn concretize_generics(&mut self) {
        let mut concretized_vec: Vec<Type> = vec![];
        // TODO - assert at this point that all the types inside concretized_vec are indeed concrete types!
        // for i in 0..len(concretized_vec) {
        //    assert!(!concretized_vec[i].has_generic())
        //}
        self.concrete_types = concretized_vec;
    }
}

#[derive(Debug)]
pub enum GenericTypeDeclarationPlaceCategory {
    // This information is useful for methods
    InStruct,
    InCallable,
}
