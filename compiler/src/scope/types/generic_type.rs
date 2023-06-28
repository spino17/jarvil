use crate::{scope::interfaces::InterfaceObject, types::core::Type};

#[derive(Debug)]
pub struct GenericTypeData {
    pub index: usize, // index in the sequence of all generic type params in declaration
    pub category: GenericTypeDeclarationPlaceCategory,
    pub interface_bounds: Vec<InterfaceObject>,
    pub concrete_types: Vec<Type>,
    pub generics_containing_types_indexes: Vec<usize>, // if this is zero then this generic has being implemented by all concrete types
    pub is_concretized: bool,
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
            concrete_types: vec![],
            generics_containing_types_indexes: vec![],
            is_concretized: false, // after concretization, concrete_types will contains entries having `.has_generics() == false`
        }
    }

    pub fn add_concrete_type(&mut self, ty: &Type) {
        self.concrete_types.push(ty.clone());
    }

    pub fn has_concrete_types(&self) -> bool {
        // if this method returns `false` then that means the function containing this generic type is never called!
        if self.concrete_types.len() > 0 {
            true
        } else {
            false
        }
    }

    pub fn is_concretization_required(&self) -> bool {
        if self.generics_containing_types_indexes.len() == 0 {
            return false;
        }
        return true;
    }

    pub fn concretize_generics(&mut self) {
        // either the concrete_types is already concretized or does not have any generic types to concretize!
        if self.is_concretized || !self.is_concretization_required() {
            return;
        }
        let mut concretized_vec: Vec<Type> = vec![];
        let concrete_types_len = self.concrete_types.len();
        let mut start_index = 0;
        for &index in &self.generics_containing_types_indexes {
            for j in start_index..index {
                concretized_vec.push(self.concrete_types[j].clone());
            }
            let mut expanded_vec = self.concrete_types[index].concretize();
            concretized_vec.append(&mut expanded_vec);
            start_index = index + 1;
        }
        if start_index < concrete_types_len {
            for i in start_index..concrete_types_len {
                concretized_vec.push(self.concrete_types[i].clone());
            }
        }
        let len = concretized_vec.len();
        for i in 0..len {
            assert!(!concretized_vec[i].has_generics());
        }
        self.concrete_types = concretized_vec;
        self.is_concretized = true;
    }
}

#[derive(Debug)]
pub enum GenericTypeDeclarationPlaceCategory {
    // This information is useful for methods
    InStruct,
    InCallable,
}
