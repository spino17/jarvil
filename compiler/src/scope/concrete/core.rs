use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::GenericContainingConstructs;
use crate::scope::core::GenericTypeParams;
use crate::scope::core::SymbolData;
use crate::types::core::AbstractType;
use crate::types::core::Type;

#[derive(Debug, Clone, Copy)]
pub struct ConcreteTypesRegistryKey(pub usize);

#[derive(Debug, Clone)]
pub struct ConcreteTypesTupleWithNoGenerics {
    pub concrete_types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct ConcreteTypesTupleWithGenerics {
    pub concrete_types: Vec<Type>, // this can contain generic type also
    pub generics_containing_indexes: Vec<usize>, // these indexes needs concretization
    pub is_concretized: Option<Vec<ConcreteTypesRegistryKey>>,
}

impl ConcreteTypesTupleWithGenerics {
    fn get_all_concrete_types_combination(&self, index: usize) -> Vec<Vec<Type>> {
        if index == self.generics_containing_indexes.len() - 1 {
            let mut v = vec![];
            for ty in &self.concrete_types[self.generics_containing_indexes[index]].concretize() {
                v.push(vec![ty.clone()])
            }
            return v;
        }
        let first_generic_concrete_type =
            self.concrete_types[self.generics_containing_indexes[index]].concretize();
        let mut remaining_concrete_types_combination =
            self.get_all_concrete_types_combination(index + 1);
        let mut all_concrete_types_combination = vec![];
        for ty in &first_generic_concrete_type {
            for ty_tuple in &mut remaining_concrete_types_combination {
                let mut v = vec![ty.clone()];
                v.append(ty_tuple);
                all_concrete_types_combination.push(v);
            }
        }
        return all_concrete_types_combination;
    }

    pub fn concretize(&mut self) -> Vec<Vec<Type>> {
        match self.is_concretized {
            Some(_) => unreachable!(),
            None => {
                let all_concrete_types_combination = self.get_all_concrete_types_combination(0);
                let mut result = vec![];
                let generics_containing_indexes_len = self.generics_containing_indexes.len();
                for ty_combination in all_concrete_types_combination {
                    let mut v = self.concrete_types.clone();
                    for index in 0..generics_containing_indexes_len {
                        let critical_index = self.generics_containing_indexes[index];
                        let concrete_type = &ty_combination[index];
                        v[critical_index] = concrete_type.clone();
                    }
                    result.push(v);
                }
                return result;
            }
        }
    }

    pub fn finish_concretization_process(&mut self, keys: &Vec<ConcreteTypesRegistryKey>) {
        self.is_concretized = Some(keys.clone());
    }
}

#[derive(Debug, Clone)]
pub enum ConcreteTypesTuple {
    HasGenericsInConcreteTypes(ConcreteTypesTupleWithGenerics),
    NoGenericsInConcreteTypes(ConcreteTypesTupleWithNoGenerics),
}

impl ConcreteTypesTuple {
    pub fn new(concrete_types: Vec<Type>, generics_containing_indexes: Vec<usize>) -> Self {
        if generics_containing_indexes.len() == 0 {
            return ConcreteTypesTuple::NoGenericsInConcreteTypes(
                ConcreteTypesTupleWithNoGenerics { concrete_types },
            );
        } else {
            return ConcreteTypesTuple::HasGenericsInConcreteTypes(
                ConcreteTypesTupleWithGenerics {
                    concrete_types,
                    is_concretized: None,
                    generics_containing_indexes,
                },
            );
        }
    }

    pub fn is_containing_generics(&self) -> bool {
        match self {
            ConcreteTypesTuple::HasGenericsInConcreteTypes(_) => true,
            ConcreteTypesTuple::NoGenericsInConcreteTypes(_) => false,
        }
    }

    pub fn get_concrete_types(&self) -> Vec<Type> {
        match self {
            ConcreteTypesTuple::HasGenericsInConcreteTypes(concrete_types_tuple) => {
                concrete_types_tuple.concrete_types.clone()
            }
            ConcreteTypesTuple::NoGenericsInConcreteTypes(concrete_types_tuple) => {
                concrete_types_tuple.concrete_types.clone()
            }
        }
    }

    pub fn is_concretization_required(&self) -> bool {
        match self {
            ConcreteTypesTuple::HasGenericsInConcreteTypes(concrete_types_tuple) => {
                return concrete_types_tuple.is_concretized.is_none()
            }
            ConcreteTypesTuple::NoGenericsInConcreteTypes(_) => false,
        }
    }
}

pub trait ConcreteTypesRegisterHandler {
    fn get_tuple_mut_ref_at_index(&mut self, index: usize) -> &mut ConcreteTypesTuple;
    fn register_new_expanded_concrete_types_tuple(
        &mut self,
        tuple: Vec<Type>,
        index: usize,
    ) -> ConcreteTypesRegistryKey;
    fn concretize_core(&mut self, key: ConcreteTypesRegistryKey) -> Vec<ConcreteTypesRegistryKey> {
        let index = key.0;
        let concrete_types_tuple_ref = self.get_tuple_mut_ref_at_index(index);
        let tuples = match concrete_types_tuple_ref {
            ConcreteTypesTuple::HasGenericsInConcreteTypes(concrete_types_tuple) => {
                match &concrete_types_tuple.is_concretized {
                    Some(keys) => return keys.clone(),
                    None => concrete_types_tuple.concretize(),
                }
            }
            _ => unreachable!(),
        };
        let mut keys = vec![];
        for tuple in tuples {
            keys.push(self.register_new_expanded_concrete_types_tuple(tuple, index));
        }
        let new_concrete_types_tuple_ref = self.get_tuple_mut_ref_at_index(index);
        match new_concrete_types_tuple_ref {
            ConcreteTypesTuple::HasGenericsInConcreteTypes(concrete_types_tuple) => {
                concrete_types_tuple.finish_concretization_process(&keys)
            }
            ConcreteTypesTuple::NoGenericsInConcreteTypes(_) => unreachable!(),
        }
        return keys;
    }
}

#[derive(Debug)]
pub struct ConcreteSymbolData<T: AbstractConcreteTypesHandler + GenericContainingConstructs> {
    pub symbol_data: SymbolData<T>,
    pub index: Option<ConcreteTypesRegistryKey>, // This will be `None` for symbol data which does not have any generic type params
}

impl<T: AbstractConcreteTypesHandler + GenericContainingConstructs> Clone
    for ConcreteSymbolData<T>
{
    fn clone(&self) -> Self {
        ConcreteSymbolData {
            symbol_data: self.symbol_data.clone(),
            index: self.index,
        }
    }
}

#[derive(Debug)]
pub struct GenericsSpecAndConcreteTypesRegistry<T: Default> {
    pub generics_spec: GenericTypeParams,
    pub concrete_types_registry: T,
}

impl<T: AbstractConcreteTypesHandler + Default> GenericsSpecAndConcreteTypesRegistry<T> {
    fn new(generics_spec: GenericTypeParams) -> Self {
        GenericsSpecAndConcreteTypesRegistry {
            generics_spec,
            concrete_types_registry: T::default(),
        }
    }
}
