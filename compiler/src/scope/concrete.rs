use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::SymbolData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct ConcreteTypesRegistryKey(usize);

pub struct ConcreteSymbolData<T: AbstractConcreteTypesHandler> {
    symbol_data: SymbolData<T>,
    index: Option<ConcreteTypesRegistryKey>, // This will be `None` for symbol data which does not have any generic type params
}

#[derive(Debug, Default)]
pub struct StructConcreteTypesRegistry(
    Vec<(Vec<Type>, FxHashMap<String, CallableConcreteTypesRegistry>)>,
);

impl StructConcreteTypesRegistry {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        todo!()
    }

    fn register_method_concrete_types_for_key(
        &mut self,
        key: &ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: &Vec<Type>,
    ) {
        todo!()
    }
}

#[derive(Debug, Default, Clone)]
pub struct CallableConcreteTypesRegistry(Vec<Vec<Type>>);

impl CallableConcreteTypesRegistry {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        todo!()
    }
}

/*
#[derive(Debug, Clone)]
pub struct ConcreteTypes {
    types: Vec<Type>,
    hash_str: String,
}

impl ConcreteTypes {
    fn new(types: &Vec<Type>) -> Self {
        ConcreteTypes {
            types: types.clone(), // expensive clone
            hash_str: ConcreteTypes::stringify_types(types),
        }
    }

    fn stringify_types(types: &Vec<Type>) -> String {
        let mut s = types[0].stringify();
        let len = types.len();
        for i in 1..len {
            s.push_str("_comma_");
            s.push_str(&types[i].stringify());
        }
        s
    }
}

impl Hash for ConcreteTypes {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let hash_str = &self.hash_str;
        hash_str.hash(state);
    }
}

impl PartialEq for ConcreteTypes {
    fn eq(&self, other: &Self) -> bool {
        self.hash_str == other.hash_str
    }
}

impl Eq for ConcreteTypes {}
 */
