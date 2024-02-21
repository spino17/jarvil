use super::namespace::Namespace;
use super::symbol::core::SymbolIndex;
use crate::core::string_interner::Interner;
use crate::scope::traits::IsInitialized;
use crate::types::core::AbstractType;
use crate::types::core::Type;
use std::ops::Index;
use std::slice::Iter;

#[derive(Debug, Clone)]
pub struct ConcreteTypesTuple(Vec<Type>);

impl ConcreteTypesTuple {
    pub fn new(concrete_types: Vec<Type>) -> Self {
        ConcreteTypesTuple(concrete_types)
    }

    pub fn core_ref(&self) -> &Vec<Type> {
        &self.0
    }

    pub fn iter(&self) -> Iter<Type> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn to_string(&self, interner: &Interner, namespace: &Namespace) -> String {
        let mut s = "".to_string();
        let concrete_types = &self.0;
        let len = concrete_types.len();
        s.push_str(&concrete_types[0].to_string(interner, namespace));
        for i in 1..len {
            s.push_str(&format!(
                ", {}",
                concrete_types[i].to_string(interner, namespace)
            ));
        }
        s
    }
}

impl Index<usize> for ConcreteTypesTuple {
    type Output = Type;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Debug)]
pub struct ConcreteSymbolIndex<T: IsInitialized> {
    index: SymbolIndex<T>,
    concrete_types: Option<ConcreteTypesTuple>, // This will be `None` for symbol data which does not have any generic type params
}

impl<T: IsInitialized> Clone for ConcreteSymbolIndex<T> {
    fn clone(&self) -> Self {
        ConcreteSymbolIndex {
            index: self.index,
            concrete_types: self.concrete_types.clone(),
        }
    }
}

impl<T: IsInitialized> ConcreteSymbolIndex<T> {
    pub fn new(symbol_index: SymbolIndex<T>, concrete_types: Option<ConcreteTypesTuple>) -> Self {
        ConcreteSymbolIndex {
            index: symbol_index,
            concrete_types,
        }
    }

    pub fn symbol_index(&self) -> SymbolIndex<T> {
        self.index
    }

    pub fn concrete_types(&self) -> Option<&ConcreteTypesTuple> {
        self.concrete_types.as_ref()
    }
}

#[derive(Debug, Default)]
pub struct ConcretizationContext<'a> {
    struct_concrete_types: Option<&'a ConcreteTypesTuple>,
    function_local_concrete_types: Option<&'a ConcreteTypesTuple>,
}

impl<'a> ConcretizationContext<'a> {
    pub fn new(
        struct_concrete_types: Option<&'a ConcreteTypesTuple>,
        function_local_concrete_types: Option<&'a ConcreteTypesTuple>,
    ) -> Self {
        ConcretizationContext {
            struct_concrete_types,
            function_local_concrete_types,
        }
    }

    pub fn struct_concrete_types(&self) -> Option<&ConcreteTypesTuple> {
        self.struct_concrete_types
    }

    pub fn function_local_concrete_types(&self) -> Option<&ConcreteTypesTuple> {
        self.function_local_concrete_types
    }
}
