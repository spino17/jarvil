use super::{core::AbstractConcreteTypesHandler, function::FunctionData};
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct InterfaceData {
    pub functions: FxHashMap<String, (FunctionData, TextRange)>,
    pub concrete_types_registry: Vec<Vec<Type>>,
}

impl InterfaceData {
    fn new(functions: FxHashMap<String, (FunctionData, TextRange)>) -> Self {
        InterfaceData {
            functions,
            concrete_types_registry: vec![],
        }
    }
}

impl AbstractConcreteTypesHandler for InterfaceData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> usize {
        let index = self.concrete_types_registry.len();
        self.concrete_types_registry.push(concrete_types.clone());
        index
    }
}
