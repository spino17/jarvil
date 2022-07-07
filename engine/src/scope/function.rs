use crate::types::core::Type;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct FunctionData {
    pub params: Rc<Vec<(Rc<String>, Type)>>,  // change second column to type
    pub return_type: Rc<Option<Type>>,  // change this to type
    // generic_symbols: GenericSymbolVSBounds,
}