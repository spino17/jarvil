use super::{
    symbol::core::SymbolIndex,
    symbol::function::{CallableData, FunctionSymbolData},
    symbol::interfaces::{InterfaceData, InterfaceSymbolData},
    symbol::types::core::{UserDefinedTypeData, UserDefinedTypeSymbolData},
    symbol::variables::{VariableData, VariableSymbolData},
    traits::{AbstractSymbol, IsInitialized},
};
use text_size::TextRange;

#[derive(Debug)]
pub struct LookupData<T: AbstractSymbol> {
    pub symbol_data: T,
    pub depth: usize,
    pub enclosing_func_scope_depth: Option<usize>,
}

impl<T: AbstractSymbol> LookupData<T> {
    fn new(symbol_data: T, depth: usize, enclosing_func_scope_depth: Option<usize>) -> Self {
        LookupData {
            symbol_data,
            depth,
            enclosing_func_scope_depth,
        }
    }
}

pub enum LookupResult<T: AbstractSymbol> {
    Ok(LookupData<T>),
    NotInitialized(TextRange),
    Unresolved,
}

#[derive(Debug)]
pub enum IntermediateLookupResult<T: IsInitialized> {
    Ok((SymbolIndex<T>, usize, Option<usize>)),
    NotInitialized(TextRange),
    Unresolved,
}

macro_rules! impl_from_intermediate_lookup_result {
    ($x: ident, $y: ident) => {
        impl From<IntermediateLookupResult<$x>> for LookupResult<$y> {
            fn from(value: IntermediateLookupResult<$x>) -> Self {
                todo!()
            }
        }
    };
}

impl_from_intermediate_lookup_result!(VariableData, VariableSymbolData);
impl_from_intermediate_lookup_result!(CallableData, FunctionSymbolData);
impl_from_intermediate_lookup_result!(UserDefinedTypeData, UserDefinedTypeSymbolData);
impl_from_intermediate_lookup_result!(InterfaceData, InterfaceSymbolData);
