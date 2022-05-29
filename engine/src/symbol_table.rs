// Symbol Table - HashMap for (token_name, meta data like datatypes etc.). Each scope bounded by {...} is represented by
// individual symbol table. Each symbol table points to the symbol table belonging to it's enclosing scope.
// Point of interactions:
// 1. Before lexical analysis phase to initialize the map with keywords.
// 2. During lexical analysis phase to distringuish between pattern matched for identifiers and keywords (if we found lexeme in
// symbol_table then it's a keyword so form token accordingly).
// 3. During parsing phase, we add entries in the table for identifiers when we find declartion statements in the current scope.
// 4. During parsing phase, when we encounter any statement involving identifiers then we access it for checking declartions, 
// datatypes etc.

use std::{collections::HashMap};

#[derive(Debug)]
pub struct SymbolData {
    data_type: String,
}

#[derive(Debug)]
pub struct Scope<'a> {
    symbol_table: HashMap<String, SymbolData>,
    parent_scope_ref: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Scope {
            symbol_table: HashMap::new(),
            parent_scope_ref: None,
        }
    }

    pub fn new_with_parent_scope(parent_scope_ref: &'a Scope<'a>) -> Self {
        Scope {
            symbol_table: HashMap::new(),
            parent_scope_ref: Some(parent_scope_ref),
        }
    }

    pub fn set(&mut self, name: String, data_type: String) {
        self.symbol_table.insert(name, SymbolData { data_type, });
    }

    pub fn get(&self, name: &str) -> Option<&SymbolData> {
        match self.symbol_table.get(name) {
            Some(value) => Some(value),
            None => {
                if let Some(parent_scope_ref) = self.parent_scope_ref {
                    parent_scope_ref.get(name)
                } else {
                    None
                }
            }
        }
    }
}