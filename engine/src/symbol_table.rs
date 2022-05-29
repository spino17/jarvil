// Symbol Table - HashMap for (token_name, meta data like datatypes etc.). Each scope bounded by {...} is represented by
// individual symbol table. Each symbol table points to the symbol table belonging to it's enclosing scope.
// Point of interactions:
// 1. Before lexical analysis phase to initialize the map with keywords.
// 2. During lexical analysis phase to distringuish between pattern matched for identifiers and keywords (if we found lexeme in
// symbol_table then it's a keyword so form token accordingly).
// 3. During parsing phase, we add entries in the table for identifiers when we find declartion statements in the current scope.
// 4. During parsing phase, when we encounter any statement involving identifiers then we access it for checking declartions, 
// datatypes etc.

use std::{collections::HashMap, cell::RefCell};
use std::rc::Rc;

#[derive(Debug)]
pub struct SymbolData {
    data_type: String,
}

#[derive(Debug)]
pub struct Scope {
    symbol_table: HashMap<String, SymbolData>,
    parent_env: Option<Env>,
}

impl Scope {
    fn set(&mut self, name: String, data_type: String) {
        self.symbol_table.insert(name, SymbolData { data_type, });
    }

    fn get(&self, name: &str) -> Option<&SymbolData> {
        self.symbol_table.get(name)
    }
}

#[derive(Debug)]
pub struct Env(Rc<RefCell<Scope>>);

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: HashMap::new(),
            parent_env: None,
        })))
    }

    pub fn new_with_parent_env(parent_env: &Env) -> Self {
        let env = parent_env.0.clone();
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: HashMap::new(),
            parent_env: Some(Env(env)),
        })))
    }

    pub fn set(&self, name: String, data_type: String) {
        self.0.borrow_mut().set(name, data_type);
    }

    pub fn get(&self, name: &str) -> Option<i64> {
        /*
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
         */
        let scope_ref = self.0.borrow();
        match scope_ref.get(name) {
            Some(value) => {
                Some(10)
            },
            None => {
                if let Some(parent_env) = &scope_ref.parent_env {
                    parent_env.get(name)
                } else {
                    None
                }
            }
        }
    }
}