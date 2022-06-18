// Symbol Table - HashMap for (token_name, meta data like datatypes etc.). Each scope bounded by {...} is represented by
// individual symbol table. Each symbol table points to the symbol table belonging to it's enclosing scope.
// Point of interactions:
// 1. Before lexical analysis phase to initialize the map with keywords.
// 2. During lexical analysis phase to distringuish between pattern matched for identifiers and keywords (if we found lexeme in
// symbol_table then it's a keyword so form token accordingly).
// 3. During parsing phase, we add entries in the table for identifiers when we find declartion statements in the current scope.
// 4. During parsing phase, when we encounter any statement involving identifiers then we access it for checking declartions, 
// datatypes etc.

use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::lexer::token::TokenValue;

#[derive(Debug)]
pub struct MetaData {
    data_type: Rc<String>,  // TODO - change this to Rc string
    is_init: bool,
}

#[derive(Debug)]
pub struct SymbolData(Rc<RefCell<MetaData>>);

impl SymbolData {
    
    // identifiers can be user defined types as well
    pub fn is_type(&self) -> bool {
        self.0.borrow().data_type.as_ref().eq("type")
    }

    pub fn type_eq(&self, data_type: &str) -> bool {
        self.0.borrow().data_type.as_ref().eq(data_type)
    }

    pub fn get_type(&self) -> Rc<String> {
        self.0.borrow().data_type.clone()
    }

    pub fn set_init(&self, is_init: bool) {
        self.0.borrow_mut().is_init = is_init;
    }

    pub fn is_init(&self) -> bool {
        self.0.borrow().is_init
    }
}

#[derive(Debug)]
pub struct Scope {
    symbol_table: FxHashMap<Rc<String>, SymbolData>,
    parent_env: Option<Env>,
}

impl Scope {
    fn set(&mut self, name: Rc<String>, data_type: Rc<String>, is_init: bool) {
        self.symbol_table.insert(name, SymbolData(Rc::new(RefCell::new(MetaData{data_type, is_init}))));
    }

    fn get(&self, name: &Rc<String>) -> Option<&SymbolData> {
        self.symbol_table.get(name)
    }
}

#[derive(Debug)]
pub struct Env(pub Rc<RefCell<Scope>>);

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: FxHashMap::default(),
            parent_env: None,
        })))
    }

    pub fn new_with_parent_env(parent_env: &Env) -> Self {
        let env = parent_env.0.clone();
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: FxHashMap::default(),
            parent_env: Some(Env(env)),
        })))
    }

    pub fn set(&self, token_value: &TokenValue, data_type: &Rc<String>, is_init: bool) {
        self.0.borrow_mut().set(token_value.0.clone(), data_type.clone(), is_init);
    }

    pub fn get(&self, token_value: &TokenValue) -> Option<SymbolData> {
        let scope_ref = self.0.borrow();

        // check the identifier name in current scope
        match scope_ref.get(&token_value.0) {
            Some(value) => {
                Some(SymbolData(value.0.clone()))
            },
            None => {

                // if not found in current scope, check recursively in parent scopes
                if let Some(parent_env) = &scope_ref.parent_env {

                    // return from the nearest scope which found the identifier name
                    parent_env.get(token_value)
                } else {
                    None
                }
            }
        }
    }
}