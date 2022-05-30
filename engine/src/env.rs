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
use crate::errors::SemanticError;
use crate::context::{is_keyword, is_type};

#[derive(Debug)]
pub struct MetaData {
    data_type: String,
}

#[derive(Debug)]
pub struct SymbolData(Rc<MetaData>);

impl SymbolData {
    pub fn new_keyword() -> Self {
        SymbolData(Rc::new(MetaData{
            data_type: String::from("keyword"),
        }))
    }

    pub fn new_type() -> Self {
        SymbolData(Rc::new(MetaData{
            data_type: String::from("type"),
        }))
    }

    pub fn check_type(&self, base_type: &str) -> Result<bool, SemanticError> {
        if self.0.data_type.eq(base_type) {
            Ok(true)
        } else {
            Err(SemanticError{})  // TODO - type mismatch, expected base_type but got symbol_data.0.data_type
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    symbol_table: HashMap<String, SymbolData>,
    parent_env: Option<Env>,
    // TODO - store it in local thread space as it is not modified
    // add reference to keyword table
    // add reference to data types table
}

impl Scope {
    fn set(&mut self, name: String, data_type: String) {
        self.symbol_table.insert(name, SymbolData(Rc::new(MetaData{data_type, })));
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
            symbol_table: HashMap::new(),  // TODO - fill this up with keyword strings! or we can have separate keyword table
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

    pub fn set_type(&self, name: &str) {
        // TODO - set type in data types table, useful for user defined types via struct
        todo!()
    }

    fn get(&self, name: &str) -> Option<SymbolData> {
        let scope_ref = self.0.borrow();

        // check the identifier name in current scope
        match scope_ref.get(name) {
            Some(value) => {
                Some(SymbolData(value.0.clone()))
            },
            None => {

                // if not found in current scope, check recursively in parent scopes
                if let Some(parent_env) = &scope_ref.parent_env {

                    // return from the nearest scope which found the identifier name
                    parent_env.get(name)
                } else {
                    None
                }
            }
        }
    }

    pub fn check_declaration(&self, name: &str) -> Result<SymbolData, SemanticError> {
        match self.get(name) {
            Some(symbol_data) => Ok(symbol_data),
            None => {
                Err(SemanticError{})  // TODO - identifier is not declared in the current scope
            }
        }
    }

    pub fn is_keyword(&self, name: &str) -> bool {
        // TODO - uses keyword table, useful during lexical analysis phase to distinguish lexeme for identifier and lexeme for
        // keyword
        is_keyword(name)
    }

    pub fn is_type(&self, name: &str) -> bool {
        // TODO - uses keyword table, useful during lexical analysis phase to distinguish lexeme for identifier and lexeme for
        // type
        if is_type(name) {
            true
        } else {
            // TODO - check user defined data types available in scope
            println!("checking user defined type");
            todo!()
        }
    }
}