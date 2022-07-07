use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::scope::user_defined_types::{UserDefinedTypeData};
use crate::scope::identifier::IdentifierData;
use crate::scope::function::FunctionData;

#[derive(Debug)]
pub enum MetaData {
    IDENTIFIER(IdentifierData),
    USER_DEFINED_TYPE(UserDefinedTypeData),
    FUNCTION(FunctionData),
}

#[derive(Debug)]
pub struct SymbolData(Rc<RefCell<MetaData>>);

#[derive(Debug)]
pub struct Scope {
    symbol_table: FxHashMap<Rc<String>, SymbolData>,
    pub parent_env: Option<Env>,
    return_type: Option<Rc<String>>,  // for functional scope - match return type in nested sub blocks checking this global field
}

impl Scope {
    fn set(&mut self, name: Rc<String>, meta_data: MetaData) {
        self.symbol_table.insert(name, SymbolData(Rc::new(RefCell::new(meta_data))));
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
            return_type: None,
        })))
    }

    pub fn new_with_parent_env(parent_env: &Env) -> Self {
        let env = parent_env.0.clone();
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: FxHashMap::default(),
            parent_env: Some(Env(env)),
            return_type: None,
        })))
    }

    pub fn insert(&self, key: &Rc<String>, meta_data: MetaData) {
        self.0.borrow_mut().set(key.clone(), meta_data)
    }

    pub fn resolve(&self, key: &Rc<String>) -> Option<SymbolData> {
        let scope_ref = self.0.borrow();

        // check the identifier name in current scope
        match scope_ref.get(key) {
            Some(value) => {
                Some(SymbolData(value.0.clone()))
            },
            None => {

                // if not found in current scope, check recursively in parent scopes
                if let Some(parent_env) = &scope_ref.parent_env {

                    // return from the nearest scope which found the identifier name
                    parent_env.resolve(key)
                } else {
                    None
                }
            }
        }
    }
}