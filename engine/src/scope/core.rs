use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::errors::JarvilError;
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
pub struct SymbolData(Rc<RefCell<MetaData>>, usize);  // meta data and line on which it was declared

#[derive(Debug)]
pub struct CoreScope {
    symbol_table: FxHashMap<Rc<String>, SymbolData>,
    pub parent_env: Option<Scope>,
}

impl CoreScope {
    fn set(&mut self, name: Rc<String>, meta_data: MetaData, line_number: usize) {
        self.symbol_table.insert(name, SymbolData(Rc::new(RefCell::new(meta_data)), line_number));
    }

    fn get(&self, name: &Rc<String>) -> Option<&SymbolData> {
        self.symbol_table.get(name)
    }
}

#[derive(Debug, Clone)]
pub struct Scope(pub Rc<RefCell<CoreScope>>);

impl Scope {
    pub fn new() -> Self {
        Scope(Rc::new(RefCell::new(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_env: None,
        })))
    }

    pub fn new_with_parent_scope(parent_env: &Scope) -> Self {
        let env = parent_env.0.clone();
        Scope(Rc::new(RefCell::new(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_env: Some(Scope(env)),
        })))
    }

    pub fn insert(&self, key: &Rc<String>, meta_data: MetaData, line_number: usize) -> Result<(), JarvilError> {
        match self.0.borrow().get(key) {
            Some(value) => {
                let err_str = format!("`{}` is already declared in the current block", key);
                return Err(JarvilError::new(value.1, value.1, err_str))
            },
            None => {}
        }
        self.0.borrow_mut().set(key.clone(), meta_data, line_number);
        Ok(())
    }

    pub fn lookup(&self, key: &Rc<String>) -> Option<SymbolData> {
        let scope_ref = self.0.borrow();
        match scope_ref.get(key) {
            Some(value) => {
                Some(SymbolData(value.0.clone(), value.1))
            },
            None => {
                if let Some(parent_env) = &scope_ref.parent_env {
                    parent_env.lookup(key)
                } else {
                    None
                }
            }
        }
    }
}