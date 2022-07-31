use crate::errors::JarvilError;
use crate::scope::function::FunctionData;
use crate::scope::identifier::IdentifierData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum MetaData {
    IDENTIFIER(IdentifierData),
    USER_DEFINED_TYPE(UserDefinedTypeData),
    FUNCTION(FunctionData),
}

#[derive(Debug, Clone)]
pub struct SymbolData(Rc<RefCell<MetaData>>, usize); // meta data and line on which it was declared
impl SymbolData {
    fn kind_as_str(&self) -> &str {
        match &*self.0.as_ref().borrow() {
            MetaData::IDENTIFIER(_) => "identifier",
            MetaData::USER_DEFINED_TYPE(user_defined_type) => {
                match user_defined_type {
                    UserDefinedTypeData::STRUCT(_) => return "struct",
                    UserDefinedTypeData::LAMBDA(_) => return "lambda",
                }
            },
            MetaData::FUNCTION(_) => "function",
        }
    }
}

#[derive(Debug)]
pub struct CoreScope {
    symbol_table: FxHashMap<Rc<String>, SymbolData>,
    pub parent_env: Option<Scope>,
}

impl CoreScope {
    fn set(&mut self, name: Rc<String>, meta_data: MetaData, line_number: usize) {
        self.symbol_table.insert(
            name,
            SymbolData(Rc::new(RefCell::new(meta_data)), line_number),
        );
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

    pub fn insert(
        &self,
        key: &Rc<String>,
        meta_data: MetaData,
        line_number: usize,
    ) -> Result<(), JarvilError> {
        match self.0.borrow().get(key) {
            Some(value) => {
                let err_str = format!("`{}` is already declared in the current block as `{}`", key, value.kind_as_str());
                return Err(JarvilError::new(value.1, value.1, err_str));
            }
            None => {}
        }
        self.0.borrow_mut().set(key.clone(), meta_data, line_number);
        Ok(())
    }

    pub fn lookup(&self, key: &Rc<String>) -> Option<(SymbolData, usize)> {
        // resolved data and depth
        let scope_ref = self.0.borrow();
        match scope_ref.get(key) {
            Some(value) => Some((SymbolData(value.0.clone(), value.1), 0)),
            None => {
                if let Some(parent_env) = &scope_ref.parent_env {
                    match parent_env.lookup(key) {
                        Some(result) => Some((result.0, result.1 + 1)),
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    // call this method only after resolving phase is done
    pub fn lookup_with_depth(&self, key: &Rc<String>, depth: usize) -> SymbolData {
        let scope_ref = self.0.borrow();
        if depth == 0 {
            match scope_ref.get(key) {
                Some(value) => return SymbolData(value.0.clone(), value.1),
                None => unreachable!(
                    "data for key `{}` should be present if resolving phase is done",
                    key
                ),
            }
        } else {
            match &scope_ref.parent_env {
                Some(parent_env) => return parent_env.lookup_with_depth(key, depth - 1),
                None => unreachable!(
                    "depth should be less than or equal to the depth of the scope chain"
                ),
            }
        }
    }
}
