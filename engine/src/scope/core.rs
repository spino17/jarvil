use crate::errors::JarvilError;
use crate::scope::function::FunctionData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::scope::variables::VariableData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

macro_rules! set_to_parent_scope {
    ($t: ident, $u: ident) => {
        let parent_scope = match &$u.$t.0.as_ref().borrow().parent_scope {
            Some(parent_scope) => parent_scope.clone(),
            None => unreachable!("attempt to close namespace should not be done at global level"),
        };
        $u.$t = parent_scope;
    };
}

#[derive(Debug, Clone)]
pub enum MetaData {
    VARIABLE(VariableData),
    USER_DEFINED_TYPE(UserDefinedTypeData),
    FUNCTION(FunctionData),
}

#[derive(Debug, Clone)]
pub struct SymbolData(Rc<RefCell<MetaData>>, usize); // meta data and line on which it was declared

#[derive(Debug)]
pub struct CoreScope {
    symbol_table: FxHashMap<String, SymbolData>,
    parent_scope: Option<Scope>,
}

impl CoreScope {
    fn set(&mut self, name: String, meta_data: MetaData, line_number: usize) {
        self.symbol_table.insert(
            name,
            SymbolData(Rc::new(RefCell::new(meta_data)), line_number),
        );
    }

    fn get(&self, name: &str) -> Option<&SymbolData> {
        self.symbol_table.get(name)
    }
}

#[derive(Debug, Clone)]
struct Scope(Rc<RefCell<CoreScope>>);

impl Scope {
    fn new() -> Self {
        Scope(Rc::new(RefCell::new(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: None,
        })))
    }

    fn new_with_parent_scope(parent_scope: &Scope) -> Self {
        let scope = parent_scope.0.clone();
        Scope(Rc::new(RefCell::new(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: Some(Scope(scope)),
        })))
    }

    fn insert(&self, key: String, meta_data: MetaData, line_number: usize) -> Option<()> {
        match self.0.borrow().get(&key) {
            Some(value) => {
                // `{}` is already declared in the current block
                return None;
            }
            None => {}
        }
        self.0.borrow_mut().set(key.clone(), meta_data, line_number);
        Some(())
    }

    fn lookup(&self, key: &str) -> Option<SymbolData> {
        // resolved data and depth
        let scope_ref = self.0.borrow();
        match scope_ref.get(key) {
            Some(value) => Some(value.clone()),
            None => {
                if let Some(parent_env) = &scope_ref.parent_scope {
                    match &parent_env.lookup(key) {
                        Some(result) => Some(result.clone()),
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    // call this method only after resolving phase is done
    /*
    fn lookup_with_depth(&self, key: &Rc<String>, depth: usize) -> SymbolData {
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
            match &scope_ref.parent_scope {
                Some(parent_env) => return parent_env.lookup_with_depth(key, depth - 1),
                None => unreachable!(
                    "depth should be less than or equal to the depth of the scope chain"
                ),
            }
        }
    }
     */
}

pub enum NameSpaceKind {
    VARIABLES,
    TYPES,
    FUNCTIONS,
}

#[derive(Debug)]
pub struct Namespace {
    variables: Scope,
    types: Scope,
    functions: Scope,
}

impl Namespace {
    pub fn new() -> Self {
        Namespace {
            variables: Scope::new(),
            types: Scope::new(),
            functions: Scope::new(),
        }
    }

    pub fn open_scope(&mut self) {
        self.variables = Scope::new_with_parent_scope(&self.variables);
        self.types = Scope::new_with_parent_scope(&self.types);
        self.functions = Scope::new_with_parent_scope(&self.functions);
    }

    pub fn close_scope(&mut self) {
        set_to_parent_scope!(variables, self);
        set_to_parent_scope!(types, self);
        set_to_parent_scope!(functions, self);
    }

    pub fn lookup_in_namespace(
        &self,
        key: &str,
        namespace_kind: NameSpaceKind,
    ) -> Option<SymbolData> {
        match namespace_kind {
            NameSpaceKind::VARIABLES => self.variables.lookup(key),
            NameSpaceKind::TYPES => self.types.lookup(key),
            NameSpaceKind::FUNCTIONS => self.functions.lookup(key),
        }
    }

    pub fn declare_variable(
        &self,
        name: String,
        data_type: Type,
        line_number: usize,
    ) -> Option<()> {
        let meta_data = MetaData::VARIABLE(VariableData {
            data_type,
            is_init: false,
        });
        self.variables.insert(name, meta_data, line_number)
    }

    // pub fn declare_lambda(...)

    pub fn define_variable(&self, name: String) -> Result<(), JarvilError> {
        // SET is_init => true
        // possible errors => no entry with key `name` exists in the scope
        todo!()
    }
}

impl Clone for Namespace {
    fn clone(&self) -> Self {
        Namespace {
            variables: self.variables.clone(), // TODO - variables and lambdas
            types: self.types.clone(),
            functions: self.functions.clone(),
        }
    }
}
