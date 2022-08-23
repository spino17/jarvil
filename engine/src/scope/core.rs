use crate::error::core::JarvilError;
use crate::scope::function::FunctionData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::scope::variables::VariableData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

use super::variables::ExpressionVariable;

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
pub enum IdentifierKind {
    VARIABLE(SymbolData<VariableData>),
    USER_DEFINED_TYPE(SymbolData<UserDefinedTypeData>),
    FUNCTION(SymbolData<FunctionData>),
}

#[derive(Debug)]
pub struct SymbolData<T>(Rc<RefCell<T>>, usize); // meta data and line on which it was declared
impl<T> Clone for SymbolData<T> {
    fn clone(&self) -> Self {
        SymbolData(self.0.clone(), self.1)
    }
}

#[derive(Debug)]
pub struct CoreScope<T> {
    symbol_table: FxHashMap<String, SymbolData<T>>,
    parent_scope: Option<Scope<T>>,
}

impl<T> CoreScope<T> {
    fn set(&mut self, name: String, meta_data: T, line_number: usize) {
        self.symbol_table.insert(
            name,
            SymbolData(Rc::new(RefCell::new(meta_data)), line_number),
        );
    }

    fn get(&self, name: &str) -> Option<&SymbolData<T>> {
        self.symbol_table.get(name)
    }
}

#[derive(Debug, Clone)]
struct Scope<T>(Rc<RefCell<CoreScope<T>>>);

impl<T> Scope<T> {
    fn new() -> Self {
        Scope(Rc::new(RefCell::new(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: None,
        })))
    }

    fn new_with_parent_scope(parent_scope: &Scope<T>) -> Self {
        let scope = parent_scope.0.clone();
        Scope(Rc::new(RefCell::new(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: Some(Scope(scope)),
        })))
    }

    fn insert(&self, key: String, meta_data: T, line_number: usize) -> Option<()> {
        if let Some(value) = self.0.borrow().get(&key) {
            return None;
        }
        self.0.borrow_mut().set(key.clone(), meta_data, line_number);
        Some(())
    }

    fn lookup(&self, key: &str) -> Option<(SymbolData<T>, usize)> {
        let scope_ref = self.0.borrow();
        match scope_ref.get(key) {
            Some(value) => Some((value.clone(), 0)),
            None => {
                if let Some(parent_env) = &scope_ref.parent_scope {
                    match &parent_env.lookup(key) {
                        Some(result) => Some((result.0.clone(), result.1 + 1)),
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }
}

pub enum NameSpaceKind {
    VARIABLES,
    TYPES,
    FUNCTIONS,
}

#[derive(Debug)]
pub struct Namespace {
    variables: Scope<VariableData>,
    types: Scope<UserDefinedTypeData>,
    functions: Scope<FunctionData>,
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

    pub fn lookup_in_variables_namespace(&self, key: &str) -> Option<(SymbolData<VariableData>, usize)> {
        self.variables.lookup(key)
    }

    pub fn lookup_in_types_namespace(&self, key: &str) -> Option<(SymbolData<UserDefinedTypeData>, usize)> {
        self.types.lookup(key)
    }

    pub fn lookup_in_functions_namespace(&self, key: &str) -> Option<(SymbolData<FunctionData>, usize)> {
        self.functions.lookup(key)
    }

    pub fn declare_variable(
        &self,
        name: String,
        data_type: Type,
        line_number: usize,
    ) -> Option<()> {
        let meta_data = VariableData::EXPRESSION(ExpressionVariable {
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
impl Default for Namespace {
    fn default() -> Self {
        Namespace::new()
    }
}
