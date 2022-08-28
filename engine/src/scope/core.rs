use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::scope::variables::VariableData;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use super::function::FunctionData;

#[derive(Debug, Clone)]
pub enum IdentifierKind {
    VARIABLE(SymbolData<VariableData>),
    USER_DEFINED_TYPE(SymbolData<UserDefinedTypeData>),
    FUNCTION(SymbolData<FunctionData>),
}

#[derive(Debug)]
pub struct SymbolData<T>(pub Rc<RefCell<T>>, usize); // meta data and line on which it was declared
impl<T> Clone for SymbolData<T> {
    fn clone(&self) -> Self {
        SymbolData(self.0.clone(), self.1)
    }
}

#[derive(Debug)]
pub struct CoreScope<T> {
    symbol_table: FxHashMap<Rc<String>, SymbolData<T>>,
    parent_scope: Option<Scope<T>>,
}

impl<T> CoreScope<T> {
    fn set(&mut self, name: &Rc<String>, meta_data: T, line_number: usize) -> SymbolData<T> {
        let symbol_data = SymbolData(Rc::new(RefCell::new(meta_data)), line_number);
        self.symbol_table.insert(
            name.clone(),
            symbol_data.clone(),
        );
        symbol_data
    }

    fn get(&self, name: &Rc<String>) -> Option<&SymbolData<T>> {
        self.symbol_table.get(name)
    }
}

#[derive(Debug, Clone)]
pub struct Scope<T>(Rc<RefCell<CoreScope<T>>>);

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

    fn insert<U: Fn(Scope<T>, Rc<String>) -> Option<SymbolData<T>>>(
        &self, key: &Rc<String>, meta_data: T, line_number: usize, lookup_fn: U
    ) -> Result<SymbolData<T>, usize> {
        let scope = Scope(self.0.clone());
        if let Some(symbol_data) = lookup_fn(scope, key.clone()) {
            return Err(symbol_data.1);
        }
        let symbol_data = self.0.borrow_mut().set(key, meta_data, line_number);
        Ok(symbol_data)
    }

    fn lookup(&self, key: &Rc<String>) -> Option<(SymbolData<T>, usize)> {
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

    pub fn lookup_in_variables_namespace(&self, key: &Rc<String>) -> Option<(SymbolData<VariableData>, usize)> {
        self.variables.lookup(key)
    }

    pub fn lookup_in_types_namespace(&self, key: &Rc<String>) -> Option<(SymbolData<UserDefinedTypeData>, usize)> {
        self.types.lookup(key)
    }

    pub fn lookup_in_functions_namespace(&self, key: &Rc<String>) -> Option<(SymbolData<FunctionData>, usize)> {
        self.functions.lookup(key)
    }

    pub fn declare_variable(&self, name: &Rc<String>, line_number: usize) -> Result<SymbolData<VariableData>, usize> {
        let lookup_func = |scope: Scope<VariableData>, key: Rc<String>| {
            match scope.0.as_ref().borrow().get(&key) {
                Some(symbol_data) => Some(symbol_data.clone()),
                None => None,
            }
        };
        self.variables.insert(name, VariableData::default(), line_number, lookup_func)
    }

    pub fn declare_function(&self, name: &Rc<String>, line_number: usize) -> Result<SymbolData<FunctionData>, usize> {
        let lookup_func = |scope: Scope<FunctionData>, key: Rc<String>| {
            match scope.0.as_ref().borrow().get(&key) {
                Some(symbol_data) => Some(symbol_data.clone()),
                None => None,
            }
        };
        self.functions.insert(name, FunctionData(None), line_number, lookup_func)
    }

    pub fn declare_struct_type(&self, name: &Rc<String>, line_number: usize) -> Result<SymbolData<UserDefinedTypeData>, usize> {
        let lookup_func = |scope: Scope<UserDefinedTypeData>, key: Rc<String>| {
            match scope.lookup(&key) {
                Some((symbol_data, _)) => Some(symbol_data),
                None => None
            }
        };
        self.types.insert(name, UserDefinedTypeData::STRUCT(None), line_number, lookup_func)
    }

    pub fn declare_lambda_type(&self, name: &Rc<String>, line_number: usize) -> Result<SymbolData<UserDefinedTypeData>, usize> {
        let lookup_func = |scope: Scope<UserDefinedTypeData>, key: Rc<String>| {
            match scope.lookup(&key) {
                Some((symbol_data, _)) => Some(symbol_data),
                None => None
            }
        };
        self.types.insert(name, UserDefinedTypeData::LAMBDA(None), line_number, lookup_func)
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
