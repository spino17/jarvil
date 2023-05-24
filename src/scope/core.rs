use super::function::FunctionData;
use super::user_defined_types::LambdaTypeData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::scope::variables::VariableData;
use crate::types::core::Type;
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::RefCell;
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug, Clone)]
pub enum IdentifierKind {
    VARIABLE(SymbolData<VariableData>),
    USER_DEFINED_TYPE(SymbolData<UserDefinedTypeData>),
    FUNCTION(SymbolData<FunctionData>),
}

pub enum VariableLookupResult {
    OK((SymbolData<VariableData>, usize)),
    NOT_INITIALIZED(TextRange),
    Err,
}

#[derive(Debug)]
pub struct SymbolData<T>(pub Rc<RefCell<T>>, pub TextRange, pub bool); // (identifier_meta_data, decl_line_number, should_add_prefix)

impl<T> SymbolData<T> {
    pub fn new(core_data: T, decl_range: TextRange, is_suffix_required: bool) -> Self {
        SymbolData(
            Rc::new(RefCell::new(core_data)),
            decl_range,
            is_suffix_required,
        )
    }
}

impl<T> Clone for SymbolData<T> {
    fn clone(&self) -> Self {
        SymbolData(self.0.clone(), self.1, self.2)
    }
}

#[derive(Debug)]
pub struct CoreScope<T> {
    symbol_table: FxHashMap<Rc<String>, SymbolData<T>>,
    parent_scope: Option<Scope<T>>,
    non_locals: Rc<RefCell<FxHashMap<Rc<String>, Option<bool>>>>,
    is_global: bool,
}

impl<T> CoreScope<T> {
    fn set(
        &mut self,
        name: &Rc<String>,
        meta_data: T,
        decl_range: TextRange,
        is_suffix_required: bool,
    ) -> SymbolData<T> {
        let symbol_data = SymbolData(
            Rc::new(RefCell::new(meta_data)),
            decl_range,
            is_suffix_required,
        );
        self.symbol_table.insert(name.clone(), symbol_data.clone());
        symbol_data
    }

    pub fn get(&self, name: &Rc<String>) -> Option<&SymbolData<T>> {
        self.symbol_table.get(name)
    }

    pub fn set_to_non_locals(&self, name: &Rc<String>, is_global: Option<bool>) {
        self.non_locals
            .as_ref()
            .borrow_mut()
            .insert(name.clone(), is_global);
    }

    pub fn is_in_non_locals(&self, name: &Rc<String>) -> bool {
        self.non_locals.as_ref().borrow().get(name).is_some()
    }

    pub fn get_non_locals(&self) -> Rc<RefCell<FxHashMap<Rc<String>, Option<bool>>>> {
        self.non_locals.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Scope<T>(pub Rc<RefCell<CoreScope<T>>>);

impl<T> Scope<T> {
    fn new() -> Self {
        Scope(Rc::new(RefCell::new(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: None,
            non_locals: Rc::new(RefCell::new(FxHashMap::default())),
            is_global: true,
        })))
    }

    fn is_global(&self) -> bool {
        self.0.as_ref().borrow().is_global
    }

    fn new_with_parent_scope(parent_scope: &Scope<T>) -> Self {
        let scope = parent_scope.0.clone();
        Scope(Rc::new(RefCell::new(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: Some(Scope(scope)),
            non_locals: Rc::new(RefCell::new(FxHashMap::default())),
            is_global: false,
        })))
    }

    pub fn parent(&self) -> Option<Scope<T>> {
        match &self.0.as_ref().borrow().parent_scope {
            Some(parent_scope) => Some(Scope(parent_scope.0.clone())),
            None => None,
        }
    }

    pub fn force_insert(
        &self,
        key: &Rc<String>,
        meta_data: T,
        decl_range: TextRange,
        is_suffix_required: bool,
    ) -> SymbolData<T> {
        // use this method only for builtin function where we know that no entry already exist in the scope
        self.0
            .borrow_mut()
            .set(key, meta_data, decl_range, is_suffix_required)
    }

    pub fn insert<U: Fn(Scope<T>, Rc<String>) -> Option<SymbolData<T>>>(
        &self,
        key: &Rc<String>,
        meta_data: T,
        decl_range: TextRange,
        lookup_fn: U,
        is_suffix_required: bool,
    ) -> Result<SymbolData<T>, TextRange> {
        let scope = Scope(self.0.clone());
        if let Some(symbol_data) = lookup_fn(scope, key.clone()) {
            return Err(symbol_data.1);
        }
        let symbol_data = self
            .0
            .borrow_mut()
            .set(key, meta_data, decl_range, is_suffix_required);
        Ok(symbol_data)
    }

    pub fn get(&self, key: &Rc<String>) -> Option<SymbolData<T>> {
        match self.0.as_ref().borrow().get(key) {
            Some(symbol_data) => Some(symbol_data.clone()),
            None => None,
        }
    }

    // returns symbol table entry and depth of the scope starting from local scope up to parents
    fn lookup(&self, key: &Rc<String>) -> Option<(SymbolData<T>, usize, bool)> {
        let scope_ref = self.0.borrow();
        match scope_ref.get(key) {
            Some(value) => Some((value.clone(), 0, self.is_global())),
            None => {
                if let Some(parent_env) = &scope_ref.parent_scope {
                    match &parent_env.lookup(key) {
                        Some(result) => Some((result.0.clone(), result.1 + 1, result.2)),
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    fn set_to_non_locals(&self, name: &Rc<String>, is_global: Option<bool>) {
        self.0
            .as_ref()
            .borrow_mut()
            .set_to_non_locals(name, is_global);
    }

    fn is_in_non_locals(&self, name: &Rc<String>) -> bool {
        self.0.as_ref().borrow().is_in_non_locals(name)
    }
}

#[derive(Debug)]
pub struct Namespace {
    pub variables: Scope<VariableData>,
    pub types: Scope<UserDefinedTypeData>,
    pub functions: Scope<FunctionData>,
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

    pub fn variable_scope(&self) -> &Scope<VariableData> {
        &self.variables
    }

    pub fn type_scope(&self) -> &Scope<UserDefinedTypeData> {
        &self.types
    }

    pub fn function_scope(&self) -> &Scope<FunctionData> {
        &self.functions
    }

    pub fn lookup_in_variables_namespace(
        &self,
        key: &Rc<String>,
    ) -> Option<(SymbolData<VariableData>, usize, bool)> {
        self.variables.lookup(key)
    }

    pub fn lookup_in_variables_namespace_with_is_init(
        &self,
        key: &Rc<String>,
    ) -> VariableLookupResult {
        match self.variables.lookup(key) {
            Some((symbol_data, depth, is_global)) => {
                if symbol_data.0.as_ref().borrow().is_initialized() {
                    return VariableLookupResult::OK((symbol_data, depth));
                } else {
                    return VariableLookupResult::NOT_INITIALIZED(symbol_data.1.clone());
                }
            }
            None => return VariableLookupResult::Err,
        }
    }

    pub fn lookup_in_types_namespace(
        &self,
        key: &Rc<String>,
    ) -> Option<(SymbolData<UserDefinedTypeData>, usize, bool)> {
        self.types.lookup(key)
    }

    pub fn lookup_in_functions_namespace(
        &self,
        key: &Rc<String>,
    ) -> Option<(SymbolData<FunctionData>, usize, bool)> {
        // last on `is_global`
        self.functions.lookup(key)
    }

    pub fn declare_variable(
        &self,
        name: &Rc<String>,
        decl_range: TextRange,
    ) -> Result<SymbolData<VariableData>, TextRange> {
        let lookup_func =
            |scope: Scope<VariableData>, key: Rc<String>| match scope.0.as_ref().borrow().get(&key)
            {
                Some(symbol_data) => Some(symbol_data.clone()),
                None => None,
            };
        self.variables
            .insert(name, VariableData::default(), decl_range, lookup_func, true)
    }

    pub fn declare_variable_with_type(
        &self,
        name: &Rc<String>,
        variable_type: &Type,
        decl_range: TextRange,
        is_init: bool,
    ) -> Result<SymbolData<VariableData>, TextRange> {
        let lookup_func =
            |scope: Scope<VariableData>, key: Rc<String>| match scope.0.as_ref().borrow().get(&key)
            {
                Some(symbol_data) => Some(symbol_data.clone()),
                None => None,
            };
        self.variables.insert(
            name,
            VariableData::new(variable_type, is_init),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn declare_function(
        &self,
        name: &Rc<String>,
        decl_range: TextRange,
    ) -> Result<SymbolData<FunctionData>, TextRange> {
        let lookup_func =
            |scope: Scope<FunctionData>, key: Rc<String>| match scope.0.as_ref().borrow().get(&key)
            {
                Some(symbol_data) => Some(symbol_data.clone()),
                None => None,
            };
        self.functions
            .insert(name, FunctionData::default(), decl_range, lookup_func, true)
    }

    pub fn declare_struct_type(
        &self,
        name: &Rc<String>,
        decl_range: TextRange,
    ) -> Result<SymbolData<UserDefinedTypeData>, TextRange> {
        let lookup_func =
            |scope: Scope<UserDefinedTypeData>, key: Rc<String>| match scope.lookup(&key) {
                Some((symbol_data, _, _)) => Some(symbol_data),
                None => None,
            };
        self.types.insert(
            name,
            UserDefinedTypeData::default_with_struct(),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn declare_lambda_type(
        &self,
        name: &Rc<String>,
        decl_range: TextRange,
    ) -> Result<SymbolData<UserDefinedTypeData>, TextRange> {
        let lookup_func =
            |scope: Scope<UserDefinedTypeData>, key: Rc<String>| match scope.lookup(&key) {
                Some((symbol_data, _, _)) => Some(symbol_data),
                None => None,
            };
        self.types.insert(
            name,
            UserDefinedTypeData::default_with_lambda(),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn declare_lambda_type_with_meta_data(
        &self,
        name: &Rc<String>,
        param_types: Vec<Type>,
        return_type: Type,
        decl_range: TextRange,
    ) -> Result<SymbolData<UserDefinedTypeData>, TextRange> {
        let lookup_func =
            |scope: Scope<UserDefinedTypeData>, key: Rc<String>| match scope.lookup(&key) {
                Some((symbol_data, _, _)) => Some(symbol_data),
                None => None,
            };
        self.types.insert(
            name,
            UserDefinedTypeData::LAMBDA(LambdaTypeData {
                param_types: Rc::new(param_types),
                return_type,
            }),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn set_to_variable_non_locals(&self, name: &Rc<String>) {
        // variables are never resolved to global declarations as they are not allowed in Jarvil
        self.variables.set_to_non_locals(name, None);
    }

    pub fn is_variable_in_non_locals(&self, name: &Rc<String>) -> bool {
        self.variables.is_in_non_locals(name)
    }

    pub fn set_to_function_non_locals(&self, name: &Rc<String>, is_global: bool) {
        self.functions.set_to_non_locals(name, Some(is_global));
    }

    pub fn is_function_in_non_locals(&self, name: &Rc<String>) -> bool {
        self.functions.is_in_non_locals(name)
    }
}

impl Clone for Namespace {
    fn clone(&self) -> Self {
        Namespace {
            variables: self.variables.clone(),
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