use super::function::FunctionData;
use super::user_defined_types::LambdaTypeData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::scope::variables::VariableData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
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
    symbol_table: FxHashMap<String, SymbolData<T>>,
    parent_scope: Option<usize>, // points to the index in the global flattened scope vec
    non_locals: Rc<RefCell<FxHashMap<String, Option<bool>>>>,
    is_global: bool,
}

impl<T> CoreScope<T> {
    fn set(
        &mut self,
        name: String,
        meta_data: T,
        decl_range: TextRange,
        is_suffix_required: bool,
    ) -> SymbolData<T> {
        let symbol_data = SymbolData(
            Rc::new(RefCell::new(meta_data)),
            decl_range,
            is_suffix_required,
        );
        self.symbol_table.insert(name, symbol_data.clone());
        symbol_data
    }

    pub fn get(&self, name: &str) -> Option<&SymbolData<T>> {
        self.symbol_table.get(name)
    }

    fn lookup(
        &self,
        key: &str,
        global_scope_vec: &Vec<CoreScope<T>>,
    ) -> Option<(SymbolData<T>, usize, bool)> {
        match self.get(key) {
            Some(value) => Some((value.clone(), 0, self.is_global)),
            None => {
                if let Some(parent_scope_index) = self.parent_scope {
                    match &global_scope_vec[parent_scope_index].lookup(key, global_scope_vec) {
                        Some(result) => Some((result.0.clone(), result.1 + 1, result.2)),
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn set_to_non_locals(&self, name: String, is_global: Option<bool>) {
        self.non_locals
            .as_ref()
            .borrow_mut()
            .insert(name, is_global);
    }

    pub fn is_in_non_locals(&self, name: &str) -> bool {
        self.non_locals.as_ref().borrow().get(name).is_some()
    }

    pub fn get_non_locals(&self) -> Rc<RefCell<FxHashMap<String, Option<bool>>>> {
        self.non_locals.clone()
    }
}

#[derive(Debug)]
// pub struct Scope<T>(pub Rc<RefCell<CoreScope<T>>>);
pub struct Scope<T> {
    flattened_vec: Vec<CoreScope<T>>,
}

impl<T> Scope<T> {
    fn new() -> Self {
        Scope {
            flattened_vec: vec![CoreScope {
                symbol_table: FxHashMap::default(),
                parent_scope: None,
                non_locals: Rc::new(RefCell::new(FxHashMap::default())),
                is_global: true,
            }],
        }
    }

    fn add_new_scope(&mut self, parent_scope_index: usize) -> usize {
        // let scope = parent_scope.0.clone();
        let new_scope_index = self.flattened_vec.len();
        self.flattened_vec.push(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: Some(parent_scope_index),
            non_locals: Rc::new(RefCell::new(FxHashMap::default())),
            is_global: false,
        });
        return new_scope_index;
    }

    fn is_global(&self, scope_index: usize) -> bool {
        self.flattened_vec[scope_index].is_global
    }

    pub fn parent(&self, scope_index: usize) -> Option<usize> {
        match self.flattened_vec[scope_index].parent_scope {
            Some(parent_scope_index) => Some(parent_scope_index),
            None => None,
        }
    }

    pub fn force_insert(
        &mut self,
        scope_index: usize,
        key: String,
        meta_data: T,
        decl_range: TextRange,
        is_suffix_required: bool,
    ) -> SymbolData<T> {
        // use this method only for builtin function where we know that no entry already exist in the scope
        self.flattened_vec[scope_index].set(key, meta_data, decl_range, is_suffix_required)
    }

    pub fn insert<U: Fn(&Scope<T>, usize, &str) -> Option<SymbolData<T>>>(
        &mut self,
        scope_index: usize,
        key: String,
        meta_data: T,
        decl_range: TextRange,
        lookup_fn: U,
        is_suffix_required: bool,
    ) -> Result<SymbolData<T>, (String, TextRange)> {
        if let Some(symbol_data) = lookup_fn(self, scope_index, &key) {
            return Err((key, symbol_data.1));
        }
        let symbol_data =
            self.flattened_vec[scope_index].set(key, meta_data, decl_range, is_suffix_required);
        Ok(symbol_data)
    }

    pub fn get(&self, scope_index: usize, key: &str) -> Option<SymbolData<T>> {
        match self.flattened_vec[scope_index].get(key) {
            Some(symbol_data) => Some(symbol_data.clone()),
            None => None,
        }
    }

    fn lookup(&self, scope_index: usize, key: &str) -> Option<(SymbolData<T>, usize, bool)> {
        self.flattened_vec[scope_index].lookup(key, &self.flattened_vec)
    }

    fn set_to_non_locals(&self, scope_index: usize, name: String, is_global: Option<bool>) {
        self.flattened_vec[scope_index].set_to_non_locals(name, is_global);
    }

    fn is_in_non_locals(&self, scope_index: usize, name: &str) -> bool {
        self.flattened_vec[scope_index].is_in_non_locals(name)
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

    pub fn open_scope(&mut self, curr_scope_index: usize) -> usize {
        self.variables.add_new_scope(curr_scope_index);
        self.types.add_new_scope(curr_scope_index);
        self.functions.add_new_scope(curr_scope_index)
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
        scope_index: usize,
        key: &str,
    ) -> Option<(SymbolData<VariableData>, usize, bool)> {
        self.variables.lookup(scope_index, key)
    }

    pub fn lookup_in_variables_namespace_with_is_init(
        &self,
        scope_index: usize,
        key: &str,
    ) -> VariableLookupResult {
        match self.variables.lookup(scope_index, key) {
            Some((symbol_data, depth, _)) => {
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
        scope_index: usize,
        key: &str,
    ) -> Option<(SymbolData<UserDefinedTypeData>, usize, bool)> {
        self.types.lookup(scope_index, key)
    }

    pub fn lookup_in_functions_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(SymbolData<FunctionData>, usize, bool)> {
        self.functions.lookup(scope_index, key)
    }

    pub fn declare_variable(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<SymbolData<VariableData>, (String, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &str| match scope
            .flattened_vec[scope_index]
            .get(key)
        {
            Some(symbol_data) => Some(symbol_data.clone()),
            None => None,
        };
        self.variables.insert(
            scope_index,
            name,
            VariableData::default(),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn declare_variable_with_type(
        &mut self,
        scope_index: usize,
        name: String,
        variable_type: &Type,
        decl_range: TextRange,
        is_init: bool,
    ) -> Result<SymbolData<VariableData>, (String, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &str| match scope
            .flattened_vec[scope_index]
            .get(key)
        {
            Some(symbol_data) => Some(symbol_data.clone()),
            None => None,
        };
        self.variables.insert(
            scope_index,
            name,
            VariableData::new(variable_type, is_init),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn declare_function(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<SymbolData<FunctionData>, (String, TextRange)> {
        let lookup_func = |scope: &Scope<FunctionData>, scope_index: usize, key: &str| match scope
            .flattened_vec[scope_index]
            .get(key)
        {
            Some(symbol_data) => Some(symbol_data.clone()),
            None => None,
        };
        self.functions.insert(
            scope_index,
            name,
            FunctionData::default(),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn declare_struct_type(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<SymbolData<UserDefinedTypeData>, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope<UserDefinedTypeData>, scope_index: usize, key: &str| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data, _, _)) => Some(symbol_data),
                None => None,
            };
        self.types.insert(
            scope_index,
            name,
            UserDefinedTypeData::default_with_struct(),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn declare_lambda_type(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<SymbolData<UserDefinedTypeData>, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope<UserDefinedTypeData>, scope_index: usize, key: &str| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data, _, _)) => Some(symbol_data),
                None => None,
            };
        self.types.insert(
            scope_index,
            name,
            UserDefinedTypeData::default_with_lambda(),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn declare_lambda_type_with_meta_data(
        &mut self,
        scope_index: usize,
        name: String,
        param_types: Vec<Type>,
        return_type: Type,
        decl_range: TextRange,
    ) -> Result<SymbolData<UserDefinedTypeData>, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope<UserDefinedTypeData>, scope_index: usize, key: &str| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data, _, _)) => Some(symbol_data),
                None => None,
            };
        self.types.insert(
            scope_index,
            name,
            UserDefinedTypeData::LAMBDA(LambdaTypeData {
                meta_data: FunctionData {
                    params: Rc::new(param_types),
                    return_type,
                },
            }),
            decl_range,
            lookup_func,
            true,
        )
    }

    pub fn set_to_variable_non_locals(&self, scope_index: usize, name: String) {
        // variables are never resolved to global declarations as they are not allowed in Jarvil
        self.variables.set_to_non_locals(scope_index, name, None);
    }

    pub fn is_variable_in_non_locals(&self, scope_index: usize, name: &str) -> bool {
        self.variables.is_in_non_locals(scope_index, name)
    }

    pub fn set_to_function_non_locals(&self, scope_index: usize, name: String, is_global: bool) {
        self.functions
            .set_to_non_locals(scope_index, name, Some(is_global));
    }

    pub fn is_function_in_non_locals(&self, scope_index: usize, name: &str) -> bool {
        self.functions.is_in_non_locals(scope_index, name)
    }
}

impl Default for Namespace {
    fn default() -> Self {
        Namespace::new()
    }
}
