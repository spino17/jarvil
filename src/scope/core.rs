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
        scope_index: usize,
        key: &str,
        global_scope_vec: &Vec<CoreScope<T>>,
    ) -> Option<(SymbolData<T>, usize, usize, bool)> {  // (symbol_data, scope_index, depth, is_global)
        match self.get(key) {
            Some(value) => Some((value.clone(), scope_index, 0, self.is_global)),
            None => {
                if let Some(parent_scope_index) = self.parent_scope {
                    match &global_scope_vec[parent_scope_index].lookup(parent_scope_index, key, global_scope_vec) {
                        Some(result) => Some((result.0.clone(), result.1, result.2 + 1, result.3)),
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Scope<T> {
    flattened_vec: Vec<CoreScope<T>>,
}

impl<T> Scope<T> {
    fn new() -> Self {
        Scope {
            flattened_vec: vec![CoreScope {
                symbol_table: FxHashMap::default(),
                parent_scope: None,
                is_global: true,
            }],
        }
    }

    fn add_new_scope(&mut self, parent_scope_index: usize) -> usize {
        let new_scope_index = self.flattened_vec.len();
        self.flattened_vec.push(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: Some(parent_scope_index),
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

    fn lookup(&self, scope_index: usize, key: &str) -> Option<(SymbolData<T>, usize, usize, bool)> {
        self.flattened_vec[scope_index].lookup(scope_index, key, &self.flattened_vec)
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

    pub fn parent_scope_index(&self, scope_index: usize) -> Option<usize> {
        self.variables.flattened_vec[scope_index].parent_scope
    }

    pub fn open_scope(&mut self, curr_scope_index: usize) -> usize {
        self.variables.add_new_scope(curr_scope_index);
        self.types.add_new_scope(curr_scope_index);
        self.functions.add_new_scope(curr_scope_index)
    }

    pub fn lookup_in_variables_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(SymbolData<VariableData>, usize, usize, bool)> {
        self.variables.lookup(scope_index, key)
    }

    pub fn lookup_in_variables_namespace_with_is_init(
        &self,
        scope_index: usize,
        key: &str,
    ) -> VariableLookupResult {
        match self.variables.lookup(scope_index, key) {
            Some((symbol_data, _, depth, _)) => {
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
    ) -> Option<(SymbolData<UserDefinedTypeData>, usize, usize, bool)> {
        self.types.lookup(scope_index, key)
    }

    pub fn lookup_in_functions_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(SymbolData<FunctionData>, usize, usize, bool)> {
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
                Some((symbol_data, _, _, _)) => Some(symbol_data),
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
                Some((symbol_data, _, _, _)) => Some(symbol_data),
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
                Some((symbol_data, _, _, _)) => Some(symbol_data),
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
}

impl Default for Namespace {
    fn default() -> Self {
        Namespace::new()
    }
}
