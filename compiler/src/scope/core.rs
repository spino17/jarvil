use super::concrete::core::ConcreteTypesRegistryKey;
use super::function::{CallableData, CallableKind, CallablePrototypeData};
use super::handler::SymbolDataEntry;
use super::interfaces::{InterfaceData, InterfaceObject};
use super::types::lambda_type::LambdaTypeData;
use crate::scope::types::core::UserDefinedTypeData;
use crate::scope::variables::VariableData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use text_size::TextRange;

pub enum VariableLookupResult {
    Ok((SymbolData<VariableData>, usize, usize)),
    NotInitialized(TextRange),
    Err,
}

pub trait AbstractConcreteTypesHandler {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey;
    fn has_generics(&self) -> bool;
}

pub enum ConcreteTypesRegistrationKind {
    Primary,
    Method,
}

#[derive(Debug)]
pub struct GenericTypeParams(Vec<(String, Vec<InterfaceObject>)>);

#[derive(Debug)]
pub struct SymbolDataCore<T: AbstractConcreteTypesHandler>(pub Rc<RefCell<T>>);

impl<T: AbstractConcreteTypesHandler> Clone for SymbolDataCore<T> {
    fn clone(&self) -> Self {
        SymbolDataCore(self.0.clone())
    }
}

#[derive(Debug)]
pub struct SymbolData<T: AbstractConcreteTypesHandler>(
    pub SymbolDataCore<T>,
    pub TextRange,
    pub bool,
); // (identifier_meta_data, decl_line_number, should_add_prefix)

impl<T: AbstractConcreteTypesHandler> SymbolData<T> {
    pub fn new(core_data: T, decl_range: TextRange, is_suffix_required: bool) -> Self {
        SymbolData(
            SymbolDataCore(Rc::new(RefCell::new(core_data))),
            decl_range,
            is_suffix_required,
        )
    }

    pub fn register_concrete_types(&self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        self.0
             .0
            .as_ref()
            .borrow_mut()
            .register_concrete_types(concrete_types)
    }

    pub fn has_generics(&self) -> bool {
        self.0 .0.as_ref().borrow().has_generics()
    }
}

impl<T: AbstractConcreteTypesHandler> Clone for SymbolData<T> {
    fn clone(&self) -> Self {
        SymbolData(self.0.clone(), self.1, self.2)
    }
}

#[derive(Debug)]
pub struct CoreScope<T: AbstractConcreteTypesHandler> {
    symbol_table: FxHashMap<String, SymbolData<T>>,
    pub parent_scope: Option<usize>, // points to the index in the global flattened scope vec
    is_global: bool,
}

impl<T: AbstractConcreteTypesHandler> CoreScope<T> {
    fn set(
        &mut self,
        name: String,
        meta_data: T,
        decl_range: TextRange,
        is_suffix_required: bool,
    ) -> SymbolData<T> {
        let symbol_data = SymbolData::new(meta_data, decl_range, is_suffix_required);
        self.symbol_table.insert(name, symbol_data.clone());
        symbol_data
    }

    pub fn get(&self, name: &str) -> Option<&SymbolData<T>> {
        self.symbol_table.get(name)
    }

    pub fn lookup(
        &self,
        scope_index: usize,
        key: &str,
        global_scope_vec: &Vec<CoreScope<T>>,
    ) -> Option<(SymbolData<T>, usize, usize, bool)> {
        // (symbol_data, scope_index, depth, is_global)
        match self.get(key) {
            Some(value) => Some((value.clone(), scope_index, 0, self.is_global)),
            None => {
                if let Some(parent_scope_index) = self.parent_scope {
                    match global_scope_vec[parent_scope_index].lookup(
                        parent_scope_index,
                        key,
                        global_scope_vec,
                    ) {
                        Some((symbol_data, resolved_scope_index, depth, range)) => {
                            Some((symbol_data, resolved_scope_index, depth + 1, range))
                        }
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn lookup_and_get_symbol_data_ref<'a>(
        &'a self,
        scope_index: usize,
        key: &str,
        global_scope_vec: &'a Vec<CoreScope<T>>,
    ) -> Option<(&SymbolData<T>, usize, usize, bool)> {
        match self.get(key) {
            Some(value) => return Some((value, scope_index, 0, self.is_global)),
            None => {
                let mut curr_scope = self;
                let mut depth: usize = 1;
                while let Some(parent_scope_index) = curr_scope.parent_scope {
                    curr_scope = &global_scope_vec[parent_scope_index];
                    if let Some(symbol_data) = curr_scope.get(key) {
                        return Some((
                            symbol_data,
                            parent_scope_index,
                            depth,
                            curr_scope.is_global,
                        ));
                    }
                    depth = depth + 1;
                }
                return None;
            }
        }
    }
}

#[derive(Debug)]
pub struct Scope<T: AbstractConcreteTypesHandler> {
    pub flattened_vec: Vec<CoreScope<T>>,
}

impl<T: AbstractConcreteTypesHandler> Scope<T> {
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

    pub fn force_insert(
        &mut self,
        scope_index: usize,
        key: String,
        meta_data: T,
        decl_range: TextRange,
        is_suffix_required: bool,
    ) {
        // use this method only for builtin function where we know that no entry already exist in the scope
        self.flattened_vec[scope_index].set(key, meta_data, decl_range, is_suffix_required);
    }

    pub fn insert<U: Fn(&Scope<T>, usize, &str) -> Option<TextRange>>(
        &mut self,
        scope_index: usize,
        key: String,
        meta_data: T,
        decl_range: TextRange,
        lookup_fn: U,
        is_suffix_required: bool,
    ) -> Result<SymbolData<T>, (String, TextRange)> {
        if let Some(previous_decl_range) = lookup_fn(self, scope_index, &key) {
            return Err((key, previous_decl_range));
        }
        let symbol_data =
            self.flattened_vec[scope_index].set(key, meta_data, decl_range, is_suffix_required);
        Ok(symbol_data)
    }

    pub fn get(&self, scope_index: usize, key: &str) -> Option<&SymbolData<T>> {
        self.flattened_vec[scope_index].get(key)
    }

    fn lookup(&self, scope_index: usize, key: &str) -> Option<(SymbolData<T>, usize, usize, bool)> {
        self.flattened_vec[scope_index].lookup(scope_index, key, &self.flattened_vec)
    }

    pub fn lookup_and_get_symbol_data_ref(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(SymbolData<T>, usize, usize, bool)> {
        match self.flattened_vec[scope_index].lookup_and_get_symbol_data_ref(
            scope_index,
            key,
            &self.flattened_vec,
        ) {
            Some((symbol_data, scope_index, depth, is_global)) => {
                Some((symbol_data.clone(), scope_index, depth, is_global))
            }
            None => None,
        }
    }
}

#[derive(Debug)]
pub enum NamespaceKind {
    Variable,
    Type,
    Function,
}

#[derive(Debug)]
pub struct Namespace {
    pub variables: Scope<VariableData>,
    pub types: Scope<UserDefinedTypeData>,
    pub functions: Scope<CallableData>,
    pub interfaces: Scope<InterfaceData>,
}

impl Namespace {
    pub fn new() -> Self {
        Namespace {
            variables: Scope::new(),
            types: Scope::new(),
            functions: Scope::new(),
            interfaces: Scope::new(),
        }
    }

    pub fn parent_scope_index(&self, scope_index: usize) -> Option<usize> {
        self.variables.flattened_vec[scope_index].parent_scope
    }

    pub fn open_scope(&mut self, curr_scope_index: usize) -> usize {
        self.variables.add_new_scope(curr_scope_index);
        self.types.add_new_scope(curr_scope_index);
        self.functions.add_new_scope(curr_scope_index);
        self.interfaces.add_new_scope(curr_scope_index)
    }

    pub fn get_from_variables_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<&SymbolData<VariableData>> {
        self.variables.get(scope_index, key)
    }

    pub fn get_from_functions_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<&SymbolData<CallableData>> {
        self.functions.get(scope_index, key)
    }

    pub fn get_from_types_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<&SymbolData<UserDefinedTypeData>> {
        self.types.get(scope_index, key)
    }

    pub fn get_from_interfaces_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<&SymbolData<InterfaceData>> {
        self.interfaces.get(scope_index, key)
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
            Some((symbol_data, resolved_scope_index, depth, _)) => {
                if symbol_data.0 .0.as_ref().borrow().is_initialized() {
                    return VariableLookupResult::Ok((symbol_data, resolved_scope_index, depth));
                } else {
                    return VariableLookupResult::NotInitialized(symbol_data.1);
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

    pub fn lookup_in_interfaces_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(SymbolData<InterfaceData>, usize, usize, bool)> {
        self.interfaces.lookup(scope_index, key)
    }

    pub fn lookup_in_functions_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(SymbolData<CallableData>, usize, usize, bool)> {
        self.functions.lookup(scope_index, key)
    }

    pub fn declare_variable(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &str| match scope
            .flattened_vec[scope_index]
            .get(key)
        {
            Some(symbol_data) => Some(symbol_data.1),
            None => None,
        };
        match self.variables.insert(
            scope_index,
            name,
            VariableData::default(),
            decl_range,
            lookup_func,
            true,
        ) {
            Ok(symbol_data) => return Ok(SymbolDataEntry::Variable(symbol_data)),
            Err(err) => return Err(err),
        }
    }

    pub fn declare_variable_with_type(
        &mut self,
        scope_index: usize,
        name: String,
        variable_type: &Type,
        decl_range: TextRange,
        is_init: bool,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &str| match scope
            .flattened_vec[scope_index]
            .get(key)
        {
            Some(symbol_data) => Some(symbol_data.1),
            None => None,
        };
        match self.variables.insert(
            scope_index,
            name,
            VariableData::new(variable_type, is_init),
            decl_range,
            lookup_func,
            true,
        ) {
            Ok(symbol_data) => return Ok(SymbolDataEntry::Variable(symbol_data)),
            Err(err) => return Err(err),
        }
    }

    pub fn declare_function(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func = |scope: &Scope<CallableData>, scope_index: usize, key: &str| match scope
            .flattened_vec[scope_index]
            .get(key)
        {
            Some(symbol_data) => Some(symbol_data.1),
            None => None,
        };
        match self.functions.insert(
            scope_index,
            name,
            CallableData::default_for_kind(CallableKind::Function),
            decl_range,
            lookup_func,
            true,
        ) {
            Ok(symbol_data) => return Ok(SymbolDataEntry::Function(symbol_data)),
            Err(err) => return Err(err),
        }
    }

    pub fn declare_struct_type(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope<UserDefinedTypeData>, scope_index: usize, key: &str| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data, _, _, _)) => Some(symbol_data.1),
                None => None,
            };
        match self.types.insert(
            scope_index,
            name,
            UserDefinedTypeData::default_with_struct(),
            decl_range,
            lookup_func,
            true,
        ) {
            Ok(symbol_data) => return Ok(SymbolDataEntry::Type(symbol_data)),
            Err(err) => return Err(err),
        }
    }

    pub fn declare_lambda_type_with_meta_data(
        &mut self,
        scope_index: usize,
        name: String,
        param_types: Vec<Type>,
        return_type: Type,
        decl_range: TextRange,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope<UserDefinedTypeData>, scope_index: usize, key: &str| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data, _, _, _)) => Some(symbol_data.1),
                None => None,
            };
        match self.types.insert(
            scope_index,
            name,
            UserDefinedTypeData::Lambda(LambdaTypeData {
                prototype: CallablePrototypeData {
                    params: param_types,
                    return_type,
                },
                generics: None,
            }),
            decl_range,
            lookup_func,
            true,
        ) {
            Ok(symbol_data) => return Ok(SymbolDataEntry::Type(symbol_data)),
            Err(err) => return Err(err),
        }
    }

    pub fn declare_interface(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func = |scope: &Scope<InterfaceData>, scope_index: usize, key: &str| match scope
            .lookup(scope_index, key)
        {
            Some((symbol_data, _, _, _)) => Some(symbol_data.1),
            None => None,
        };
        match self.interfaces.insert(
            scope_index,
            name,
            InterfaceData::default(),
            decl_range,
            lookup_func,
            true,
        ) {
            Ok(symbol_data) => return Ok(SymbolDataEntry::Interface(symbol_data)),
            Err(err) => return Err(err),
        }
    }
}

impl Default for Namespace {
    fn default() -> Self {
        Namespace::new()
    }
}
