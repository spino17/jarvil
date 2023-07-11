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
    Ok((usize, usize, usize)),
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

    pub fn get_core_ref(&self) -> &T {
        todo!()
    }

    pub fn get_core_mut_ref(&mut self) -> &mut T {
        todo!()
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
pub struct SymbolDataRegistry<T: AbstractConcreteTypesHandler>(Vec<SymbolData<T>>);

impl<T: AbstractConcreteTypesHandler> SymbolDataRegistry<T> {
    pub fn register_symbol_data(&mut self, symbol_data: SymbolData<T>) -> usize {
        let index = self.0.len();
        self.0.push(symbol_data);
        index
    }

    pub fn get_symbol_data_ref_at_index(&self, index: usize) -> &SymbolData<T> {
        &self.0[index]
    }

    pub fn get_symbol_data_mut_ref_at_index(&mut self, index: usize) -> &mut SymbolData<T> {
        &mut self.0[index]
    }
}

impl<T: AbstractConcreteTypesHandler> Default for SymbolDataRegistry<T> {
    fn default() -> Self {
        SymbolDataRegistry(vec![])
    }
}

#[derive(Debug, Default)]
pub struct GlobalSymbolDataRegistry {
    pub variables_registry: SymbolDataRegistry<VariableData>,
    pub functions_registry: SymbolDataRegistry<CallableData>,
    pub types_registry: SymbolDataRegistry<UserDefinedTypeData>,
    pub interfaces_registry: SymbolDataRegistry<InterfaceData>,
}

impl GlobalSymbolDataRegistry {
    pub fn register_variables_symbol_data(
        &mut self,
        symbol_data: SymbolData<VariableData>,
    ) -> usize {
        self.variables_registry.register_symbol_data(symbol_data)
    }

    pub fn get_variables_symbol_data_ref_at_index(
        &self,
        index: usize,
    ) -> &SymbolData<VariableData> {
        self.variables_registry.get_symbol_data_ref_at_index(index)
    }

    pub fn get_variables_symbol_data_mut_ref_at_index(
        &mut self,
        index: usize,
    ) -> &mut SymbolData<VariableData> {
        self.variables_registry
            .get_symbol_data_mut_ref_at_index(index)
    }

    pub fn register_functions_symbol_data(
        &mut self,
        symbol_data: SymbolData<CallableData>,
    ) -> usize {
        self.functions_registry.register_symbol_data(symbol_data)
    }

    pub fn get_functions_symbol_data_ref_at_index(
        &self,
        index: usize,
    ) -> &SymbolData<CallableData> {
        self.functions_registry.get_symbol_data_ref_at_index(index)
    }

    pub fn get_functions_symbol_data_mut_ref_at_index(
        &mut self,
        index: usize,
    ) -> &mut SymbolData<CallableData> {
        self.functions_registry
            .get_symbol_data_mut_ref_at_index(index)
    }

    pub fn register_types_symbol_data(
        &mut self,
        symbol_data: SymbolData<UserDefinedTypeData>,
    ) -> usize {
        self.types_registry.register_symbol_data(symbol_data)
    }

    pub fn get_types_symbol_data_ref_at_index(
        &self,
        index: usize,
    ) -> &SymbolData<UserDefinedTypeData> {
        self.types_registry.get_symbol_data_ref_at_index(index)
    }

    pub fn get_types_symbol_data_mut_ref_at_index(
        &mut self,
        index: usize,
    ) -> &mut SymbolData<UserDefinedTypeData> {
        self.types_registry.get_symbol_data_mut_ref_at_index(index)
    }

    pub fn register_interfaces_symbol_data(
        &mut self,
        symbol_data: SymbolData<InterfaceData>,
    ) -> usize {
        self.interfaces_registry.register_symbol_data(symbol_data)
    }

    pub fn get_interfaces_symbol_data_ref_at_index(
        &self,
        index: usize,
    ) -> &SymbolData<InterfaceData> {
        self.interfaces_registry.get_symbol_data_ref_at_index(index)
    }

    pub fn get_interfaces_symbol_data_mut_ref_at_index(
        &mut self,
        index: usize,
    ) -> &mut SymbolData<InterfaceData> {
        self.interfaces_registry
            .get_symbol_data_mut_ref_at_index(index)
    }
}

#[derive(Debug)]
pub struct CoreScope {
    symbol_table: FxHashMap<String, usize>,
    pub parent_scope: Option<usize>, // points to the index in the global flattened scope vec
    is_global: bool,
}

impl CoreScope {
    fn set(&mut self, name: String, symbol_data_index: usize) {
        self.symbol_table.insert(name, symbol_data_index);
    }

    pub fn get(&self, name: &str) -> Option<&usize> {
        self.symbol_table.get(name)
    }

    pub fn lookup(
        &self,
        scope_index: usize,
        key: &str,
        global_scope_vec: &Vec<CoreScope>,
    ) -> Option<(usize, usize, usize, bool)> {
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
}

#[derive(Debug)]
pub struct Scope {
    pub flattened_vec: Vec<CoreScope>,
}

impl Scope {
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

    pub fn force_insert<T: AbstractConcreteTypesHandler>(
        &mut self,
        scope_index: usize,
        key: String,
        meta_data: T,
        decl_range: TextRange,
        is_suffix_required: bool,
        symbol_data_registry: &mut SymbolDataRegistry<T>,
    ) {
        // use this method only for builtin function where we know that no entry already exist in the scope
        let symbol_data = SymbolData::new(meta_data, decl_range, is_suffix_required);
        let symbol_data_index = symbol_data_registry.register_symbol_data(symbol_data);
        self.flattened_vec[scope_index].set(key, symbol_data_index);
    }

    pub fn insert<
        T: AbstractConcreteTypesHandler,
        U: Fn(&Scope, usize, &str, &SymbolDataRegistry<T>) -> Option<TextRange>,
    >(
        &mut self,
        scope_index: usize,
        key: String,
        meta_data: T,
        decl_range: TextRange,
        lookup_fn: U,
        is_suffix_required: bool,
        symbol_data_registry: &mut SymbolDataRegistry<T>,
    ) -> Result<usize, (String, TextRange)> {
        if let Some(previous_decl_range) = lookup_fn(self, scope_index, &key, &symbol_data_registry)
        {
            return Err((key, previous_decl_range));
        }
        let symbol_data = SymbolData::new(meta_data, decl_range, is_suffix_required);
        let symbol_data_index = symbol_data_registry.register_symbol_data(symbol_data);
        self.flattened_vec[scope_index].set(key, symbol_data_index);
        Ok(symbol_data_index)
    }

    pub fn get(&self, scope_index: usize, key: &str) -> Option<&usize> {
        self.flattened_vec[scope_index].get(key)
    }

    pub fn lookup(&self, scope_index: usize, key: &str) -> Option<(usize, usize, usize, bool)> {
        self.flattened_vec[scope_index].lookup(scope_index, key, &self.flattened_vec)
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
    pub variables: Scope,
    pub types: Scope,
    pub functions: Scope,
    pub interfaces: Scope,
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

    pub fn get_from_variables_namespace(&self, scope_index: usize, key: &str) -> Option<&usize> {
        self.variables.get(scope_index, key)
    }

    pub fn get_from_functions_namespace(&self, scope_index: usize, key: &str) -> Option<&usize> {
        self.functions.get(scope_index, key)
    }

    pub fn get_from_types_namespace(&self, scope_index: usize, key: &str) -> Option<&usize> {
        self.types.get(scope_index, key)
    }

    pub fn get_from_interfaces_namespace(&self, scope_index: usize, key: &str) -> Option<&usize> {
        self.interfaces.get(scope_index, key)
    }

    pub fn lookup_in_variables_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(usize, usize, usize, bool)> {
        self.variables.lookup(scope_index, key)
    }

    pub fn lookup_in_variables_namespace_with_is_init(
        &self,
        scope_index: usize,
        key: &str,
        symbol_data_registry: &GlobalSymbolDataRegistry,
    ) -> VariableLookupResult {
        match self.variables.lookup(scope_index, key) {
            Some((symbol_data_index, resolved_scope_index, depth, _)) => {
                let symbol_data =
                    symbol_data_registry.get_variables_symbol_data_ref_at_index(symbol_data_index);
                if symbol_data.0 .0.as_ref().borrow().is_initialized() {
                    return VariableLookupResult::Ok((
                        symbol_data_index,
                        resolved_scope_index,
                        depth,
                    ));
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
    ) -> Option<(usize, usize, usize, bool)> {
        self.types.lookup(scope_index, key)
    }

    pub fn lookup_in_interfaces_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(usize, usize, usize, bool)> {
        self.interfaces.lookup(scope_index, key)
    }

    pub fn lookup_in_functions_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> Option<(usize, usize, usize, bool)> {
        self.functions.lookup(scope_index, key)
    }

    pub fn declare_variable(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
        symbol_data_registry: &mut GlobalSymbolDataRegistry,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope,
             scope_index: usize,
             key: &str,
             symbol_data_registry: &SymbolDataRegistry<VariableData>| match scope
                .flattened_vec[scope_index]
                .get(key)
            {
                Some(&symbol_data_index) => {
                    let symbol_data =
                        symbol_data_registry.get_symbol_data_ref_at_index(symbol_data_index);
                    Some(symbol_data.1)
                }
                None => None,
            };
        match self.variables.insert(
            scope_index,
            name,
            VariableData::default(),
            decl_range,
            lookup_func,
            true,
            &mut symbol_data_registry.variables_registry,
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
        symbol_data_registry: &mut GlobalSymbolDataRegistry,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope,
             scope_index: usize,
             key: &str,
             symbol_data_registry: &SymbolDataRegistry<VariableData>| match scope
                .flattened_vec[scope_index]
                .get(key)
            {
                Some(&symbol_data_index) => {
                    let symbol_data =
                        symbol_data_registry.get_symbol_data_ref_at_index(symbol_data_index);
                    Some(symbol_data.1)
                }
                None => None,
            };
        match self.variables.insert(
            scope_index,
            name,
            VariableData::new(variable_type, is_init),
            decl_range,
            lookup_func,
            true,
            &mut symbol_data_registry.variables_registry,
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
        symbol_data_registry: &mut GlobalSymbolDataRegistry,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope,
             scope_index: usize,
             key: &str,
             symbol_data_registry: &SymbolDataRegistry<CallableData>| match scope
                .flattened_vec[scope_index]
                .get(key)
            {
                Some(&symbol_data_index) => {
                    let symbol_data =
                        symbol_data_registry.get_symbol_data_ref_at_index(symbol_data_index);
                    Some(symbol_data.1)
                }
                None => None,
            };
        match self.functions.insert(
            scope_index,
            name,
            CallableData::default_for_kind(CallableKind::Function),
            decl_range,
            lookup_func,
            true,
            &mut symbol_data_registry.functions_registry,
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
        symbol_data_registry: &mut GlobalSymbolDataRegistry,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope,
             scope_index: usize,
             key: &str,
             symbol_data_registry: &SymbolDataRegistry<UserDefinedTypeData>| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data_index, _, _, _)) => {
                    let symbol_data =
                        symbol_data_registry.get_symbol_data_ref_at_index(symbol_data_index);
                    Some(symbol_data.1)
                }
                None => None,
            };
        match self.types.insert(
            scope_index,
            name,
            UserDefinedTypeData::default_with_struct(),
            decl_range,
            lookup_func,
            true,
            &mut symbol_data_registry.types_registry,
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
        is_concretization_required: bool,
        decl_range: TextRange,
        symbol_data_registry: &mut GlobalSymbolDataRegistry,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope,
             scope_index: usize,
             key: &str,
             symbol_data_registry: &SymbolDataRegistry<UserDefinedTypeData>| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data_index, _, _, _)) => {
                    let symbol_data =
                        symbol_data_registry.get_symbol_data_ref_at_index(symbol_data_index);
                    Some(symbol_data.1)
                }
                None => None,
            };
        match self.types.insert(
            scope_index,
            name,
            UserDefinedTypeData::Lambda(LambdaTypeData {
                prototype: CallablePrototypeData {
                    params: param_types,
                    return_type,
                    is_concretization_required,
                },
                generics: None,
            }),
            decl_range,
            lookup_func,
            true,
            &mut symbol_data_registry.types_registry,
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
        symbol_data_registry: &mut GlobalSymbolDataRegistry,
    ) -> Result<SymbolDataEntry, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope,
             scope_index: usize,
             key: &str,
             symbol_data_registry: &SymbolDataRegistry<InterfaceData>| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data_index, _, _, _)) => {
                    let symbol_data =
                        symbol_data_registry.get_symbol_data_ref_at_index(symbol_data_index);
                    Some(symbol_data.1)
                }
                None => None,
            };
        match self.interfaces.insert(
            scope_index,
            name,
            InterfaceData::default(),
            decl_range,
            lookup_func,
            true,
            &mut symbol_data_registry.interfaces_registry,
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
