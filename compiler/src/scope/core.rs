use super::concrete::core::{ConcreteTypesRegistryKey, ConcreteTypesTuple};
use super::errors::GenericTypeArgsCheckError;
use super::function::{CallableData, CallableKind};
use super::handler::SymbolDataEntry;
use super::helper::check_concrete_types_bounded_by_interfaces;
use super::interfaces::{InterfaceBounds, InterfaceData};
use super::types::generic_type::{GenericTypeData, GenericTypeDeclarationPlaceCategory};
use super::types::lambda_type::LambdaTypeData;
use crate::scope::types::core::UserDefinedTypeData;
use crate::scope::variables::VariableData;
use crate::types::core::AbstractType;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug)]
pub struct LookupData<T: AbstractSymbolData> {
    pub symbol_data: T,
    pub resolved_scope_index: usize,
    pub depth: usize,
    pub is_global: bool,
}

impl<T: AbstractSymbolData> LookupData<T> {
    fn new(symbol_data: T, resolved_scope_index: usize, depth: usize, is_global: bool) -> Self {
        LookupData {
            symbol_data,
            resolved_scope_index,
            depth,
            is_global,
        }
    }
}

pub enum LookupResult<T: AbstractSymbolData> {
    Ok(LookupData<T>),
    NotInitialized(TextRange),
    Unresolved,
}

#[derive(Debug)]
pub enum IntermediateLookupResult<T: AbstractConcreteTypesHandler> {
    Ok((SymbolData<T>, usize, usize, bool)),
    NotInitialized(TextRange),
    Unresolved,
}

pub trait AbstractConcreteTypesHandler {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey;
    fn is_initialized(&self) -> bool;
}

pub trait AbstractSymbolMetaData {
    fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple;
}

pub trait AbstractSymbolData {
    fn get_entry(&self) -> SymbolDataEntry;
    fn register_concrete_types(
        &self,
        concrete_types: Option<Vec<Type>>,
    ) -> Option<ConcreteTypesRegistryKey>;
    fn check_generic_type_args(
        &self,
        concrete_types: &Option<Vec<Type>>,
        type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError>;
}

pub enum ConcreteTypesRegistrationKind {
    Primary,
    Method,
}

#[derive(Debug)]
pub struct GenericTypeParams(pub Vec<(String, InterfaceBounds, TextRange)>);

impl GenericTypeParams {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn check_concrete_types_bounded_by(
        &self,
        concrete_types: &Vec<Type>,
        type_ranges: &Vec<TextRange>,
    ) -> Result<(), GenericTypeArgsCheckError> {
        let expected_len = self.len();
        let received_len = concrete_types.len();
        if expected_len != received_len {
            return Err(GenericTypeArgsCheckError::GenericTypeArgsCountMismatched(
                received_len,
                expected_len,
            ));
        }
        let mut incorrectly_bounded_types: Vec<(TextRange, String)> = vec![];
        for (index, (_, interface_bounds, _)) in self.0.iter().enumerate() {
            let ty = &concrete_types[index];
            if !ty.is_type_bounded_by_interfaces(interface_bounds) {
                incorrectly_bounded_types.push((type_ranges[index], interface_bounds.to_string()))
            }
        }
        if incorrectly_bounded_types.len() > 0 {
            return Err(
                GenericTypeArgsCheckError::GenericTypeArgsIncorrectlyBounded(
                    incorrectly_bounded_types,
                ),
            );
        }
        return Ok(());
    }
}

#[derive(Debug)]
pub struct SymbolData<T: AbstractConcreteTypesHandler>(pub Rc<RefCell<T>>, pub TextRange, pub bool); // (identifier_meta_data, decl_line_number, should_add_prefix)

impl<T: AbstractConcreteTypesHandler> SymbolData<T> {
    pub fn new(core_data: T, decl_range: TextRange, is_suffix_required: bool) -> Self {
        SymbolData(
            Rc::new(RefCell::new(core_data)),
            decl_range,
            is_suffix_required,
        )
    }

    pub fn get_core_ref<'a>(&'a self) -> Ref<'a, T> {
        self.0.as_ref().borrow()
    }

    pub fn get_core_mut_ref<'a>(&'a self) -> RefMut<'a, T> {
        self.0.as_ref().borrow_mut()
    }

    pub fn register_concrete_types(
        &self,
        concrete_types: Option<Vec<Type>>,
    ) -> Option<ConcreteTypesRegistryKey> {
        match concrete_types {
            Some(concrete_types) => {
                return Some(
                    self.get_core_mut_ref()
                        .register_concrete_types(concrete_types),
                )
            }
            None => return None,
        }
    }
}

impl<T: AbstractConcreteTypesHandler> Clone for SymbolData<T> {
    fn clone(&self) -> Self {
        SymbolData(self.0.clone(), self.1, self.2)
    }
}

#[derive(Debug)]
pub struct VariableSymbolData(pub SymbolData<VariableData>);

impl AbstractSymbolData for VariableSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Variable(self.0.clone())
    }

    fn register_concrete_types(
        &self,
        concrete_types: Option<Vec<Type>>,
    ) -> Option<ConcreteTypesRegistryKey> {
        self.0.register_concrete_types(concrete_types)
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<Vec<Type>>,
        _type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError> {
        assert!(!is_concrete_types_none_allowed);
        if concrete_types.is_some() {
            return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected);
        }
        return Ok(());
    }
}

#[derive(Debug)]
pub struct FunctionSymbolData(pub SymbolData<CallableData>);

impl AbstractSymbolData for FunctionSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Function(self.0.clone())
    }

    fn register_concrete_types(
        &self,
        concrete_types: Option<Vec<Type>>,
    ) -> Option<ConcreteTypesRegistryKey> {
        self.0.register_concrete_types(concrete_types)
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<Vec<Type>>,
        type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError> {
        assert!(is_concrete_types_none_allowed);
        let function_data = self.0.get_core_ref();
        let generic_type_decls = &function_data.generics.generics_spec;
        check_concrete_types_bounded_by_interfaces(
            generic_type_decls,
            concrete_types,
            type_ranges,
            true,
        )
    }
}

#[derive(Debug)]
pub struct UserDefinedTypeSymbolData(pub SymbolData<UserDefinedTypeData>);

impl AbstractSymbolData for UserDefinedTypeSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Type(self.0.clone())
    }

    fn register_concrete_types(
        &self,
        concrete_types: Option<Vec<Type>>,
    ) -> Option<ConcreteTypesRegistryKey> {
        self.0.register_concrete_types(concrete_types)
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<Vec<Type>>,
        type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError> {
        match &*self.0.get_core_ref() {
            UserDefinedTypeData::Struct(struct_data) => {
                let generic_type_decls = &struct_data.generics.generics_spec;
                return check_concrete_types_bounded_by_interfaces(
                    generic_type_decls,
                    concrete_types,
                    type_ranges,
                    is_concrete_types_none_allowed,
                );
            }
            UserDefinedTypeData::Lambda(lambda_data) => {
                let generic_type_decls = &lambda_data.get_generic_type_decls();
                return check_concrete_types_bounded_by_interfaces(
                    generic_type_decls,
                    concrete_types,
                    type_ranges,
                    false,
                );
            }
            UserDefinedTypeData::Generic(_) => {
                if concrete_types.is_some() {
                    return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected);
                }
                return Ok(());
            }
        }
    }
}

#[derive(Debug)]
pub struct InterfaceSymbolData(pub SymbolData<InterfaceData>);

impl AbstractSymbolData for InterfaceSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Interface(self.0.clone())
    }

    fn register_concrete_types(
        &self,
        concrete_types: Option<Vec<Type>>,
    ) -> Option<ConcreteTypesRegistryKey> {
        self.0.register_concrete_types(concrete_types)
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<Vec<Type>>,
        type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError> {
        assert!(!is_concrete_types_none_allowed);
        let interface_data = self.0.get_core_ref();
        let generic_type_decls = &interface_data.generics.generics_spec;
        check_concrete_types_bounded_by_interfaces(
            generic_type_decls,
            concrete_types,
            type_ranges,
            false,
        )
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
        // (symbol_data, resolved_scope_index, depth, is_global)
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

    fn lookup_with_is_init(&self, scope_index: usize, key: &str) -> IntermediateLookupResult<T> {
        match self.lookup(scope_index, key) {
            Some((symbol_data, resolved_scope_index, depth, is_global)) => {
                if symbol_data.get_core_ref().is_initialized() {
                    return IntermediateLookupResult::Ok((
                        symbol_data,
                        resolved_scope_index,
                        depth,
                        is_global,
                    ));
                } else {
                    return IntermediateLookupResult::NotInitialized(symbol_data.1);
                }
            }
            None => return IntermediateLookupResult::Unresolved,
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
    ) -> LookupResult<VariableSymbolData> {
        match self.variables.lookup_with_is_init(scope_index, key) {
            IntermediateLookupResult::Ok(data) => LookupResult::Ok(LookupData::new(
                VariableSymbolData(data.0),
                data.1,
                data.2,
                data.3,
            )),
            IntermediateLookupResult::NotInitialized(decl_range) => {
                LookupResult::NotInitialized(decl_range)
            }
            IntermediateLookupResult::Unresolved => LookupResult::Unresolved,
        }
    }

    pub fn lookup_in_functions_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> LookupResult<FunctionSymbolData> {
        match self.functions.lookup_with_is_init(scope_index, key) {
            IntermediateLookupResult::Ok(data) => LookupResult::Ok(LookupData::new(
                FunctionSymbolData(data.0),
                data.1,
                data.2,
                data.3,
            )),
            IntermediateLookupResult::NotInitialized(decl_range) => {
                LookupResult::NotInitialized(decl_range)
            }
            IntermediateLookupResult::Unresolved => LookupResult::Unresolved,
        }
    }

    pub fn lookup_in_types_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> LookupResult<UserDefinedTypeSymbolData> {
        match self.types.lookup_with_is_init(scope_index, key) {
            IntermediateLookupResult::Ok(data) => LookupResult::Ok(LookupData::new(
                UserDefinedTypeSymbolData(data.0),
                data.1,
                data.2,
                data.3,
            )),
            IntermediateLookupResult::NotInitialized(decl_range) => {
                LookupResult::NotInitialized(decl_range)
            }
            IntermediateLookupResult::Unresolved => LookupResult::Unresolved,
        }
    }

    pub fn lookup_in_interfaces_namespace(
        &self,
        scope_index: usize,
        key: &str,
    ) -> LookupResult<InterfaceSymbolData> {
        match self.interfaces.lookup_with_is_init(scope_index, key) {
            IntermediateLookupResult::Ok(data) => LookupResult::Ok(LookupData::new(
                InterfaceSymbolData(data.0),
                data.1,
                data.2,
                data.3,
            )),
            IntermediateLookupResult::NotInitialized(decl_range) => {
                LookupResult::NotInitialized(decl_range)
            }
            IntermediateLookupResult::Unresolved => LookupResult::Unresolved,
        }
    }

    pub fn declare_variable(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<VariableSymbolData, (String, TextRange)> {
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
            Ok(symbol_data) => Ok(VariableSymbolData(symbol_data)),
            Err(err) => Err(err),
        }
    }

    pub fn declare_variable_with_type(
        &mut self,
        scope_index: usize,
        name: String,
        variable_type: &Type,
        decl_range: TextRange,
        is_init: bool,
    ) -> Result<VariableSymbolData, (String, TextRange)> {
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
            Ok(symbol_data) => Ok(VariableSymbolData(symbol_data)),
            Err(err) => Err(err),
        }
    }

    pub fn declare_function(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<FunctionSymbolData, (String, TextRange)> {
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
            Ok(symbol_data) => Ok(FunctionSymbolData(symbol_data)),
            Err(err) => Err(err),
        }
    }

    pub fn declare_user_defined_type(
        &mut self,
        scope_index: usize,
        name: String,
        meta_data: UserDefinedTypeData,
        decl_range: TextRange,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let lookup_func =
            |scope: &Scope<UserDefinedTypeData>, scope_index: usize, key: &str| match scope
                .lookup(scope_index, key)
            {
                Some((symbol_data, _, _, _)) => Some(symbol_data.1),
                None => None,
            };
        match self
            .types
            .insert(scope_index, name, meta_data, decl_range, lookup_func, true)
        {
            Ok(symbol_data) => Ok(UserDefinedTypeSymbolData(symbol_data)),
            Err(err) => Err(err),
        }
    }

    pub fn declare_struct_type(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let meta_data = UserDefinedTypeData::default_with_struct();
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range)
    }

    pub fn declare_lambda_type_with_meta_data(
        &mut self,
        scope_index: usize,
        name: String,
        param_types: Vec<Type>,
        return_type: Type,
        is_concretization_required: Option<(Vec<usize>, bool)>,
        generics_spec: Option<GenericTypeParams>,
        decl_range: TextRange,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let meta_data = UserDefinedTypeData::Lambda(LambdaTypeData::new(
            param_types,
            return_type,
            is_concretization_required,
            generics_spec,
        ));
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range)
    }

    pub fn declare_generic_type_with_meta_data(
        &mut self,
        scope_index: usize,
        name: String,
        index: usize,
        category: GenericTypeDeclarationPlaceCategory,
        interface_bounds: &InterfaceBounds,
        decl_range: TextRange,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let meta_data = UserDefinedTypeData::Generic(GenericTypeData::new(
            index,
            category,
            interface_bounds.clone(),
        ));
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range)
    }

    pub fn declare_interface(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
    ) -> Result<InterfaceSymbolData, (String, TextRange)> {
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
            Ok(symbol_data) => Ok(InterfaceSymbolData(symbol_data)),
            Err(err) => Err(err),
        }
    }
}

impl Default for Namespace {
    fn default() -> Self {
        Namespace::new()
    }
}
