use super::concrete::ConcreteTypesTuple;
use super::errors::GenericTypeArgsCheckError;
use super::function::{CallableData, CallableKind};
use super::handler::SymbolDataEntry;
use super::helper::check_concrete_types_bounded_by_interfaces;
use super::interfaces::{InterfaceBounds, InterfaceData};
use super::types::generic_type::{GenericTypeData, GenericTypeDeclarationPlaceCategory};
use super::types::lambda_type::LambdaTypeData;
use crate::parser::resolver::BlockKind;
use crate::scope::types::core::UserDefinedTypeData;
use crate::scope::variables::VariableData;
use crate::types::core::AbstractType;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::{Ref, RefCell, RefMut};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug)]
pub struct LookupData<T: AbstractSymbolData> {
    pub symbol_data: T,
    pub resolved_scope_index: usize,
    pub depth: usize,
}

impl<T: AbstractSymbolData> LookupData<T> {
    fn new(symbol_data: T, resolved_scope_index: usize, depth: usize) -> Self {
        LookupData {
            symbol_data,
            resolved_scope_index,
            depth,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MangledIdentifierName {
    pub jarvil_identifer_name: String,
    unique_id: Option<usize>,
}

impl MangledIdentifierName {
    pub fn to_string(&self, suffix: &str) -> String {
        match self.unique_id {
            Some(id) => format!(
                "{}_{}_{}",
                self.jarvil_identifer_name,
                id.to_string(),
                suffix
            ),
            None => self.jarvil_identifer_name.to_string(),
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
    Ok((SymbolData<T>, usize, usize)),
    NotInitialized(TextRange),
    Unresolved,
}

pub trait AbstractConcreteTypesHandler {
    fn is_initialized(&self) -> bool;
}

pub trait AbstractSymbolData {
    fn get_entry(&self) -> SymbolDataEntry;
    fn check_generic_type_args(
        &self,
        concrete_types: &Option<ConcreteTypesTuple>,
        type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError>;
    fn get_mangled_name(&self) -> MangledIdentifierName;
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
        concrete_types: &ConcreteTypesTuple,
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
        if !incorrectly_bounded_types.is_empty() {
            return Err(
                GenericTypeArgsCheckError::GenericTypeArgsIncorrectlyBounded(
                    incorrectly_bounded_types,
                ),
            );
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct SymbolDataCore<T: AbstractConcreteTypesHandler> {
    pub identifier_name: String,
    pub identifier_data: RefCell<T>,
    pub declaration_line_number: TextRange,
    unique_id: Option<usize>,
}

impl<T: AbstractConcreteTypesHandler> SymbolDataCore<T> {
    fn new(
        identifier_name: String,
        identifier_data: T,
        decl_range: TextRange,
        unique_id: Option<usize>,
    ) -> Self {
        SymbolDataCore {
            identifier_name,
            identifier_data: RefCell::new(identifier_data),
            declaration_line_number: decl_range,
            unique_id,
        }
    }
}

#[derive(Debug)]
pub struct SymbolData<T: AbstractConcreteTypesHandler>(Rc<SymbolDataCore<T>>); // (identifier_meta_data, decl_line_number, should_add_prefix)

impl<T: AbstractConcreteTypesHandler> SymbolData<T> {
    pub fn new(
        name: String,
        core_data: T,
        decl_range: TextRange,
        unique_id: Option<usize>,
    ) -> Self {
        SymbolData(Rc::new(SymbolDataCore::new(
            name, core_data, decl_range, unique_id,
        )))
    }

    pub fn get_core_ref(&self) -> Ref<'_, T> {
        self.0.as_ref().identifier_data.borrow()
    }

    pub fn get_core_mut_ref(&self) -> RefMut<'_, T> {
        self.0.as_ref().identifier_data.borrow_mut()
    }

    pub fn identifier_name(&self) -> &str {
        &self.0.identifier_name
    }

    pub fn declaration_line_number(&self) -> TextRange {
        self.0.declaration_line_number
    }

    pub fn is_suffix_required(&self) -> bool {
        self.0.unique_id.is_some()
    }

    pub fn get_mangled_name(&self) -> MangledIdentifierName {
        MangledIdentifierName {
            jarvil_identifer_name: self.identifier_name().to_string(),
            unique_id: self.0.unique_id,
        }
    }
}

impl<T: AbstractConcreteTypesHandler> Clone for SymbolData<T> {
    fn clone(&self) -> Self {
        SymbolData(self.0.clone())
    }
}

impl<T: AbstractConcreteTypesHandler> PartialEq for SymbolData<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: AbstractConcreteTypesHandler> Eq for SymbolData<T> {}

impl<T: AbstractConcreteTypesHandler> Hash for SymbolData<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

#[derive(Debug)]
pub struct VariableSymbolData(pub SymbolData<VariableData>);

impl AbstractSymbolData for VariableSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Variable(self.0.clone())
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<ConcreteTypesTuple>,
        _type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError> {
        assert!(!is_concrete_types_none_allowed);
        if concrete_types.is_some() {
            return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected);
        }
        Ok(())
    }

    fn get_mangled_name(&self) -> MangledIdentifierName {
        self.0.get_mangled_name()
    }
}

#[derive(Debug)]
pub struct FunctionSymbolData(pub SymbolData<CallableData>);

impl AbstractSymbolData for FunctionSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Function(self.0.clone())
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<ConcreteTypesTuple>,
        type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError> {
        assert!(is_concrete_types_none_allowed);
        let function_data = self.0.get_core_ref();
        let generic_type_decls = &function_data.generics;
        check_concrete_types_bounded_by_interfaces(
            generic_type_decls,
            concrete_types,
            type_ranges,
            true,
        )
    }

    fn get_mangled_name(&self) -> MangledIdentifierName {
        self.0.get_mangled_name()
    }
}

#[derive(Debug)]
pub struct UserDefinedTypeSymbolData(pub SymbolData<UserDefinedTypeData>);

impl AbstractSymbolData for UserDefinedTypeSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Type(self.0.clone())
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<ConcreteTypesTuple>,
        type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError> {
        match &*self.0.get_core_ref() {
            UserDefinedTypeData::Struct(struct_data) => {
                let generic_type_decls = &struct_data.generics;
                check_concrete_types_bounded_by_interfaces(
                    generic_type_decls,
                    concrete_types,
                    type_ranges,
                    is_concrete_types_none_allowed,
                )
            }
            UserDefinedTypeData::Lambda(lambda_data) => {
                let generic_type_decls = &lambda_data.get_generic_type_decls();
                check_concrete_types_bounded_by_interfaces(
                    generic_type_decls,
                    concrete_types,
                    type_ranges,
                    false,
                )
            }
            UserDefinedTypeData::Generic(_) => {
                if concrete_types.is_some() {
                    return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected);
                }
                Ok(())
            }
        }
    }

    fn get_mangled_name(&self) -> MangledIdentifierName {
        self.0.get_mangled_name()
    }
}

#[derive(Debug)]
pub struct InterfaceSymbolData(pub SymbolData<InterfaceData>);

impl AbstractSymbolData for InterfaceSymbolData {
    fn get_entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Interface(self.0.clone())
    }

    fn check_generic_type_args(
        &self,
        concrete_types: &Option<ConcreteTypesTuple>,
        type_ranges: &Option<Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(), GenericTypeArgsCheckError> {
        assert!(!is_concrete_types_none_allowed);
        let interface_data = self.0.get_core_ref();
        let generic_type_decls = &interface_data.generics;
        check_concrete_types_bounded_by_interfaces(
            generic_type_decls,
            concrete_types,
            type_ranges,
            false,
        )
    }

    fn get_mangled_name(&self) -> MangledIdentifierName {
        unreachable!()
    }
}

#[derive(Debug)]
pub struct CoreScope<T: AbstractConcreteTypesHandler> {
    symbol_table: FxHashMap<String, SymbolData<T>>,
    pub parent_scope: Option<usize>, // points to the index in the global flattened scope vec
    scope_kind: BlockKind,
}

impl<T: AbstractConcreteTypesHandler> CoreScope<T> {
    fn set(
        &mut self,
        name: String,
        meta_data: T,
        decl_range: TextRange,
        unique_id: Option<usize>,
    ) -> SymbolData<T> {
        let symbol_data = SymbolData::new(name.to_string(), meta_data, decl_range, unique_id);
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
    ) -> Option<(SymbolData<T>, usize, usize)> {
        // (symbol_data, resolved_scope_index, depth, is_global)
        match self.get(key) {
            Some(value) => Some((value.clone(), scope_index, 0)),
            None => {
                if let Some(parent_scope_index) = self.parent_scope {
                    global_scope_vec[parent_scope_index]
                        .lookup(parent_scope_index, key, global_scope_vec)
                        .map(|(symbol_data, resolved_scope_index, depth)| {
                            (symbol_data, resolved_scope_index, depth + 1)
                        })
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
                scope_kind: BlockKind::Function,
            }],
        }
    }

    fn add_new_scope(&mut self, parent_scope_index: usize, scope_kind: BlockKind) -> usize {
        let new_scope_index = self.flattened_vec.len();
        self.flattened_vec.push(CoreScope {
            symbol_table: FxHashMap::default(),
            parent_scope: Some(parent_scope_index),
            scope_kind,
        });
        new_scope_index
    }

    pub fn force_insert(
        &mut self,
        scope_index: usize,
        key: String,
        meta_data: T,
        decl_range: TextRange,
    ) {
        // use this method only for builtin function where we know that no entry already exist in the scope
        self.flattened_vec[scope_index].set(key, meta_data, decl_range, None);
    }

    pub fn insert<U: Fn(&Scope<T>, usize, &str) -> Option<TextRange>>(
        &mut self,
        scope_index: usize,
        key: String,
        meta_data: T,
        decl_range: TextRange,
        lookup_fn: U,
        unique_id: usize,
    ) -> Result<SymbolData<T>, (String, TextRange)> {
        if let Some(previous_decl_range) = lookup_fn(self, scope_index, &key) {
            return Err((key, previous_decl_range));
        }
        let symbol_data =
            self.flattened_vec[scope_index].set(key, meta_data, decl_range, Some(unique_id));
        Ok(symbol_data)
    }

    pub fn get(&self, scope_index: usize, key: &str) -> Option<&SymbolData<T>> {
        self.flattened_vec[scope_index].get(key)
    }

    fn lookup(&self, scope_index: usize, key: &str) -> Option<(SymbolData<T>, usize, usize)> {
        self.flattened_vec[scope_index].lookup(scope_index, key, &self.flattened_vec)
    }

    fn lookup_with_is_init(&self, scope_index: usize, key: &str) -> IntermediateLookupResult<T> {
        match self.lookup(scope_index, key) {
            Some((symbol_data, resolved_scope_index, depth)) => {
                if symbol_data.get_core_ref().is_initialized() {
                    IntermediateLookupResult::Ok((symbol_data, resolved_scope_index, depth))
                } else {
                    IntermediateLookupResult::NotInitialized(symbol_data.declaration_line_number())
                }
            }
            None => IntermediateLookupResult::Unresolved,
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

    pub fn open_scope(&mut self, curr_scope_index: usize, scope_kind: BlockKind) -> usize {
        self.variables.add_new_scope(curr_scope_index, scope_kind);
        self.types.add_new_scope(curr_scope_index, scope_kind);
        self.functions.add_new_scope(curr_scope_index, scope_kind);
        self.interfaces.add_new_scope(curr_scope_index, scope_kind)
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
            IntermediateLookupResult::Ok(data) => {
                LookupResult::Ok(LookupData::new(VariableSymbolData(data.0), data.1, data.2))
            }
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
            IntermediateLookupResult::Ok(data) => {
                LookupResult::Ok(LookupData::new(FunctionSymbolData(data.0), data.1, data.2))
            }
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
            IntermediateLookupResult::Ok(data) => {
                LookupResult::Ok(LookupData::new(InterfaceSymbolData(data.0), data.1, data.2))
            }
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
        unique_id: usize,
    ) -> Result<VariableSymbolData, (String, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &str| {
            scope.flattened_vec[scope_index]
                .get(key)
                .map(|symbol_data| symbol_data.declaration_line_number())
        };
        match self.variables.insert(
            scope_index,
            name,
            VariableData::default(),
            decl_range,
            lookup_func,
            unique_id,
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
        unique_id: usize,
    ) -> Result<VariableSymbolData, (String, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &str| {
            scope.flattened_vec[scope_index]
                .get(key)
                .map(|symbol_data| symbol_data.declaration_line_number())
        };
        match self.variables.insert(
            scope_index,
            name,
            VariableData::new(variable_type, is_init),
            decl_range,
            lookup_func,
            unique_id,
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
        unique_id: usize,
    ) -> Result<FunctionSymbolData, (String, TextRange)> {
        let lookup_func = |scope: &Scope<CallableData>, scope_index: usize, key: &str| {
            scope.flattened_vec[scope_index]
                .get(key)
                .map(|symbol_data| symbol_data.declaration_line_number())
        };
        match self.functions.insert(
            scope_index,
            name,
            CallableData::default_for_kind(CallableKind::Function),
            decl_range,
            lookup_func,
            unique_id,
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
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let lookup_func = |scope: &Scope<UserDefinedTypeData>, scope_index: usize, key: &str| {
            scope
                .lookup(scope_index, key)
                .map(|(symbol_data, _, _)| symbol_data.declaration_line_number())
        };
        match self.types.insert(
            scope_index,
            name,
            meta_data,
            decl_range,
            lookup_func,
            unique_id,
        ) {
            Ok(symbol_data) => Ok(UserDefinedTypeSymbolData(symbol_data)),
            Err(err) => Err(err),
        }
    }

    pub fn declare_struct_type(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let meta_data = UserDefinedTypeData::default_with_struct();
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range, unique_id)
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
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let meta_data = UserDefinedTypeData::Lambda(LambdaTypeData::new(
            param_types,
            return_type,
            is_concretization_required,
            generics_spec,
        ));
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_generic_type_with_meta_data(
        &mut self,
        scope_index: usize,
        name: String,
        index: usize,
        category: GenericTypeDeclarationPlaceCategory,
        interface_bounds: &InterfaceBounds,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let meta_data = UserDefinedTypeData::Generic(GenericTypeData::new(
            index,
            category,
            interface_bounds.clone(),
        ));
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_interface(
        &mut self,
        scope_index: usize,
        name: String,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<InterfaceSymbolData, (String, TextRange)> {
        let lookup_func = |scope: &Scope<InterfaceData>, scope_index: usize, key: &str| {
            scope
                .lookup(scope_index, key)
                .map(|(symbol_data, _, _)| symbol_data.declaration_line_number())
        };
        match self.interfaces.insert(
            scope_index,
            name,
            InterfaceData::default(),
            decl_range,
            lookup_func,
            unique_id,
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
