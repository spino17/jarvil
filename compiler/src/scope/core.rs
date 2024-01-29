use super::concrete::ConcreteTypesTuple;
use super::errors::GenericTypeArgsCheckError;
use super::function::{CallableData, CallableKind};
use super::handler::SymbolDataEntry;
use super::helper::check_concrete_types_bounded_by_interfaces;
use super::interfaces::{InterfaceBounds, InterfaceData};
use super::types::generic_type::{GenericTypeData, GenericTypeDeclarationPlaceCategory};
use super::types::lambda_type::LambdaTypeData;
use crate::core::string_interner::{Interner, StrId};
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
    pub enclosing_func_scope_depth: Option<usize>,
}

impl<T: AbstractSymbolData> LookupData<T> {
    fn new(
        symbol_data: T,
        resolved_scope_index: usize,
        depth: usize,
        enclosing_func_scope_depth: Option<usize>,
    ) -> Self {
        LookupData {
            symbol_data,
            resolved_scope_index,
            depth,
            enclosing_func_scope_depth,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct MangledIdentifierName {
    pub jarvil_identifer_name: StrId,
    unique_id: Option<usize>,
}

impl MangledIdentifierName {
    pub fn to_string(&self, suffix: &str, interner: &Interner) -> String {
        match self.unique_id {
            Some(id) => format!(
                "{}_{}_{}",
                interner.lookup(self.jarvil_identifer_name),
                id,
                suffix
            ),
            None => interner.lookup(self.jarvil_identifer_name).to_string(),
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
    Ok((SymbolData<T>, usize, usize, Option<usize>)),
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
        interner: &Interner,
    ) -> Result<(), GenericTypeArgsCheckError>;
    fn get_mangled_name(&self) -> MangledIdentifierName;
}

pub enum ConcreteTypesRegistrationKind {
    Primary,
    Method,
}

#[derive(Debug)]
pub struct GenericTypeParams(pub Vec<(StrId, InterfaceBounds, TextRange)>);

impl GenericTypeParams {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn check_concrete_types_bounded_by(
        &self,
        concrete_types: &ConcreteTypesTuple,
        type_ranges: &Vec<TextRange>,
        interner: &Interner,
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
                incorrectly_bounded_types
                    .push((type_ranges[index], interface_bounds.to_string(interner)))
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
    pub identifier_name: StrId,
    pub identifier_data: RefCell<T>,
    pub declaration_line_number: TextRange,
    pub unique_id: Option<usize>,
}

impl<T: AbstractConcreteTypesHandler> SymbolDataCore<T> {
    fn new(
        identifier_name: StrId,
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
    pub fn new(name: StrId, core_data: T, decl_range: TextRange, unique_id: Option<usize>) -> Self {
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

    pub fn identifier_name(&self) -> StrId {
        self.0.identifier_name
    }

    pub fn declaration_line_number(&self) -> TextRange {
        self.0.declaration_line_number
    }

    pub fn is_suffix_required(&self) -> bool {
        self.0.unique_id.is_some()
    }

    pub fn get_mangled_name(&self) -> MangledIdentifierName {
        MangledIdentifierName {
            jarvil_identifer_name: self.identifier_name(),
            unique_id: self.0.unique_id,
        }
    }

    pub fn get_index(&self) -> Option<usize> {
        self.0.unique_id
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
        _interner: &Interner,
    ) -> Result<(), GenericTypeArgsCheckError> {
        debug_assert!(!is_concrete_types_none_allowed);
        if concrete_types.is_some() {
            return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected);
        }
        Ok(())
    }

    fn get_mangled_name(&self) -> MangledIdentifierName {
        self.0.get_mangled_name()
    }
}

impl From<SymbolData<VariableData>> for VariableSymbolData {
    fn from(value: SymbolData<VariableData>) -> Self {
        VariableSymbolData(value)
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
        interner: &Interner,
    ) -> Result<(), GenericTypeArgsCheckError> {
        debug_assert!(is_concrete_types_none_allowed);
        let function_data = self.0.get_core_ref();
        let generic_type_decls = &function_data.generics;
        check_concrete_types_bounded_by_interfaces(
            generic_type_decls,
            concrete_types,
            type_ranges,
            true,
            interner,
        )
    }

    fn get_mangled_name(&self) -> MangledIdentifierName {
        self.0.get_mangled_name()
    }
}

impl From<SymbolData<CallableData>> for FunctionSymbolData {
    fn from(value: SymbolData<CallableData>) -> Self {
        FunctionSymbolData(value)
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
        interner: &Interner,
    ) -> Result<(), GenericTypeArgsCheckError> {
        match &*self.0.get_core_ref() {
            UserDefinedTypeData::Struct(struct_data) => {
                let generic_type_decls = &struct_data.generics;
                check_concrete_types_bounded_by_interfaces(
                    generic_type_decls,
                    concrete_types,
                    type_ranges,
                    is_concrete_types_none_allowed,
                    interner,
                )
            }
            UserDefinedTypeData::Lambda(lambda_data) => {
                let generic_type_decls = &lambda_data.get_generic_type_decls();
                check_concrete_types_bounded_by_interfaces(
                    generic_type_decls,
                    concrete_types,
                    type_ranges,
                    false,
                    interner,
                )
            }
            UserDefinedTypeData::Enum(enum_data) => {
                let generic_type_decls = &enum_data.generics;
                check_concrete_types_bounded_by_interfaces(
                    generic_type_decls,
                    concrete_types,
                    type_ranges,
                    false,
                    interner,
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

impl From<SymbolData<UserDefinedTypeData>> for UserDefinedTypeSymbolData {
    fn from(value: SymbolData<UserDefinedTypeData>) -> Self {
        UserDefinedTypeSymbolData(value)
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
        interner: &Interner,
    ) -> Result<(), GenericTypeArgsCheckError> {
        debug_assert!(!is_concrete_types_none_allowed);
        let interface_data = self.0.get_core_ref();
        let generic_type_decls = &interface_data.generics;
        check_concrete_types_bounded_by_interfaces(
            generic_type_decls,
            concrete_types,
            type_ranges,
            false,
            interner,
        )
    }

    fn get_mangled_name(&self) -> MangledIdentifierName {
        unreachable!()
    }
}

impl From<SymbolData<InterfaceData>> for InterfaceSymbolData {
    fn from(value: SymbolData<InterfaceData>) -> Self {
        InterfaceSymbolData(value)
    }
}

#[derive(Debug)]
pub struct CoreScope<T: AbstractConcreteTypesHandler> {
    symbol_table: FxHashMap<StrId, SymbolData<T>>,
    pub parent_scope: Option<usize>, // points to the index in the global flattened scope vec
    scope_kind: BlockKind,
}

impl<T: AbstractConcreteTypesHandler> CoreScope<T> {
    fn set(
        &mut self,
        name: StrId,
        meta_data: T,
        decl_range: TextRange,
        unique_id: Option<usize>,
    ) -> SymbolData<T> {
        let symbol_data = SymbolData::new(name, meta_data, decl_range, unique_id);
        self.symbol_table.insert(name, symbol_data.clone());
        symbol_data
    }

    pub fn get(&self, name: &StrId) -> Option<&SymbolData<T>> {
        self.symbol_table.get(name)
    }

    pub fn lookup(
        &self,
        scope_index: usize,
        key: &StrId,
        global_scope_vec: &Vec<CoreScope<T>>,
    ) -> Option<(SymbolData<T>, usize, usize, Option<usize>)> {
        let mut enclosing_func_scope_depth: Option<usize> = None;
        let mut previous_scope_kind = self.scope_kind;
        match self.get(key) {
            Some(value) => Some((value.clone(), scope_index, 0, None)),
            None => {
                let mut parent_scope_index = self.parent_scope;
                let mut depth = 1;
                while let Some(scope_index) = parent_scope_index {
                    if enclosing_func_scope_depth.is_none()
                        && previous_scope_kind.has_callable_body()
                    {
                        enclosing_func_scope_depth = Some(depth);
                    }
                    let scope = &global_scope_vec[scope_index];
                    match scope.get(key) {
                        Some(value) => {
                            return Some((
                                value.clone(),
                                scope_index,
                                depth,
                                enclosing_func_scope_depth,
                            ))
                        }
                        None => {
                            parent_scope_index = scope.parent_scope;
                            depth += 1;
                            previous_scope_kind = scope.scope_kind;
                        }
                    }
                }
                None
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
        key: StrId,
        meta_data: T,
        decl_range: TextRange,
    ) {
        // use this method only for builtin function where we know that no entry already exist in the scope
        self.flattened_vec[scope_index].set(key, meta_data, decl_range, None);
    }

    pub fn insert<U: Fn(&Scope<T>, usize, &StrId) -> Option<TextRange>>(
        &mut self,
        scope_index: usize,
        key: StrId,
        meta_data: T,
        decl_range: TextRange,
        lookup_fn: U,
        unique_id: usize,
    ) -> Result<SymbolData<T>, (StrId, TextRange)> {
        if let Some(previous_decl_range) = lookup_fn(self, scope_index, &key) {
            return Err((key, previous_decl_range));
        }
        let symbol_data =
            self.flattened_vec[scope_index].set(key, meta_data, decl_range, Some(unique_id));
        Ok(symbol_data)
    }

    pub fn get(&self, scope_index: usize, key: &StrId) -> Option<&SymbolData<T>> {
        self.flattened_vec[scope_index].get(key)
    }

    fn lookup(
        &self,
        scope_index: usize,
        key: &StrId,
    ) -> Option<(SymbolData<T>, usize, usize, Option<usize>)> {
        self.flattened_vec[scope_index].lookup(scope_index, key, &self.flattened_vec)
    }

    fn lookup_with_is_init(&self, scope_index: usize, key: &StrId) -> IntermediateLookupResult<T> {
        let Some((symbol_data, resolved_scope_index, depth, enclosing_func_scope_depth)) =
            self.lookup(scope_index, key)
        else {
            return IntermediateLookupResult::Unresolved;
        };
        if symbol_data.get_core_ref().is_initialized() {
            IntermediateLookupResult::Ok((
                symbol_data,
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
            ))
        } else {
            IntermediateLookupResult::NotInitialized(symbol_data.declaration_line_number())
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
        key: &StrId,
    ) -> Option<&SymbolData<VariableData>> {
        self.variables.get(scope_index, key)
    }

    pub fn get_from_functions_namespace(
        &self,
        scope_index: usize,
        key: &StrId,
    ) -> Option<&SymbolData<CallableData>> {
        self.functions.get(scope_index, key)
    }

    pub fn get_from_types_namespace(
        &self,
        scope_index: usize,
        key: &StrId,
    ) -> Option<&SymbolData<UserDefinedTypeData>> {
        self.types.get(scope_index, key)
    }

    pub fn get_from_interfaces_namespace(
        &self,
        scope_index: usize,
        key: &StrId,
    ) -> Option<&SymbolData<InterfaceData>> {
        self.interfaces.get(scope_index, key)
    }

    pub fn lookup_in_variables_namespace(
        &self,
        scope_index: usize,
        key: &StrId,
    ) -> LookupResult<VariableSymbolData> {
        match self.variables.lookup_with_is_init(scope_index, key) {
            IntermediateLookupResult::Ok((
                symbol_data,
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
            )) => LookupResult::Ok(LookupData::new(
                VariableSymbolData(symbol_data),
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
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
        key: &StrId,
    ) -> LookupResult<FunctionSymbolData> {
        match self.functions.lookup_with_is_init(scope_index, key) {
            IntermediateLookupResult::Ok((
                symbol_data,
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
            )) => LookupResult::Ok(LookupData::new(
                FunctionSymbolData(symbol_data),
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
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
        key: &StrId,
    ) -> LookupResult<UserDefinedTypeSymbolData> {
        match self.types.lookup_with_is_init(scope_index, key) {
            IntermediateLookupResult::Ok((
                symbol_data,
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
            )) => LookupResult::Ok(LookupData::new(
                UserDefinedTypeSymbolData(symbol_data),
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
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
        key: &StrId,
    ) -> LookupResult<InterfaceSymbolData> {
        match self.interfaces.lookup_with_is_init(scope_index, key) {
            IntermediateLookupResult::Ok((
                symbol_data,
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
            )) => LookupResult::Ok(LookupData::new(
                InterfaceSymbolData(symbol_data),
                resolved_scope_index,
                depth,
                enclosing_func_scope_depth,
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
        name: StrId,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<VariableSymbolData, (StrId, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &StrId| {
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
        name: StrId,
        variable_type: &Type,
        decl_range: TextRange,
        is_init: bool,
        unique_id: usize,
    ) -> Result<VariableSymbolData, (StrId, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &StrId| {
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
        name: StrId,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<FunctionSymbolData, (StrId, TextRange)> {
        let lookup_func = |scope: &Scope<CallableData>, scope_index: usize, key: &StrId| {
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
        name: StrId,
        meta_data: UserDefinedTypeData,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (StrId, TextRange)> {
        let lookup_func = |scope: &Scope<UserDefinedTypeData>, scope_index: usize, key: &StrId| {
            scope
                .lookup(scope_index, key)
                .map(|(symbol_data, _, _, _)| symbol_data.declaration_line_number())
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
        name: StrId,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (StrId, TextRange)> {
        let meta_data = UserDefinedTypeData::default_with_struct();
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_enum_type(
        &mut self,
        scope_index: usize,
        name: StrId,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (StrId, TextRange)> {
        let meta_data = UserDefinedTypeData::default_with_enum();
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_lambda_type_with_meta_data(
        &mut self,
        scope_index: usize,
        name: StrId,
        param_types: Vec<Type>,
        return_type: Type,
        is_concretization_required: Option<(Vec<usize>, bool)>,
        generics_spec: Option<GenericTypeParams>,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (StrId, TextRange)> {
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
        name: StrId,
        index: usize,
        category: GenericTypeDeclarationPlaceCategory,
        interface_bounds: &InterfaceBounds,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<UserDefinedTypeSymbolData, (StrId, TextRange)> {
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
        name: StrId,
        decl_range: TextRange,
        unique_id: usize,
    ) -> Result<InterfaceSymbolData, (StrId, TextRange)> {
        let lookup_func = |scope: &Scope<InterfaceData>, scope_index: usize, key: &StrId| {
            scope
                .lookup(scope_index, key)
                .map(|(symbol_data, _, _, _)| symbol_data.declaration_line_number())
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
