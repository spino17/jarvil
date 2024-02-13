/*use super::symbol::core::IdentDeclId;
use super::symbol::function::{CallableData, CallableKind};
use super::symbol::interfaces::{InterfaceBounds, InterfaceData};
use super::symbol::types::generic_type::{GenericTypeData, GenericTypeDeclarationPlaceCategory};
use super::symbol::types::lambda_type::LambdaTypeData;
use crate::core::string_interner::StrId;
use crate::parser::resolver::BlockKind;
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::symbol::function::FunctionSymbolData;
use crate::scope::symbol::interfaces::InterfaceSymbolData;
use crate::scope::symbol::types::core::UserDefinedTypeData;
use crate::scope::symbol::types::core::UserDefinedTypeSymbolData;
use crate::scope::symbol::types::generic_type::GenericTypeParams;
use crate::scope::symbol::variables::VariableData;
use crate::scope::symbol::variables::VariableSymbolData;
use crate::scope::traits::AbstractSymbol;
use crate::scope::traits::IsInitialized;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::{Ref, RefCell, RefMut};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug)]
pub struct LookupData<T: AbstractSymbol> {
    pub symbol_data: T,
    pub resolved_scope_index: usize,
    pub depth: usize,
    pub enclosing_func_scope_depth: Option<usize>,
}

impl<T: AbstractSymbol> LookupData<T> {
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

pub enum LookupResult<T: AbstractSymbol> {
    Ok(LookupData<T>),
    NotInitialized(TextRange),
    Unresolved,
}

#[derive(Debug)]
pub enum IntermediateLookupResult<T: IsInitialized> {
    Ok((SymbolData<T>, usize, usize, Option<usize>)),
    NotInitialized(TextRange),
    Unresolved,
}

macro_rules! impl_from_intermediate_lookup_result {
    ($x: ident, $y: ident) => {
        impl From<IntermediateLookupResult<$x>> for LookupResult<$y> {
            fn from(value: IntermediateLookupResult<$x>) -> Self {
                match value {
                    IntermediateLookupResult::Ok((
                        symbol_data,
                        resolved_scope_index,
                        depth,
                        enclosing_func_scope_depth,
                    )) => LookupResult::Ok(LookupData::new(
                        symbol_data.into(),
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
        }
    };
}

impl_from_intermediate_lookup_result!(VariableData, VariableSymbolData);
impl_from_intermediate_lookup_result!(CallableData, FunctionSymbolData);
impl_from_intermediate_lookup_result!(UserDefinedTypeData, UserDefinedTypeSymbolData);
impl_from_intermediate_lookup_result!(InterfaceData, InterfaceSymbolData);

#[derive(Debug)]
pub struct SymbolDataCore<T: IsInitialized> {
    pub identifier_name: StrId,
    pub identifier_data: RefCell<T>,
    pub declaration_line_number: TextRange,
    pub unique_id: Option<usize>,
}

impl<T: IsInitialized> SymbolDataCore<T> {
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
pub struct SymbolData<T: IsInitialized>(Rc<SymbolDataCore<T>>); // (identifier_meta_data, decl_line_number, should_add_prefix)

impl<T: IsInitialized> SymbolData<T> {
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

    pub fn get_mangled_name(&self) -> MangledIdentifierName<T> {
        MangledIdentifierName {
            jarvil_identifer_name: self.identifier_name(),
            unique_id: todo!(),
        }
    }

    pub fn get_index(&self) -> Option<usize> {
        self.0.unique_id
    }
}

impl<T: IsInitialized> Clone for SymbolData<T> {
    fn clone(&self) -> Self {
        SymbolData(self.0.clone())
    }
}

impl<T: IsInitialized> PartialEq for SymbolData<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: IsInitialized> Eq for SymbolData<T> {}

impl<T: IsInitialized> Hash for SymbolData<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

#[derive(Debug)]
pub struct CoreScope<T: IsInitialized> {
    symbol_table: FxHashMap<StrId, SymbolData<T>>,
    pub parent_scope: Option<usize>, // points to the index in the global flattened scope vec
    scope_kind: BlockKind,
}

impl<T: IsInitialized> CoreScope<T> {
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
pub struct Scope<T: IsInitialized> {
    pub flattened_vec: Vec<CoreScope<T>>,
}

impl<T: IsInitialized> Scope<T> {
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
        self.variables.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_functions_namespace(
        &self,
        scope_index: usize,
        key: &StrId,
    ) -> LookupResult<FunctionSymbolData> {
        self.functions.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_types_namespace(
        &self,
        scope_index: usize,
        key: &StrId,
    ) -> LookupResult<UserDefinedTypeSymbolData> {
        self.types.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_interfaces_namespace(
        &self,
        scope_index: usize,
        key: &StrId,
    ) -> LookupResult<InterfaceSymbolData> {
        self.interfaces.lookup_with_is_init(scope_index, key).into()
    }

    pub fn declare_variable(
        &mut self,
        scope_index: usize,
        name: StrId,
        decl_range: TextRange,
        unique_id: IdentDeclId<VariableData>,
    ) -> Result<VariableSymbolData, (StrId, TextRange)> {
        let lookup_func = |scope: &Scope<VariableData>, scope_index: usize, key: &StrId| {
            scope.flattened_vec[scope_index]
                .get(key)
                .map(|symbol_data| symbol_data.declaration_line_number())
        };
        Ok(self
            .variables
            .insert(
                scope_index,
                name,
                VariableData::default(),
                decl_range,
                lookup_func,
                unique_id,
            )?
            .into())
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
        Ok(self
            .variables
            .insert(
                scope_index,
                name,
                VariableData::new(variable_type, is_init),
                decl_range,
                lookup_func,
                unique_id,
            )?
            .into())
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
        Ok(self
            .functions
            .insert(
                scope_index,
                name,
                CallableData::default_for_kind(CallableKind::Function),
                decl_range,
                lookup_func,
                unique_id,
            )?
            .into())
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
        Ok(self
            .types
            .insert(
                scope_index,
                name,
                meta_data,
                decl_range,
                lookup_func,
                unique_id,
            )?
            .into())
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
        Ok(self
            .interfaces
            .insert(
                scope_index,
                name,
                InterfaceData::default(),
                decl_range,
                lookup_func,
                unique_id,
            )?
            .into())
    }
}

impl Default for Namespace {
    fn default() -> Self {
        Namespace::new()
    }
}*/
