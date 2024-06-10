use super::{
    helper::fill_side_scope_with_generic_types,
    scope::{ScopeArena, ScopeIndex},
    symbol::{
        core::{IdentDeclId, SymbolIndex},
        function::{CallableData, CallableKind, FunctionSymbolData},
        interfaces::{InterfaceBounds, InterfaceData, InterfaceSymbolData},
        types::{
            core::{UserDefinedTypeData, UserDefinedTypeSymbolData},
            generic_ty::{GenericTypeData, GenericTypeParams},
            lambda_ty::LambdaTypeData,
        },
        variables::{VariableData, VariableSymbolData},
    },
};
use crate::{core::string_interner::IdentName, parser::resolver::BlockKind};
use crate::{
    core::string_interner::Interner,
    scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory,
};
use crate::{scope::lookup::LookupResult, types::core::Type};
use text_size::TextRange;

#[derive(Debug)]
pub struct Namespace {
    variables: ScopeArena<VariableData>,
    funcs: ScopeArena<CallableData>,
    types: ScopeArena<UserDefinedTypeData>,
    interfaces: ScopeArena<InterfaceData>,
}

impl Namespace {
    pub fn new(interner: &Interner) -> Self {
        let mut namespace = Namespace {
            variables: ScopeArena::new(),
            types: ScopeArena::new(),
            funcs: ScopeArena::new(),
            interfaces: ScopeArena::new(),
        };

        fill_side_scope_with_generic_types(&mut namespace, interner);

        namespace
    }

    pub fn variables_ref(&self) -> &ScopeArena<VariableData> {
        &self.variables
    }

    pub fn variables_mut_ref(&mut self) -> &mut ScopeArena<VariableData> {
        &mut self.variables
    }

    pub fn funcs_ref(&self) -> &ScopeArena<CallableData> {
        &self.funcs
    }

    pub fn funcs_mut_ref(&mut self) -> &mut ScopeArena<CallableData> {
        &mut self.funcs
    }

    pub fn types_ref(&self) -> &ScopeArena<UserDefinedTypeData> {
        &self.types
    }

    pub fn types_mut_ref(&mut self) -> &mut ScopeArena<UserDefinedTypeData> {
        &mut self.types
    }

    pub fn interfaces_ref(&self) -> &ScopeArena<InterfaceData> {
        &self.interfaces
    }

    pub fn interfaces_mut_ref(&mut self) -> &mut ScopeArena<InterfaceData> {
        &mut self.interfaces
    }

    pub fn parent_scope_index(&self, scope_index: ScopeIndex) -> Option<ScopeIndex> {
        self.variables.parent_scope(scope_index)
    }

    pub fn open_scope(
        &mut self,
        curr_scope_index: ScopeIndex,
        scope_kind: BlockKind,
    ) -> ScopeIndex {
        self.variables.add_new_scope(curr_scope_index, scope_kind);
        self.types.add_new_scope(curr_scope_index, scope_kind);
        self.funcs.add_new_scope(curr_scope_index, scope_kind);
        self.interfaces.add_new_scope(curr_scope_index, scope_kind)
    }

    pub fn from_variables_namespace(
        &self,
        scope_index: ScopeIndex,
        key: IdentName,
    ) -> Option<SymbolIndex<VariableData>> {
        self.variables.get(scope_index, key)
    }

    pub fn from_funcs_namespace(
        &self,
        scope_index: ScopeIndex,
        key: IdentName,
    ) -> Option<SymbolIndex<CallableData>> {
        self.funcs.get(scope_index, key)
    }

    pub fn from_types_namespace(
        &self,
        scope_index: ScopeIndex,
        key: IdentName,
    ) -> Option<SymbolIndex<UserDefinedTypeData>> {
        self.types.get(scope_index, key)
    }

    pub fn from_interfaces_namespace(
        &self,
        scope_index: ScopeIndex,
        key: IdentName,
    ) -> Option<SymbolIndex<InterfaceData>> {
        self.interfaces.get(scope_index, key)
    }

    pub fn lookup_in_variables_namespace(
        &self,
        scope_index: ScopeIndex,
        key: IdentName,
    ) -> LookupResult<VariableSymbolData> {
        self.variables.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_funcs_namespace(
        &self,
        scope_index: ScopeIndex,
        key: IdentName,
    ) -> LookupResult<FunctionSymbolData> {
        self.funcs.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_types_namespace(
        &self,
        scope_index: ScopeIndex,
        key: IdentName,
    ) -> LookupResult<UserDefinedTypeSymbolData> {
        self.types.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_interfaces_namespace(
        &self,
        scope_index: ScopeIndex,
        key: IdentName,
    ) -> LookupResult<InterfaceSymbolData> {
        self.interfaces.lookup_with_is_init(scope_index, key).into()
    }

    pub fn declare_variable(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        decl_range: TextRange,
        unique_id: IdentDeclId<VariableData>,
    ) -> Result<VariableSymbolData, (IdentName, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<VariableData>, scope_index: ScopeIndex, key: IdentName| {
                scope
                    .get(scope_index, key)
                    .map(|symbol_index| symbol_index.decl_line_number(scope))
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

    pub fn declare_variable_with_ty(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        variable_ty: &Type,
        decl_range: TextRange,
        is_init: bool,
        unique_id: IdentDeclId<VariableData>,
    ) -> Result<VariableSymbolData, (IdentName, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<VariableData>, scope_index: ScopeIndex, key: IdentName| {
                scope
                    .get(scope_index, key)
                    .map(|symbol_index| symbol_index.decl_line_number(scope))
            };

        Ok(self
            .variables
            .insert(
                scope_index,
                name,
                VariableData::new(variable_ty, is_init),
                decl_range,
                lookup_func,
                unique_id,
            )?
            .into())
    }

    pub fn declare_func(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        decl_range: TextRange,
        unique_id: IdentDeclId<CallableData>,
    ) -> Result<FunctionSymbolData, (IdentName, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<CallableData>, scope_index: ScopeIndex, key: IdentName| {
                scope
                    .get(scope_index, key)
                    .map(|symbol_index| symbol_index.decl_line_number(scope))
            };

        Ok(self
            .funcs
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

    pub fn declare_user_defined_ty(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        meta_data: UserDefinedTypeData,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
    ) -> Result<UserDefinedTypeSymbolData, (IdentName, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<UserDefinedTypeData>, scope_index: ScopeIndex, key: IdentName| {
                scope
                    .lookup(scope_index, key)
                    .map(|(symbol_index, _, _)| symbol_index.decl_line_number(scope))
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

    pub fn declare_struct_ty(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
    ) -> Result<UserDefinedTypeSymbolData, (IdentName, TextRange)> {
        let meta_data = UserDefinedTypeData::default_with_struct();

        self.declare_user_defined_ty(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_enum_ty(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
    ) -> Result<UserDefinedTypeSymbolData, (IdentName, TextRange)> {
        let meta_data = UserDefinedTypeData::default_with_enum();

        self.declare_user_defined_ty(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_lambda_ty_with_meta_data(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        param_types: Vec<Type>,
        return_ty: Type,
        generics_spec: Option<GenericTypeParams>,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
    ) -> Result<UserDefinedTypeSymbolData, (IdentName, TextRange)> {
        let meta_data =
            UserDefinedTypeData::Lambda(LambdaTypeData::new(param_types, return_ty, generics_spec));

        self.declare_user_defined_ty(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_generic_ty_with_meta_data(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        index: usize,
        category: GenericTypeDeclarationPlaceCategory,
        interface_bounds: &InterfaceBounds,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
    ) -> Result<UserDefinedTypeSymbolData, (IdentName, TextRange)> {
        let meta_data = UserDefinedTypeData::Generic(GenericTypeData::new(
            index,
            category,
            interface_bounds.clone(),
        ));

        self.declare_user_defined_ty(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_interface(
        &mut self,
        scope_index: ScopeIndex,
        name: IdentName,
        decl_range: TextRange,
        unique_id: IdentDeclId<InterfaceData>,
    ) -> Result<InterfaceSymbolData, (IdentName, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<InterfaceData>, scope_index: ScopeIndex, key: IdentName| {
                scope
                    .lookup(scope_index, key)
                    .map(|(symbol_index, _, _)| symbol_index.decl_line_number(scope))
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
