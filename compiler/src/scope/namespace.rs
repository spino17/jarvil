use super::{
    scope::{ScopeArena, ScopeIndex},
    symbol::{
        core::{IdentDeclId, SymbolIndex},
        function::{CallableData, CallableKind, FunctionSymbolData},
        interfaces::{InterfaceBounds, InterfaceData, InterfaceSymbolData},
        types::{
            core::{UserDefinedTypeData, UserDefinedTypeSymbolData},
            generic_type::{GenericTypeData, GenericTypeParams},
            lambda_type::LambdaTypeData,
        },
        variables::{VariableData, VariableSymbolData},
    },
};
use crate::{
    core::string_interner::Interner,
    scope::symbol::types::generic_type::GenericTypeDeclarationPlaceCategory,
};
use crate::{core::string_interner::StrId, parser::resolver::BlockKind};
use crate::{scope::lookup::LookupResult, types::core::Type};
use text_size::TextRange;

#[derive(Debug)]
pub struct Namespace {
    pub variables: ScopeArena<VariableData>,
    pub types: ScopeArena<UserDefinedTypeData>,
    pub functions: ScopeArena<CallableData>,
    pub interfaces: ScopeArena<InterfaceData>,
}

impl Namespace {
    pub fn new(interner: &Interner) -> Self {
        let mut namespace = Namespace {
            variables: ScopeArena::new(),
            types: ScopeArena::new(),
            functions: ScopeArena::new(),
            interfaces: ScopeArena::new(),
        };

        // filling side scope for types with generic types with indexes 0 and 1
        namespace.types[ScopeIndex::side()].set(
            interner.intern("T"),
            UserDefinedTypeData::Generic(GenericTypeData {
                index: 0,
                category: GenericTypeDeclarationPlaceCategory::InStruct,
                interface_bounds: InterfaceBounds::default(),
            }),
            TextRange::default(),
            None,
        );
        namespace.types[ScopeIndex::side()].set(
            interner.intern("U"),
            UserDefinedTypeData::Generic(GenericTypeData {
                index: 1,
                category: GenericTypeDeclarationPlaceCategory::InStruct,
                interface_bounds: InterfaceBounds::default(),
            }),
            TextRange::default(),
            None,
        );

        namespace
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
        self.functions.add_new_scope(curr_scope_index, scope_kind);
        self.interfaces.add_new_scope(curr_scope_index, scope_kind)
    }

    pub fn get_from_variables_namespace(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> Option<SymbolIndex<VariableData>> {
        self.variables.get(scope_index, key)
    }

    pub fn get_from_functions_namespace(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> Option<SymbolIndex<CallableData>> {
        self.functions.get(scope_index, key)
    }

    pub fn get_from_types_namespace(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> Option<SymbolIndex<UserDefinedTypeData>> {
        self.types.get(scope_index, key)
    }

    pub fn get_from_interfaces_namespace(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> Option<SymbolIndex<InterfaceData>> {
        self.interfaces.get(scope_index, key)
    }

    pub fn lookup_in_variables_namespace(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> LookupResult<VariableSymbolData> {
        self.variables.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_functions_namespace(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> LookupResult<FunctionSymbolData> {
        self.functions.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_types_namespace(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> LookupResult<UserDefinedTypeSymbolData> {
        self.types.lookup_with_is_init(scope_index, key).into()
    }

    pub fn lookup_in_interfaces_namespace(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> LookupResult<InterfaceSymbolData> {
        self.interfaces.lookup_with_is_init(scope_index, key).into()
    }

    pub fn declare_variable(
        &mut self,
        scope_index: ScopeIndex,
        name: StrId,
        decl_range: TextRange,
        unique_id: IdentDeclId<VariableData>,
    ) -> Result<VariableSymbolData, (StrId, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<VariableData>, scope_index: ScopeIndex, key: StrId| {
                scope
                    .get(scope_index, key)
                    .map(|symbol_index| symbol_index.declaration_line_number(scope))
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
        scope_index: ScopeIndex,
        name: StrId,
        variable_type: &Type,
        decl_range: TextRange,
        is_init: bool,
        unique_id: IdentDeclId<VariableData>,
    ) -> Result<VariableSymbolData, (StrId, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<VariableData>, scope_index: ScopeIndex, key: StrId| {
                scope
                    .get(scope_index, key)
                    .map(|symbol_index| symbol_index.declaration_line_number(scope))
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
        scope_index: ScopeIndex,
        name: StrId,
        decl_range: TextRange,
        unique_id: IdentDeclId<CallableData>,
    ) -> Result<FunctionSymbolData, (StrId, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<CallableData>, scope_index: ScopeIndex, key: StrId| {
                scope
                    .get(scope_index, key)
                    .map(|symbol_index| symbol_index.declaration_line_number(scope))
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
        scope_index: ScopeIndex,
        name: StrId,
        meta_data: UserDefinedTypeData,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
    ) -> Result<UserDefinedTypeSymbolData, (StrId, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<UserDefinedTypeData>, scope_index: ScopeIndex, key: StrId| {
                scope
                    .lookup(scope_index, key)
                    .map(|(symbol_index, _, _)| symbol_index.declaration_line_number(scope))
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
        scope_index: ScopeIndex,
        name: StrId,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
    ) -> Result<UserDefinedTypeSymbolData, (StrId, TextRange)> {
        let meta_data = UserDefinedTypeData::default_with_struct();
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_enum_type(
        &mut self,
        scope_index: ScopeIndex,
        name: StrId,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
    ) -> Result<UserDefinedTypeSymbolData, (StrId, TextRange)> {
        let meta_data = UserDefinedTypeData::default_with_enum();
        self.declare_user_defined_type(scope_index, name, meta_data, decl_range, unique_id)
    }

    pub fn declare_lambda_type_with_meta_data(
        &mut self,
        scope_index: ScopeIndex,
        name: StrId,
        param_types: Vec<Type>,
        return_type: Type,
        is_concretization_required: Option<(Vec<usize>, bool)>,
        generics_spec: Option<GenericTypeParams>,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
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
        scope_index: ScopeIndex,
        name: StrId,
        index: usize,
        category: GenericTypeDeclarationPlaceCategory,
        interface_bounds: &InterfaceBounds,
        decl_range: TextRange,
        unique_id: IdentDeclId<UserDefinedTypeData>,
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
        scope_index: ScopeIndex,
        name: StrId,
        decl_range: TextRange,
        unique_id: IdentDeclId<InterfaceData>,
    ) -> Result<InterfaceSymbolData, (StrId, TextRange)> {
        let lookup_func =
            |scope: &ScopeArena<InterfaceData>, scope_index: ScopeIndex, key: StrId| {
                scope
                    .lookup(scope_index, key)
                    .map(|(symbol_index, _, _)| symbol_index.declaration_line_number(scope))
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
