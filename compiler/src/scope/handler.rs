use super::{
    common::GlobalUniqueKeyGenerator,
    concrete::{ConcreteSymbolData, ConcreteTypesTuple},
    core::{MangledIdentifierName, Namespace, SymbolData},
    function::CallableData,
    interfaces::InterfaceData,
    types::core::UserDefinedTypeData,
    variables::VariableData,
};
use crate::{
    ast::ast::{
        BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, OkIdentifierInDeclNode,
        OkIdentifierInUseNode, OkSelfKeywordNode, TypeExpressionNode,
    },
    types::core::Type,
};
use rustc_hash::{FxHashMap, FxHashSet};

pub enum SymbolDataEntry {
    Variable(SymbolData<VariableData>),
    Function(SymbolData<CallableData>),
    Type(SymbolData<UserDefinedTypeData>),
    Interface(SymbolData<InterfaceData>),
}

pub enum ConcreteSymbolDataEntry {
    Variable(ConcreteSymbolData<VariableData>),
    Function(ConcreteSymbolData<CallableData>),
    Type(ConcreteSymbolData<UserDefinedTypeData>),
    Interface(ConcreteSymbolData<InterfaceData>),
}

impl ConcreteSymbolDataEntry {
    pub fn new(symbol_data: SymbolDataEntry, concrete_types: Option<ConcreteTypesTuple>) -> Self {
        match symbol_data {
            SymbolDataEntry::Variable(variable_symbol_data) => ConcreteSymbolDataEntry::Variable(
                ConcreteSymbolData::new(variable_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Function(func_symbol_data) => ConcreteSymbolDataEntry::Function(
                ConcreteSymbolData::new(func_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Type(type_symbol_data) => ConcreteSymbolDataEntry::Type(
                ConcreteSymbolData::new(type_symbol_data, concrete_types),
            ),
            SymbolDataEntry::Interface(interface_symbol_data) => {
                ConcreteSymbolDataEntry::Interface(ConcreteSymbolData::new(
                    interface_symbol_data,
                    concrete_types,
                ))
            }
        }
    }
}

pub enum IdentifierNodeWrapper<'a> {
    InDecl(&'a OkIdentifierInDeclNode),
    InUse(&'a OkIdentifierInUseNode),
}

// This contains all the relevant semantic information collected over various AST passes
pub struct SemanticStateDatabase {
    pub namespace: Namespace,
    pub unique_key_generator: GlobalUniqueKeyGenerator,
    pub identifier_in_decl_binding_table: FxHashMap<OkIdentifierInDeclNode, SymbolDataEntry>,
    pub identifier_in_use_binding_table: FxHashMap<OkIdentifierInUseNode, ConcreteSymbolDataEntry>,
    pub type_expr_obj_table: FxHashMap<TypeExpressionNode, (Type, bool)>,
    pub self_keyword_binding_table: FxHashMap<OkSelfKeywordNode, SymbolData<VariableData>>, // `self` (node) -> scope_index
    pub block_non_locals: FxHashMap<BlockNode, FxHashSet<MangledIdentifierName>>,
    pub bounded_method_kind: FxHashMap<BoundedMethodWrapperNode, BoundedMethodKind>,
}

impl SemanticStateDatabase {
    pub fn new() -> Self {
        SemanticStateDatabase {
            namespace: Namespace::new(),
            unique_key_generator: GlobalUniqueKeyGenerator::default(),
            identifier_in_decl_binding_table: FxHashMap::default(),
            identifier_in_use_binding_table: FxHashMap::default(),
            type_expr_obj_table: FxHashMap::default(),
            self_keyword_binding_table: FxHashMap::default(),
            block_non_locals: FxHashMap::default(),
            bounded_method_kind: FxHashMap::default(),
        }
    }

    pub fn set_type_expr_obj_mapping(
        &mut self,
        ty_expr: &TypeExpressionNode,
        ty_obj: &Type,
        has_generics: bool,
    ) {
        self.type_expr_obj_table
            .insert(ty_expr.clone(), (ty_obj.clone(), has_generics));
    }

    pub fn get_type_obj_from_expr(&self, ty_expr: &TypeExpressionNode) -> (Type, bool) {
        match self.type_expr_obj_table.get(ty_expr) {
            Some((ty, has_generics)) => (ty.clone(), *has_generics),
            None => unreachable!(),
        }
    }

    pub fn get_symbol_data_for_identifier_in_decl(
        &self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Option<&SymbolDataEntry> {
        self.identifier_in_decl_binding_table.get(identifier)
    }

    pub fn get_symbol_data_for_identifier_in_use(
        &self,
        identifier: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolDataEntry> {
        self.identifier_in_use_binding_table.get(identifier)
    }

    pub fn get_variable_symbol_data_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<&SymbolData<VariableData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Variable(variable_symbol_data) => Some(variable_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_variable_symbol_data_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolData<VariableData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                ConcreteSymbolDataEntry::Variable(variable_symbol_data) => {
                    Some(variable_symbol_data)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_function_symbol_data_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<&SymbolData<CallableData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Function(func_symbol_data) => Some(func_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_function_symbol_data_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolData<CallableData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                ConcreteSymbolDataEntry::Function(func_symbol_data) => Some(func_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_type_symbol_data_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<&SymbolData<UserDefinedTypeData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Type(type_symbol_data) => Some(type_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_type_symbol_data_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolData<UserDefinedTypeData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                ConcreteSymbolDataEntry::Type(type_symbol_data) => Some(type_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_interface_symbol_data_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<&SymbolData<InterfaceData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Interface(interface_symbol_data) => Some(interface_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_interface_symbol_data_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolData<InterfaceData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                ConcreteSymbolDataEntry::Interface(interface_symbol_data) => {
                    Some(interface_symbol_data)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_self_keyword_symbol_data_ref(
        &self,
        node: &OkSelfKeywordNode,
    ) -> Option<&SymbolData<VariableData>> {
        self.self_keyword_binding_table.get(node)
    }

    pub fn set_non_locals(
        &mut self,
        block: &BlockNode,
        variable_non_locals: FxHashSet<MangledIdentifierName>,
    ) {
        self.block_non_locals
            .insert(block.clone(), variable_non_locals);
    }

    pub fn get_non_locals_ref(&self, block: &BlockNode) -> &FxHashSet<MangledIdentifierName> {
        match self.block_non_locals.get(block) {
            Some(variable_non_locals) => variable_non_locals,
            None => unreachable!(),
        }
    }

    pub fn set_bounded_kind(
        &mut self,
        bounded_method_wrapper: &BoundedMethodWrapperNode,
        bounded_kind: BoundedMethodKind,
    ) {
        self.bounded_method_kind
            .insert(bounded_method_wrapper.clone(), bounded_kind);
    }

    pub fn get_bounded_kind_ref(
        &self,
        bounded_method_wrapper: &BoundedMethodWrapperNode,
    ) -> Option<&BoundedMethodKind> {
        self.bounded_method_kind.get(bounded_method_wrapper)
    }
}
