use super::{
    concrete::core::{ConcreteSymbolData, ConcreteTypesRegistryKey},
    core::{Namespace, SymbolData},
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

impl SymbolDataEntry {
    pub fn register_concrete_types(
        &self,
        concrete_types: Option<Vec<Type>>,
        has_generics: bool,
    ) -> Option<ConcreteTypesRegistryKey> {
        match self {
            SymbolDataEntry::Variable(variable_symbol_data) => {
                variable_symbol_data.register_concrete_types(concrete_types, has_generics)
            }
            SymbolDataEntry::Function(func_symbol_data) => {
                func_symbol_data.register_concrete_types(concrete_types, has_generics)
            }
            SymbolDataEntry::Type(type_symbol_data) => {
                type_symbol_data.register_concrete_types(concrete_types, has_generics)
            }
            SymbolDataEntry::Interface(interface_symbol_data) => {
                interface_symbol_data.register_concrete_types(concrete_types, has_generics)
            }
        }
    }
}

pub enum ConcreteSymbolDataEntry {
    Variable(ConcreteSymbolData<VariableData>),
    Function(ConcreteSymbolData<CallableData>),
    Type(ConcreteSymbolData<UserDefinedTypeData>),
    Interface(ConcreteSymbolData<InterfaceData>),
}

impl ConcreteSymbolDataEntry {
    pub fn new(symbol_data: SymbolDataEntry, index: Option<ConcreteTypesRegistryKey>) -> Self {
        match symbol_data {
            SymbolDataEntry::Variable(variable_symbol_data) => ConcreteSymbolDataEntry::Variable(
                ConcreteSymbolData::new(variable_symbol_data, index),
            ),
            SymbolDataEntry::Function(func_symbol_data) => {
                ConcreteSymbolDataEntry::Function(ConcreteSymbolData::new(func_symbol_data, index))
            }
            SymbolDataEntry::Type(type_symbol_data) => {
                ConcreteSymbolDataEntry::Type(ConcreteSymbolData::new(type_symbol_data, index))
            }
            SymbolDataEntry::Interface(interface_symbol_data) => {
                ConcreteSymbolDataEntry::Interface(ConcreteSymbolData::new(
                    interface_symbol_data,
                    index,
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
    pub identifier_in_decl_binding_table: FxHashMap<OkIdentifierInDeclNode, SymbolDataEntry>,
    pub identifier_in_use_binding_table: FxHashMap<OkIdentifierInUseNode, ConcreteSymbolDataEntry>,
    pub type_expr_obj_table: FxHashMap<TypeExpressionNode, (Type, bool)>,
    pub self_keyword_binding_table: FxHashMap<OkSelfKeywordNode, SymbolData<VariableData>>, // `self` (node) -> scope_index
    pub block_non_locals: FxHashMap<BlockNode, (FxHashSet<String>, FxHashMap<String, bool>)>, // block_node -> (non_locally resolved variables, (non_locally resolved functions -> is_in_global_scope))
    pub bounded_method_kind: FxHashMap<BoundedMethodWrapperNode, BoundedMethodKind>,
}

impl SemanticStateDatabase {
    pub fn new() -> Self {
        SemanticStateDatabase {
            namespace: Namespace::new(),
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
            Some((ty, has_generics)) => return (ty.clone(), *has_generics),
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
        variable_non_locals: FxHashSet<String>,
        function_non_locals: FxHashMap<String, bool>,
    ) {
        self.block_non_locals
            .insert(block.clone(), (variable_non_locals, function_non_locals));
    }

    pub fn get_non_locals_ref(
        &self,
        block: &BlockNode,
    ) -> (&FxHashSet<String>, &FxHashMap<String, bool>) {
        match self.block_non_locals.get(block) {
            Some((variable_non_locals, function_non_locals)) => {
                (variable_non_locals, function_non_locals)
            }
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
