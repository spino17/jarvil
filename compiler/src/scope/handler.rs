use super::{
    core::{Namespace, SymbolData, AbstractConcreteTypesHandler},
    function::CallableData,
    interfaces::InterfaceData,
    types::core::UserDefinedTypeData,
    variables::VariableData, concrete::core::ConcreteSymbolData,
};
use crate::ast::ast::{
    BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, OkIdentifierNode, OkSelfKeywordNode, OkTokenNode, OkIdentifierInUseNode,
};
use rustc_hash::{FxHashMap, FxHashSet};

pub enum SymbolDataRef<'a> {
    Variable(&'a SymbolData<VariableData>),
    Function(&'a SymbolData<CallableData>),
    Type(&'a SymbolData<UserDefinedTypeData>),
    Interface(&'a SymbolData<InterfaceData>),
}

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

pub enum IdentifierNodeWrapper<'a> {
    InDecl(&'a OkTokenNode),
    InUse(&'a OkIdentifierNode)
}

// This contains all the relevant semantic information collected over various AST passes
pub struct NamespaceHandler {
    pub namespace: Namespace,
    pub identifier_in_decl_binding_table: FxHashMap<OkTokenNode, SymbolDataEntry>,
    pub identifier_binding_table: FxHashMap<OkIdentifierNode, SymbolDataEntry>, // node -> (scope_index, namespace_kind)
    pub self_keyword_binding_table: FxHashMap<OkSelfKeywordNode, SymbolData<VariableData>>, // `self` (node) -> scope_index
    pub block_non_locals: FxHashMap<BlockNode, (FxHashSet<String>, FxHashMap<String, bool>)>, // block_node -> (non_locally resolved variables, (non_locally resolved functions -> is_in_global_scope))
    pub bounded_method_kind: FxHashMap<BoundedMethodWrapperNode, BoundedMethodKind>,
}

impl NamespaceHandler {
    pub fn new() -> Self {
        NamespaceHandler {
            namespace: Namespace::new(),
            identifier_in_decl_binding_table: FxHashMap::default(),
            identifier_binding_table: FxHashMap::default(),
            self_keyword_binding_table: FxHashMap::default(),
            block_non_locals: FxHashMap::default(),
            bounded_method_kind: FxHashMap::default(),
        }
    }

    pub fn get_symbol_data_entry_for_node(&self, node: IdentifierNodeWrapper) -> Option<&SymbolDataEntry> {
        match node {
            IdentifierNodeWrapper::InDecl(ok_token) => {
                return self.identifier_in_decl_binding_table.get(ok_token)
            }
            IdentifierNodeWrapper::InUse(ok_identifier_in_use) => {
                return self.identifier_binding_table.get(ok_identifier_in_use)
            }
        }
    }

    //pub fn get_symbol_data_entry(&self, node: &OkIdentifierNode) -> Option<&SymbolDataEntry> {
    //    self.identifier_binding_table.get(node)
    //}

    pub fn get_variable_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
    ) -> Option<&SymbolData<VariableData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Variable(variable_symbol_data) => Some(variable_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_function_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
    ) -> Option<&SymbolData<CallableData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Function(func_symbol_data) => Some(func_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_type_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
    ) -> Option<&SymbolData<UserDefinedTypeData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Type(type_symbol_data) => Some(type_symbol_data),
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
