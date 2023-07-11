use super::{
    core::{GlobalSymbolDataRegistry, Namespace, SymbolData},
    function::CallableData,
    interfaces::InterfaceData,
    types::core::UserDefinedTypeData,
    variables::VariableData,
};
use crate::ast::ast::{
    BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, OkIdentifierNode, OkSelfKeywordNode,
};
use rustc_hash::{FxHashMap, FxHashSet};

pub enum SymbolDataEntry {
    Variable(usize),
    Function(usize),
    Type(usize),
    Interface(usize),
}

pub struct NamespaceHandler<'a> {
    pub namespace: Namespace,
    pub symbol_data_registry: &'a mut GlobalSymbolDataRegistry,
    pub identifier_binding_table: FxHashMap<OkIdentifierNode, SymbolDataEntry>, // node -> (scope_index, namespace_kind)
    pub self_keyword_binding_table: FxHashMap<OkSelfKeywordNode, usize>, // `self` (node) -> scope_index
    pub block_non_locals: FxHashMap<BlockNode, (FxHashSet<String>, FxHashMap<String, bool>)>, // block_node -> (non_locally resolved variables, (non_locally resolved functions -> is_in_global_scope))
    pub bounded_method_kind: FxHashMap<BoundedMethodWrapperNode, BoundedMethodKind>,
}

impl<'a> NamespaceHandler<'a> {
    pub fn new(symbol_data_registry: &'a mut GlobalSymbolDataRegistry) -> Self {
        NamespaceHandler {
            namespace: Namespace::new(),
            symbol_data_registry,
            identifier_binding_table: FxHashMap::default(),
            self_keyword_binding_table: FxHashMap::default(),
            block_non_locals: FxHashMap::default(),
            bounded_method_kind: FxHashMap::default(),
        }
    }

    pub fn get_symbol_data_ref(&self, node: &OkIdentifierNode) -> Option<&SymbolDataEntry> {
        self.identifier_binding_table.get(node)
    }

    pub fn get_variable_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
    ) -> Option<&SymbolData<VariableData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Variable(symbol_data_index) => {
                    let variable_symbol_data = self
                        .symbol_data_registry
                        .get_variables_symbol_data_ref_at_index(*symbol_data_index);
                    Some(variable_symbol_data)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_variable_symbol_data_mut_ref(
        &mut self,
        node: &OkIdentifierNode,
    ) -> Option<&mut SymbolData<VariableData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Variable(symbol_data_index) => {
                    let variable_symbol_data = self
                        .symbol_data_registry
                        .get_variables_symbol_data_mut_ref_at_index(*symbol_data_index);
                    Some(variable_symbol_data)
                }
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
                SymbolDataEntry::Function(func_symbol_data_index) => {
                    let func_symbol_data = self
                        .symbol_data_registry
                        .get_functions_symbol_data_ref_at_index(*func_symbol_data_index);
                    Some(func_symbol_data)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_function_symbol_data_mut_ref(
        &mut self,
        node: &OkIdentifierNode,
    ) -> Option<&mut SymbolData<CallableData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Function(func_symbol_data_index) => {
                    let func_symbol_data = self
                        .symbol_data_registry
                        .get_functions_symbol_data_mut_ref_at_index(*func_symbol_data_index);
                    Some(func_symbol_data)
                }
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
                SymbolDataEntry::Type(type_symbol_data_index) => {
                    let type_symbol_data = self
                        .symbol_data_registry
                        .get_types_symbol_data_ref_at_index(*type_symbol_data_index);
                    Some(type_symbol_data)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_type_symbol_data_mut_ref(
        &mut self,
        node: &OkIdentifierNode,
    ) -> Option<&mut SymbolData<UserDefinedTypeData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Type(type_symbol_data_index) => {
                    let type_symbol_data = self
                        .symbol_data_registry
                        .get_types_symbol_data_mut_ref_at_index(*type_symbol_data_index);
                    Some(type_symbol_data)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_interface_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
    ) -> Option<&SymbolData<InterfaceData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Interface(interface_symbol_data_index) => {
                    let type_symbol_data = self
                        .symbol_data_registry
                        .get_interfaces_symbol_data_ref_at_index(*interface_symbol_data_index);
                    Some(type_symbol_data)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_interface_symbol_data_mut_ref(
        &mut self,
        node: &OkIdentifierNode,
    ) -> Option<&mut SymbolData<InterfaceData>> {
        match self.identifier_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Interface(interface_symbol_data_index) => {
                    let type_symbol_data = self
                        .symbol_data_registry
                        .get_interfaces_symbol_data_mut_ref_at_index(*interface_symbol_data_index);
                    Some(type_symbol_data)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_self_keyword_symbol_data_ref(&self, node: &OkSelfKeywordNode) -> Option<&usize> {
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
