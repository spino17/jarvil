use super::{
    core::{Namespace, NamespaceKind, SymbolData},
    function::FunctionData,
    user_defined_types::UserDefinedTypeData,
    variables::VariableData,
};
use crate::{
    ast::ast::{OkIdentifierNode, OkSelfKeywordNode},
    code::JarvilCode,
};
use rustc_hash::FxHashMap;

pub enum SymbolDataRef<'a> {
    VARIABLE(&'a SymbolData<VariableData>),
    FUNCTION(&'a SymbolData<FunctionData>),
    TYPE(&'a SymbolData<UserDefinedTypeData>),
}

pub struct NamespaceHandler {
    pub namespace: Namespace,
    pub identifier_binding_table: FxHashMap<OkIdentifierNode, (usize, NamespaceKind)>,
    pub self_keyword_binding_table: FxHashMap<OkSelfKeywordNode, usize>,
}

impl NamespaceHandler {
    pub fn new() -> Self {
        NamespaceHandler {
            namespace: Namespace::new(),
            identifier_binding_table: FxHashMap::default(),
            self_keyword_binding_table: FxHashMap::default(),
        }
    }

    pub fn get_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
        code: &JarvilCode,
    ) -> Option<SymbolDataRef> {
        match self.identifier_binding_table.get(node) {
            Some((scope_index, namespace_kind)) => {
                let name = node.token_value(code);
                match namespace_kind {
                    NamespaceKind::VARIABLE => {
                        match self
                            .namespace
                            .get_from_variables_namespace(*scope_index, &name)
                        {
                            Some(symbol_data) => return Some(SymbolDataRef::VARIABLE(symbol_data)),
                            None => unreachable!(),
                        }
                    }
                    NamespaceKind::FUNCTION => {
                        match self
                            .namespace
                            .get_from_functions_namespace(*scope_index, &name)
                        {
                            Some(symbol_data) => return Some(SymbolDataRef::FUNCTION(symbol_data)),
                            None => unreachable!(),
                        }
                    }
                    NamespaceKind::TYPE => {
                        match self.namespace.get_from_types_namespace(*scope_index, &name) {
                            Some(symbol_data) => return Some(SymbolDataRef::TYPE(symbol_data)),
                            None => unreachable!(),
                        }
                    }
                }
            }
            None => None,
        }
    }

    pub fn get_variable_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
        code: &JarvilCode,
    ) -> Option<&SymbolData<VariableData>> {
        match self.identifier_binding_table.get(node) {
            Some((scope_index, namespace_kind)) => match namespace_kind {
                NamespaceKind::VARIABLE => {
                    let name = node.token_value(code);
                    return self
                        .namespace
                        .get_from_variables_namespace(*scope_index, &name);
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_function_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
        code: &JarvilCode,
    ) -> Option<&SymbolData<FunctionData>> {
        match self.identifier_binding_table.get(node) {
            Some((scope_index, namespace_kind)) => match namespace_kind {
                NamespaceKind::FUNCTION => {
                    let name = node.token_value(code);
                    return self
                        .namespace
                        .get_from_functions_namespace(*scope_index, &name);
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_type_symbol_data_ref(
        &self,
        node: &OkIdentifierNode,
        code: &JarvilCode,
    ) -> Option<SymbolData<UserDefinedTypeData>> {
        match self.identifier_binding_table.get(node) {
            Some((scope_index, namespace_kind)) => match namespace_kind {
                NamespaceKind::TYPE => {
                    let name = node.token_value(code);
                    return self
                        .namespace
                        .get_from_types_namespace(*scope_index, &name)
                        .cloned();
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
        match self.self_keyword_binding_table.get(node) {
            Some(&scope_index) => {
                return self
                    .namespace
                    .get_from_variables_namespace(scope_index, "self")
            }
            None => return None,
        }
    }
}
