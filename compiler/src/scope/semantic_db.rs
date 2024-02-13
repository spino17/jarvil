use super::{
    concrete::ConcreteSymbolIndex,
    namespace::Namespace,
    scope::ScopeIndex,
    symbol::{
        common::GlobalUniqueKeyGenerator,
        core::{ConcreteSymbolDataEntry, SymbolDataEntry, SymbolIndex},
        function::CallableData,
        interfaces::InterfaceData,
        types::core::UserDefinedTypeData,
        variables::VariableData,
    },
};
use crate::builtin::get_builtin_functions;
use crate::scope::mangled::MangledIdentifierName;
use crate::{
    ast::ast::{
        BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, OkIdentifierInDeclNode,
        OkIdentifierInUseNode, OkSelfKeywordNode, TypeExpressionNode,
    },
    core::string_interner::Interner,
    types::core::Type,
};
use rustc_hash::{FxHashMap, FxHashSet};
use text_size::TextRange;

// This contains all the relevant semantic information collected over various AST passes
pub struct SemanticStateDatabase {
    pub namespace: Namespace,
    pub interner: Interner,
    pub unique_key_generator: GlobalUniqueKeyGenerator,
    pub identifier_in_decl_binding_table: FxHashMap<OkIdentifierInDeclNode, SymbolDataEntry>,
    pub identifier_in_use_binding_table: FxHashMap<OkIdentifierInUseNode, ConcreteSymbolDataEntry>,
    pub type_expr_obj_table: FxHashMap<TypeExpressionNode, (Type, bool)>,
    pub self_keyword_binding_table: FxHashMap<OkSelfKeywordNode, SymbolIndex<VariableData>>, // `self` (node) -> scope_index
    pub block_non_locals: FxHashMap<BlockNode, FxHashSet<MangledIdentifierName<VariableData>>>,
    pub bounded_method_kind: FxHashMap<BoundedMethodWrapperNode, BoundedMethodKind>,
}

impl SemanticStateDatabase {
    pub fn new() -> Self {
        let mut namespace = Namespace::new();
        let mut interner = Interner::default();

        // fill the built-in functions inside the global namespace
        let builtin_functions = get_builtin_functions();
        for (name, callable_data) in builtin_functions {
            namespace.functions.force_insert(
                ScopeIndex::global(), // index of global namespace
                interner.intern(name),
                callable_data,
                TextRange::default(),
            );
        }

        SemanticStateDatabase {
            namespace,
            interner,
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
    ) -> Option<ConcreteSymbolDataEntry> {
        self.identifier_in_use_binding_table
            .get(identifier)
            .cloned()
    }

    pub fn get_variable_symbol_data_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<SymbolIndex<VariableData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Variable(variable_symbol_data) => Some(*variable_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_variable_symbol_data_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolIndex<VariableData>> {
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
    ) -> Option<SymbolIndex<CallableData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Function(func_symbol_data) => Some(*func_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_function_symbol_data_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolIndex<CallableData>> {
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
    ) -> Option<SymbolIndex<UserDefinedTypeData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Type(type_symbol_data) => Some(*type_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_type_symbol_data_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<ConcreteSymbolIndex<UserDefinedTypeData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                ConcreteSymbolDataEntry::Type(type_symbol_data) => Some(*type_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_interface_symbol_data_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<SymbolIndex<InterfaceData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Interface(interface_symbol_data) => Some(*interface_symbol_data),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn get_interface_symbol_data_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolIndex<InterfaceData>> {
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
    ) -> Option<SymbolIndex<VariableData>> {
        let Some(val) = self.self_keyword_binding_table.get(node) else {
            return None;
        };
        Some(*val)
    }

    pub fn set_non_locals(
        &mut self,
        block: &BlockNode,
        variable_non_locals: FxHashSet<MangledIdentifierName<VariableData>>,
    ) {
        self.block_non_locals
            .insert(block.clone(), variable_non_locals);
    }

    pub fn get_non_locals_ref(
        &self,
        block: &BlockNode,
    ) -> &FxHashSet<MangledIdentifierName<VariableData>> {
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
