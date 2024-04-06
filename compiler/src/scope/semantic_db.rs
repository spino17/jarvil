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
use crate::{
    ast::ast::{
        BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, OkIdentifierInDeclNode,
        OkIdentifierInUseNode, OkSelfKeywordNode, TypeExpressionNode,
    },
    core::string_interner::Interner,
    types::core::{Type, TypeStringifyContext},
};
use crate::{builtin::builtin_funcs, scope::mangled::MangledIdentifierName};
use rustc_hash::{FxHashMap, FxHashSet};
use text_size::TextRange;

// This contains all the relevant semantic information collected over various AST passes
pub struct SemanticStateDatabase {
    namespace: Namespace,
    interner: Interner,
    unique_key_generator: GlobalUniqueKeyGenerator,
    identifier_in_decl_binding_table: FxHashMap<OkIdentifierInDeclNode, SymbolDataEntry>,
    identifier_in_use_binding_table: FxHashMap<OkIdentifierInUseNode, ConcreteSymbolDataEntry>,
    ty_expr_obj_table: FxHashMap<TypeExpressionNode, Type>,
    self_keyword_binding_table: FxHashMap<OkSelfKeywordNode, SymbolIndex<VariableData>>, // `self` (node) -> scope_index
    block_non_locals: FxHashMap<BlockNode, FxHashSet<MangledIdentifierName<VariableData>>>,
    bounded_method_kind: FxHashMap<BoundedMethodWrapperNode, BoundedMethodKind>,
}

impl SemanticStateDatabase {
    pub fn new() -> Self {
        let interner = Interner::default();
        let mut namespace = Namespace::new(&interner);

        // fill the built-in functions inside the global namespace
        let builtin_funcs = builtin_funcs(&interner);
        for (name, callable_data) in builtin_funcs {
            namespace.funcs_mut_ref().force_insert(
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
            ty_expr_obj_table: FxHashMap::default(),
            self_keyword_binding_table: FxHashMap::default(),
            block_non_locals: FxHashMap::default(),
            bounded_method_kind: FxHashMap::default(),
        }
    }

    pub fn err_logging_context(&self) -> TypeStringifyContext {
        TypeStringifyContext::new(&self.interner, &self.namespace)
    }

    pub fn namespace_ref(&self) -> &Namespace {
        &self.namespace
    }

    pub fn namespace_mut_ref(&mut self) -> &mut Namespace {
        &mut self.namespace
    }

    pub fn unique_key_generator_mut_ref(&mut self) -> &mut GlobalUniqueKeyGenerator {
        &mut self.unique_key_generator
    }

    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    pub fn identifier_in_decl_binding_table_ref(
        &self,
    ) -> &FxHashMap<OkIdentifierInDeclNode, SymbolDataEntry> {
        &self.identifier_in_decl_binding_table
    }

    pub fn identifier_in_decl_binding_table_mut_ref(
        &mut self,
    ) -> &mut FxHashMap<OkIdentifierInDeclNode, SymbolDataEntry> {
        &mut self.identifier_in_decl_binding_table
    }

    pub fn identifier_in_use_binding_table_ref(
        &self,
    ) -> &FxHashMap<OkIdentifierInUseNode, ConcreteSymbolDataEntry> {
        &self.identifier_in_use_binding_table
    }

    pub fn identifier_in_use_binding_table_mut_ref(
        &mut self,
    ) -> &mut FxHashMap<OkIdentifierInUseNode, ConcreteSymbolDataEntry> {
        &mut self.identifier_in_use_binding_table
    }

    pub fn self_keyword_binding_table_ref(
        &self,
    ) -> &FxHashMap<OkSelfKeywordNode, SymbolIndex<VariableData>> {
        &self.self_keyword_binding_table
    }

    pub fn self_keyword_binding_table_mut_ref(
        &mut self,
    ) -> &mut FxHashMap<OkSelfKeywordNode, SymbolIndex<VariableData>> {
        &mut self.self_keyword_binding_table
    }

    pub fn set_ty_expr_obj_mapping(&mut self, ty_expr: &TypeExpressionNode, ty: &Type) {
        self.ty_expr_obj_table.insert(ty_expr.clone(), ty.clone());
    }

    pub fn ty_from_expr(&self, ty_expr: &TypeExpressionNode) -> Type {
        match self.ty_expr_obj_table.get(ty_expr) {
            Some(ty) => ty.clone(),
            None => unreachable!(),
        }
    }

    pub fn symbol_for_identifier_in_decl(
        &self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Option<&SymbolDataEntry> {
        self.identifier_in_decl_binding_table.get(identifier)
    }

    pub fn symbol_for_identifier_in_use(
        &self,
        identifier: &OkIdentifierInUseNode,
    ) -> Option<ConcreteSymbolDataEntry> {
        self.identifier_in_use_binding_table
            .get(identifier)
            .cloned()
    }

    pub fn variable_symbol_ref(&self, symbol_index: SymbolIndex<VariableData>) -> &VariableData {
        self.namespace
            .variables_ref()
            .symbol_ref(symbol_index)
            .data_ref()
    }

    pub fn variable_symbol_mut_ref(
        &mut self,
        symbol_index: SymbolIndex<VariableData>,
    ) -> &mut VariableData {
        self.namespace
            .variables_mut_ref()
            .symbol_mut_ref(symbol_index)
            .data_mut_ref()
    }

    pub fn variable_symbol_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<SymbolIndex<VariableData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_entry) => match symbol_entry {
                SymbolDataEntry::Variable(symbol_index) => Some(*symbol_index),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn variable_symbol_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolIndex<VariableData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(concrete_symbol_entry) => match concrete_symbol_entry {
                ConcreteSymbolDataEntry::Variable(concrete_symbol_index) => {
                    Some(concrete_symbol_index)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn func_symbol_ref(&self, symbol_index: SymbolIndex<CallableData>) -> &CallableData {
        self.namespace
            .funcs_ref()
            .symbol_ref(symbol_index)
            .data_ref()
    }

    pub fn func_symbol_mut_ref(
        &mut self,
        symbol_index: SymbolIndex<CallableData>,
    ) -> &mut CallableData {
        self.namespace
            .funcs_mut_ref()
            .symbol_mut_ref(symbol_index)
            .data_mut_ref()
    }

    pub fn func_symbol_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<SymbolIndex<CallableData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_entry) => match symbol_entry {
                SymbolDataEntry::Function(symbol_index) => Some(*symbol_index),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn func_symbol_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolIndex<CallableData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(concrete_symbol_entry) => match concrete_symbol_entry {
                ConcreteSymbolDataEntry::Function(concrete_symbol_index) => {
                    Some(concrete_symbol_index)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn ty_symbol_ref(
        &self,
        symbol_index: SymbolIndex<UserDefinedTypeData>,
    ) -> &UserDefinedTypeData {
        self.namespace
            .types_ref()
            .symbol_ref(symbol_index)
            .data_ref()
    }

    pub fn ty_symbol_mut_ref(
        &mut self,
        symbol_index: SymbolIndex<UserDefinedTypeData>,
    ) -> &mut UserDefinedTypeData {
        self.namespace
            .types_mut_ref()
            .symbol_mut_ref(symbol_index)
            .data_mut_ref()
    }

    pub fn ty_symbol_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<SymbolIndex<UserDefinedTypeData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_entry) => match symbol_entry {
                SymbolDataEntry::Type(symbol_index) => Some(*symbol_index),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn ty_symbol_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<ConcreteSymbolIndex<UserDefinedTypeData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(concrete_symbol_entry) => match concrete_symbol_entry {
                ConcreteSymbolDataEntry::Type(concrete_symbol_index) => {
                    Some(concrete_symbol_index.clone())
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn interface_symbol_ref(&self, symbol_index: SymbolIndex<InterfaceData>) -> &InterfaceData {
        self.namespace
            .interfaces_ref()
            .symbol_ref(symbol_index)
            .data_ref()
    }

    pub fn interface_symbol_mut_ref(
        &mut self,
        symbol_index: SymbolIndex<InterfaceData>,
    ) -> &mut InterfaceData {
        self.namespace
            .interfaces_mut_ref()
            .symbol_mut_ref(symbol_index)
            .data_mut_ref()
    }

    pub fn interface_symbol_for_identifier_in_decl(
        &self,
        node: &OkIdentifierInDeclNode,
    ) -> Option<SymbolIndex<InterfaceData>> {
        match self.identifier_in_decl_binding_table.get(node) {
            Some(symbol_entry) => match symbol_entry {
                SymbolDataEntry::Interface(symbol_index) => Some(*symbol_index),
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn interface_symbol_for_identifier_in_use(
        &self,
        node: &OkIdentifierInUseNode,
    ) -> Option<&ConcreteSymbolIndex<InterfaceData>> {
        match self.identifier_in_use_binding_table.get(node) {
            Some(concrete_symbol_entry) => match concrete_symbol_entry {
                ConcreteSymbolDataEntry::Interface(concrete_symbol_index) => {
                    Some(concrete_symbol_index)
                }
                _ => unreachable!(),
            },
            None => None,
        }
    }

    pub fn self_keyword_symbol_ref(
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

    pub fn non_locals_ref(
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

    pub fn bounded_kind_ref(
        &self,
        bounded_method_wrapper: &BoundedMethodWrapperNode,
    ) -> Option<&BoundedMethodKind> {
        self.bounded_method_kind.get(bounded_method_wrapper)
    }
}
