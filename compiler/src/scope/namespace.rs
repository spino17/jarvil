use super::{
    scope::{ScopeArena, ScopeIndex},
    symbol::{
        function::CallableData, interfaces::InterfaceData, types::core::UserDefinedTypeData,
        variables::VariableData,
    },
};
use crate::parser::resolver::BlockKind;

#[derive(Debug)]
pub struct Namespace {
    pub variables: ScopeArena<VariableData>,
    pub types: ScopeArena<UserDefinedTypeData>,
    pub functions: ScopeArena<CallableData>,
    pub interfaces: ScopeArena<InterfaceData>,
}

impl Namespace {
    pub fn new() -> Self {
        Namespace {
            variables: ScopeArena::new(),
            types: ScopeArena::new(),
            functions: ScopeArena::new(),
            interfaces: ScopeArena::new(),
        }
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
}
