use super::{
    lookup::IntermediateLookupResult,
    symbol::core::{IdentDeclId, Symbol, SymbolIndex},
    symbol::function::CallableData,
    symbol::interfaces::InterfaceData,
    symbol::types::core::UserDefinedTypeData,
    symbol::variables::VariableData,
    traits::IsInitialized,
};
use crate::{core::string_interner::StrId, parser::resolver::BlockKind};
use rustc_hash::FxHashMap;
use std::marker::PhantomData;
use text_size::TextRange;

#[derive(Debug, Clone, Copy)]
pub struct ScopeIndex(usize);

impl ScopeIndex {
    fn index(&self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub struct Scope<T> {
    table: FxHashMap<StrId, Symbol<T>>,
    parent_scope: Option<ScopeIndex>,
    scope_kind: BlockKind,
}

impl<T> Scope<T> {
    fn set(
        &mut self,
        ident_name: StrId,
        data: T,
        decl_line_number: TextRange,
        unique_id: Option<IdentDeclId>,
    ) {
        self.table.insert(
            ident_name,
            Symbol::new(ident_name, data, decl_line_number, unique_id),
        );
    }

    fn get(&self, name: &StrId) -> Option<&Symbol<T>> {
        self.table.get(name)
    }

    fn get_mut(&mut self, name: &StrId) -> Option<&mut Symbol<T>> {
        self.table.get_mut(name)
    }
}

#[derive(Debug)]
pub struct ScopeArena<T: IsInitialized> {
    arena: Vec<Box<Scope<T>>>,
}

impl<T: IsInitialized> ScopeArena<T> {
    fn new() -> Self {
        ScopeArena {
            arena: vec![Box::new(Scope {
                table: FxHashMap::default(),
                parent_scope: None,
                scope_kind: BlockKind::Function,
            })],
        }
    }

    fn add_new_scope(
        &mut self,
        parent_scope_index: ScopeIndex,
        scope_kind: BlockKind,
    ) -> ScopeIndex {
        let new_scope_index = self.arena.len();
        self.arena.push(Box::new(Scope {
            table: FxHashMap::default(),
            parent_scope: Some(parent_scope_index),
            scope_kind,
        }));
        ScopeIndex(new_scope_index)
    }

    fn set(
        &mut self,
        scope_index: ScopeIndex,
        ident_name: StrId,
        data: T,
        decl_line_number: TextRange,
        unique_id: Option<IdentDeclId>,
    ) -> SymbolIndex<T> {
        self.arena[scope_index.index()].set(ident_name, data, decl_line_number, unique_id);
        SymbolIndex {
            scope_index,
            ident_name,
            phanton: PhantomData,
        }
    }

    fn get(&self, scope_index: ScopeIndex, key: StrId) -> Option<SymbolIndex<T>> {
        if self.arena[scope_index.index()].get(&key).is_none() {
            return None;
        }
        Some(SymbolIndex {
            scope_index,
            ident_name: key,
            phanton: PhantomData,
        })
    }

    fn get_symbol_data_ref(&self, index: SymbolIndex<T>) -> &Symbol<T> {
        let scope = &self.arena[index.scope_index.index()];
        let Some(symbol_ref) = scope.get(&index.ident_name) else {
            unreachable!()
        };
        symbol_ref
    }

    fn get_symbol_data_mut_ref(&mut self, index: SymbolIndex<T>) -> &mut Symbol<T> {
        let scope = &mut self.arena[index.scope_index.index()];
        let Some(symbol_ref) = scope.get_mut(&index.ident_name) else {
            unreachable!()
        };
        symbol_ref
    }

    fn lookup(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> Option<(SymbolIndex<T>, usize, Option<usize>)> {
        let mut enclosing_func_scope_depth: Option<usize> = None;
        let scope = &self.arena[scope_index.index()];
        let mut previous_scope_kind = scope.scope_kind;
        if scope.get(&key).is_some() {
            return Some((
                SymbolIndex {
                    scope_index,
                    ident_name: key,
                    phanton: PhantomData,
                },
                0,
                None,
            ));
        }
        let mut parent_scope_index = scope.parent_scope;
        let mut depth = 1;
        while let Some(scope_index) = parent_scope_index {
            if enclosing_func_scope_depth.is_none() && previous_scope_kind.has_callable_body() {
                enclosing_func_scope_depth = Some(depth);
            }
            let scope = &self.arena[scope_index.index()];
            if scope.get(&key).is_some() {
                return Some((
                    SymbolIndex {
                        scope_index,
                        ident_name: key,
                        phanton: PhantomData,
                    },
                    depth,
                    enclosing_func_scope_depth,
                ));
            }
            parent_scope_index = scope.parent_scope;
            depth += 1;
            previous_scope_kind = scope.scope_kind;
        }
        None
    }

    fn lookup_with_is_init(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> IntermediateLookupResult<T> {
        let Some((symbol_index, depth, enclosing_func_scope_depth)) = self.lookup(scope_index, key)
        else {
            return IntermediateLookupResult::Unresolved;
        };
        let symbol_data = self.get_symbol_data_ref(symbol_index);
        if symbol_data.data.is_initialized() {
            todo!()
        } else {
            IntermediateLookupResult::NotInitialized(symbol_data.decl_line_number())
        }
    }
}

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
        self.variables.arena[scope_index.index()].parent_scope
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
