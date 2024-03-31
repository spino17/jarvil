use super::{
    lookup::IntermediateLookupResult,
    symbol::core::{IdentDeclId, Symbol, SymbolIndex},
    traits::IsInitialized,
};
use crate::{core::string_interner::StrId, parser::resolver::BlockKind};
use rustc_hash::FxHashMap;
use std::ops::Index;
use std::ops::IndexMut;
use text_size::TextRange;

#[derive(Debug)]
pub struct Scope<T> {
    table: FxHashMap<StrId, Symbol<T>>,
    parent_scope: Option<ScopeIndex>,
    scope_kind: BlockKind,
}

impl<T> Scope<T> {
    pub fn set(
        &mut self,
        ident_name: StrId,
        data: T,
        decl_line_number: TextRange,
        unique_id: Option<IdentDeclId<T>>,
    ) {
        self.table
            .insert(ident_name, Symbol::new(data, decl_line_number, unique_id));
    }

    fn get(&self, name: &StrId) -> Option<&Symbol<T>> {
        self.table.get(name)
    }

    fn get_mut(&mut self, name: &StrId) -> Option<&mut Symbol<T>> {
        self.table.get_mut(name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ScopeIndex(usize);

impl ScopeIndex {
    pub fn global() -> ScopeIndex {
        ScopeIndex(0)
    }

    pub fn side() -> ScopeIndex {
        ScopeIndex(1)
    }
}

#[derive(Debug)]
pub struct ScopeArena<T: IsInitialized> {
    arena: Vec<Box<Scope<T>>>,
}

impl<T: IsInitialized> Index<ScopeIndex> for ScopeArena<T> {
    type Output = Scope<T>;
    fn index(&self, scope_index: ScopeIndex) -> &Self::Output {
        &self.arena[scope_index.0]
    }
}

impl<T: IsInitialized> IndexMut<ScopeIndex> for ScopeArena<T> {
    fn index_mut(&mut self, scope_index: ScopeIndex) -> &mut Self::Output {
        &mut self.arena[scope_index.0]
    }
}

impl<T: IsInitialized> ScopeArena<T> {
    pub fn new() -> Self {
        ScopeArena {
            arena: vec![
                Box::new(Scope {
                    table: FxHashMap::default(),
                    parent_scope: None,
                    scope_kind: BlockKind::Function,
                }),
                Box::new(Scope {
                    // This scope is a side scope for storing things like `T` unbounded generic type used for builtin functions
                    // This scope will never be explored during name-resolution phase.
                    table: FxHashMap::default(),
                    parent_scope: None,
                    scope_kind: BlockKind::Function,
                }),
            ],
        }
    }

    fn set(
        &mut self,
        scope_index: ScopeIndex,
        ident_name: StrId,
        data: T,
        decl_line_number: TextRange,
        unique_id: Option<IdentDeclId<T>>,
    ) -> SymbolIndex<T> {
        self[scope_index].set(ident_name, data, decl_line_number, unique_id);
        SymbolIndex::new(scope_index, ident_name)
    }

    pub fn get(&self, scope_index: ScopeIndex, key: StrId) -> Option<SymbolIndex<T>> {
        if self[scope_index].get(&key).is_none() {
            return None;
        }
        Some(SymbolIndex::new(scope_index, key))
    }

    pub fn symbol_ref(&self, index: SymbolIndex<T>) -> &Symbol<T> {
        let scope = &self[index.scope_index()];
        let Some(symbol_ref) = scope.get(&index.ident_name()) else {
            unreachable!()
        };
        symbol_ref
    }

    pub fn symbol_mut_ref(&mut self, index: SymbolIndex<T>) -> &mut Symbol<T> {
        let scope = &mut self[index.scope_index()];
        let Some(symbol_ref) = scope.get_mut(&index.ident_name()) else {
            unreachable!()
        };
        symbol_ref
    }

    pub fn add_new_scope(
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

    pub fn force_insert(
        &mut self,
        scope_index: ScopeIndex,
        key: StrId,
        meta_data: T,
        decl_range: TextRange,
    ) {
        // use this method only for builtin function where we know that no entry already exist in the scope
        self.set(scope_index, key, meta_data, decl_range, None);
    }

    pub fn insert<U: Fn(&ScopeArena<T>, ScopeIndex, StrId) -> Option<TextRange>>(
        &mut self,
        scope_index: ScopeIndex,
        key: StrId,
        meta_data: T,
        decl_range: TextRange,
        lookup_fn: U,
        unique_id: IdentDeclId<T>,
    ) -> Result<SymbolIndex<T>, (StrId, TextRange)> {
        if let Some(previous_decl_range) = lookup_fn(self, scope_index, key) {
            return Err((key, previous_decl_range));
        }
        Ok(self.set(scope_index, key, meta_data, decl_range, Some(unique_id)))
    }

    pub fn lookup(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> Option<(SymbolIndex<T>, usize, Option<usize>)> {
        let mut enclosing_func_scope_depth: Option<usize> = None;
        let scope = &self[scope_index];
        let mut previous_scope_kind = scope.scope_kind;
        if scope.get(&key).is_some() {
            return Some((SymbolIndex::new(scope_index, key), 0, None));
        }
        let mut parent_scope_index = scope.parent_scope;
        let mut depth = 1;
        while let Some(scope_index) = parent_scope_index {
            if enclosing_func_scope_depth.is_none() && previous_scope_kind.has_callable_body() {
                enclosing_func_scope_depth = Some(depth);
            }
            let scope = &self[scope_index];
            if scope.get(&key).is_some() {
                return Some((
                    SymbolIndex::new(scope_index, key),
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

    pub fn lookup_with_is_init(
        &self,
        scope_index: ScopeIndex,
        key: StrId,
    ) -> IntermediateLookupResult<T> {
        let Some((symbol_index, depth, enclosing_func_scope_depth)) = self.lookup(scope_index, key)
        else {
            return IntermediateLookupResult::Unresolved;
        };
        if !self.symbol_ref(symbol_index).data_ref().is_initialized() {
            return IntermediateLookupResult::NotInitialized(
                symbol_index.declaration_line_number(self),
            );
        }
        IntermediateLookupResult::Ok((symbol_index, depth, enclosing_func_scope_depth))
    }

    pub fn parent_scope(&self, scope_index: ScopeIndex) -> Option<ScopeIndex> {
        self[scope_index].parent_scope
    }
}
