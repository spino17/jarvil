use crate::{core::string_interner::StrId, scope::new_core::ScopeIndex};
use std::marker::PhantomData;
use text_size::TextRange;

#[derive(Debug)]
pub struct Symbol<T> {
    pub ident_name: StrId,
    pub data: T,
    pub decl_line_number: TextRange,
    pub unique_id: Option<IdentDeclId>,
}

impl<T> Symbol<T> {
    pub fn new(
        ident_name: StrId,
        data: T,
        decl_line_number: TextRange,
        unique_id: Option<IdentDeclId>,
    ) -> Self {
        Symbol {
            ident_name,
            data,
            decl_line_number,
            unique_id,
        }
    }

    pub fn decl_line_number(&self) -> TextRange {
        self.decl_line_number
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdentDeclId(usize);

#[derive(Debug, Clone, Copy)]
pub struct SymbolIndex<T> {
    pub scope_index: ScopeIndex,
    pub ident_name: StrId,
    pub phanton: PhantomData<T>,
}
