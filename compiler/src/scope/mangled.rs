use super::symbol::core::IdentDeclId;
use crate::core::string_interner::{Interner, StrId};

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct MangledIdentifierName<T> {
    pub jarvil_identifer_name: StrId,
    pub unique_id: Option<IdentDeclId<T>>,
}

impl<T> MangledIdentifierName<T> {
    pub fn to_string(&self, suffix: &str, interner: &Interner) -> String {
        let Some(id) = self.unique_id else {
            return interner.lookup(self.jarvil_identifer_name).to_string();
        };
        format!(
            "{}_{}_{}",
            interner.lookup(self.jarvil_identifer_name),
            id.index(),
            suffix
        )
    }
}
