use super::symbol::core::IdentDeclId;
use crate::core::string_interner::{Interner, StrId};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub struct MangledIdentifierName<T> {
    pub jarvil_identifer_name: StrId,
    pub unique_id: Option<IdentDeclId<T>>,
}

impl<T> PartialEq for MangledIdentifierName<T> {
    fn eq(&self, other: &Self) -> bool {
        self.jarvil_identifer_name == other.jarvil_identifer_name
            && self.unique_id == other.unique_id
    }
}

impl<T> Eq for MangledIdentifierName<T> {}

impl<T> Hash for MangledIdentifierName<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.jarvil_identifer_name.hash(state);
        self.unique_id.hash(state);
    }
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
