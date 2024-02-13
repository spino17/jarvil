use crate::core::string_interner::{Interner, StrId};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct MangledIdentifierName {
    pub jarvil_identifer_name: StrId,
    pub unique_id: Option<usize>,
}

impl MangledIdentifierName {
    pub fn to_string(&self, suffix: &str, interner: &Interner) -> String {
        let Some(id) = self.unique_id else {
            return interner.lookup(self.jarvil_identifer_name).to_string();
        };
        format!(
            "{}_{}_{}",
            interner.lookup(self.jarvil_identifer_name),
            id,
            suffix
        )
    }
}
