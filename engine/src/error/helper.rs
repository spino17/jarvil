use std::fmt::{self};
use std::fmt::{write, Display};
use text_size::TextRange;

#[derive(Clone, Debug)]
pub enum IdentifierKind {
    VARIABLE,
    FUNCTION,
    TYPE,
    ARGUMENT,
    FIELD,
}
impl Display for IdentifierKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IdentifierKind::VARIABLE => write!(f, "variable"),
            IdentifierKind::FUNCTION => write!(f, "function"),
            IdentifierKind::TYPE => write!(f, "type"),
            IdentifierKind::ARGUMENT => write!(f, "argument"),
            IdentifierKind::FIELD => write!(f, "field"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PropertyKind {
    FIELD,
    METHOD,
}
impl Display for PropertyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PropertyKind::FIELD => write!(f, "field"),
            PropertyKind::METHOD => write!(f, "method"),
        }
    }
}

pub fn range_to_span(range: TextRange) -> (usize, usize) {
    let start_index = range.start();
    let end_index = range.end();
    let len = end_index - start_index;
    (start_index.into(), len.into())
}
