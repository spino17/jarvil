use std::fmt::Display;
use std::fmt::{self};
use text_size::TextRange;

#[derive(Clone, Debug)]
pub enum IdentifierKind {
    Variable,
    Function,
    Type,
    Argument,
    Field,
    Method,
    Constructor,
    Interface,
}

impl Display for IdentifierKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IdentifierKind::Variable => write!(f, "variable"),
            IdentifierKind::Function => write!(f, "function"),
            IdentifierKind::Type => write!(f, "type"),
            IdentifierKind::Argument => write!(f, "argument"),
            IdentifierKind::Field => write!(f, "field"),
            IdentifierKind::Method => write!(f, "method"),
            IdentifierKind::Constructor => write!(f, "constructor"),
            IdentifierKind::Interface => write!(f, "interface"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PropertyKind {
    Field,
    Method,
}

impl Display for PropertyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PropertyKind::Field => write!(f, "field"),
            PropertyKind::Method => write!(f, "method"),
        }
    }
}

pub fn range_to_span(range: TextRange) -> (usize, usize) {
    let start_index = range.start();
    let end_index = range.end();
    let len = end_index - start_index;
    (start_index.into(), len.into())
}
