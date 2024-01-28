use super::ast::SymbolSeparatedSequenceNode;
use crate::ast::ast::Node;

pub struct SymbolSeparatedSequenceIterator<'a, T: Node> {
    node: Option<&'a SymbolSeparatedSequenceNode<T>>,
}

impl<'a, T: Node> SymbolSeparatedSequenceIterator<'a, T> {
    pub fn new(node: &'a SymbolSeparatedSequenceNode<T>) -> Self {
        SymbolSeparatedSequenceIterator { node: Some(node) }
    }
}

impl<'a, T: Node> Iterator for SymbolSeparatedSequenceIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let ok_entity = match self.node {
            Some(node) => node,
            None => return None,
        };
        self.node = match &ok_entity.core_ref().remaining_entities {
            Some((_, remaining_params)) => Some(remaining_params),
            None => None,
        };
        Some(&ok_entity.core_ref().entity)
    }
}
