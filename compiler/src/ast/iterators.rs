use super::ast::SymbolSeparatedSequenceNode;

pub struct SymbolSeparatedSequenceIterator<'a, T: Clone> {
    node: Option<&'a SymbolSeparatedSequenceNode<T>>,
}

impl<'a, T: Clone> SymbolSeparatedSequenceIterator<'a, T> {
    pub fn new(node: &'a SymbolSeparatedSequenceNode<T>) -> Self {
        SymbolSeparatedSequenceIterator {
            node: Some(node),
        }
    }
}

impl<'a, T: Clone> Iterator for SymbolSeparatedSequenceIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let ok_entity = match &self.node {
            Some(node) => node.clone(),
            None => return None,
        };
        self.node = match &ok_entity.core_ref().remaining_entities {
            Some((_, remaining_params)) => Some(remaining_params),
            None => None,
        };
        Some(&ok_entity.core_ref().entity)
    }
}
