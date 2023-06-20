use super::ast::SymbolSeparatedSequenceNode;

pub struct SymbolSeparatedSequenceIterator<T: Clone> {
    node: Option<SymbolSeparatedSequenceNode<T>>,
}

impl<T: Clone> SymbolSeparatedSequenceIterator<T> {
    pub fn new(node: &SymbolSeparatedSequenceNode<T>) -> Self {
        SymbolSeparatedSequenceIterator {
            node: Some(node.clone()),
        }
    }
}

impl<T: Clone> Iterator for SymbolSeparatedSequenceIterator<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let ok_entity = match &self.node {
            Some(node) => node.clone(),
            None => return None,
        };
        self.node = match &ok_entity.core_ref().remaining_entities {
            Some(remaining_params) => Some(remaining_params.clone()),
            None => None,
        };
        Some(ok_entity.core_ref().entity.clone())
    }
}
