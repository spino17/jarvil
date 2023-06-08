use super::ast::{CommaSeparatedNode, ExpressionNode};

pub struct CommanSeparedIterator<T: Clone> {
    node: Option<CommaSeparatedNode<T>>,
}

impl<T: Clone> CommanSeparedIterator<T> {
    pub fn new(node: &CommaSeparatedNode<T>) -> Self {
        CommanSeparedIterator {
            node: Some(node.clone()),
        }
    }
}

impl<T: Clone> Iterator for CommanSeparedIterator<T> {
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
