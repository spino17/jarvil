use super::ast::{
    CommaSeparatedNode, ExpressionNode, NameTypeSpecNode, ParamsNode, TypeExpressionNode,
};

pub struct ParamsIterator {
    node: Option<ParamsNode>,
}

impl ParamsIterator {
    pub fn new(node: &ParamsNode) -> Self {
        ParamsIterator {
            node: Some(node.clone()),
        }
    }
}

impl Iterator for ParamsIterator {
    type Item = ExpressionNode;
    fn next(&mut self) -> Option<Self::Item> {
        let ok_params = match &self.node {
            Some(node) => node.clone(),
            None => return None,
        };
        self.node = match &ok_params.core_ref().remaining_params {
            Some(remaining_params) => Some(remaining_params.clone()),
            None => None,
        };
        Some(ok_params.core_ref().param.clone())
    }
}

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
