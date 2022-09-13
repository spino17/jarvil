use super::ast::{
    CoreNameTypeSpecsNode, CoreParamsNode, ExpressionNode, NameTypeSpecNode, NameTypeSpecsNode,
    ParamsNode,
};

pub struct NameTypeSpecsIterator {
    node: Option<NameTypeSpecsNode>,
}

impl NameTypeSpecsIterator {
    pub fn new(node: &NameTypeSpecsNode) -> Self {
        NameTypeSpecsIterator {
            node: Some(node.clone()),
        }
    }
}

impl Iterator for NameTypeSpecsIterator {
    type Item = NameTypeSpecNode;
    fn next(&mut self) -> Option<Self::Item> {
        let ok_name_type_specs = match &self.node {
            Some(node) => match node.core_ref() {
                CoreNameTypeSpecsNode::OK(ok_name_type_specs) => ok_name_type_specs.clone(),
                _ => return None,
            },
            None => return None,
        };
        self.node = match &ok_name_type_specs.core_ref().remaining_args {
            Some(remaining_args) => Some(remaining_args.clone()),
            None => None,
        };
        Some(ok_name_type_specs.core_ref().arg.clone())
    }
}

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
            Some(node) => match node.core_ref() {
                CoreParamsNode::OK(ok_params) => ok_params.clone(),
                _ => return None,
            },
            None => return None,
        };
        self.node = match &ok_params.core_ref().remaining_params {
            Some(remaining_params) => Some(remaining_params.clone()),
            None => None,
        };
        Some(ok_params.core_ref().param.clone())
    }
}
