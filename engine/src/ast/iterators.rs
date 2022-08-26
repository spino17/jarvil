use super::ast::{NameTypeSpecsNode, NameTypeSpecNode, CoreNameTypeSpecsNode};

pub struct NameTypeSpecsIterator {
    node: Option<NameTypeSpecsNode>,
}
impl NameTypeSpecsIterator {
    pub fn new(node: &NameTypeSpecsNode) -> Self {
        NameTypeSpecsIterator {
            node: Some(node.clone())
        }
    }
}
impl Iterator for NameTypeSpecsIterator {
    type Item = NameTypeSpecNode;
    fn next(&mut self) -> Option<Self::Item> {
        let ok_name_type_specs = match &self.node {
            Some(node) => {
                match node.core_ref() {
                    CoreNameTypeSpecsNode::OK(ok_name_type_specs) => {
                        ok_name_type_specs.clone()
                    },
                    _ => return None
                }
            },
            None => return None
        };
        self.node = match &ok_name_type_specs.core_ref().remaining_args {
            Some(remaining_args) => Some(remaining_args.clone()),
            None => None,
        };
        Some(ok_name_type_specs.core_ref().arg.clone())
    }
}