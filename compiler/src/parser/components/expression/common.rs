use crate::ast::ast::{CommaSeparatedNode, ExpressionNode};
use crate::lexer::token::CoreToken;
use crate::parser::parser::JarvilParser;

pub fn params(parser: &mut JarvilParser) -> CommaSeparatedNode<ExpressionNode> {
    let first_param_node = parser.expr();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let remaining_params_node = parser.params();
            CommaSeparatedNode::new_with_entities(
                &first_param_node,
                &remaining_params_node,
                &comma_node,
            )
        }
        _ => CommaSeparatedNode::new_with_single_entity(&first_param_node),
    }
}
