use crate::ast::ast::ParamsNode;
use crate::lexer::token::CoreToken;
use crate::parser::parser::JarvilParser;

pub fn params(parser: &mut JarvilParser) -> ParamsNode {
    let first_param_node = parser.expr();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let remaining_params_node = parser.params();
            ParamsNode::new_with_params(&first_param_node, &remaining_params_node, &comma_node)
        }
        _ => ParamsNode::new_with_single_param(&first_param_node),
    }
}
