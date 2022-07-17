use crate::ast::ast::ParamsNode;
use crate::parser::parser::PackratParser;
use crate::lexer::token::CoreToken;

pub fn params(parser: &mut PackratParser) -> ParamsNode {
    let first_param_node = parser.expr();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let remaining_params_node = parser.params();
            return ParamsNode::new_with_params(&first_param_node, &remaining_params_node)
        },
        _ => {
            return ParamsNode::new_with_single_param(&first_param_node)
        }
    }
}