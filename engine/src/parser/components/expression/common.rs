use crate::ast::ast::ParamsNode;
use crate::parser::parser::PackratParser;
use crate::lexer::token::CoreToken;

pub fn params(parser: &mut PackratParser) -> ParamsNode {
    let first_param_node = parser.expr();
    println!("{:?}", first_param_node);
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",", false);
            let remaining_params_node = parser.params();
            return ParamsNode::new_with_params(&first_param_node, &remaining_params_node)
        },
        _ => {
            return ParamsNode::new_with_single_param(&first_param_node)
        }
    }
}

pub fn params_within_parenthesis(parser: &mut PackratParser) -> Option<ParamsNode> {
    let lparen_node = parser.expect("(", false);
    let mut params: Option<ParamsNode> = None;
    if !parser.check_curr_token(")") {
        params = Some(parser.params());
    }
    let rparen_node = parser.expect(")", false);
    params
}