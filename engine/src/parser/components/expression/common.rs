use crate::ast::ast::{ParamsNode, OkParamsNode};
use crate::parser::parser::PackratParser;
use crate::lexer::token::CoreToken;
use std::rc::Rc;
use crate::ast::ast::ErrornousNode;

pub fn params(parser: &mut PackratParser) -> ParamsNode {
    let first_param_node = parser.expr();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA    => {
            let comma_node = parser.expect(",", false);
            let remaining_params_node = parser.params();
            let ok_params_node = OkParamsNode::new_with_params(&first_param_node, &remaining_params_node);
            return ParamsNode::new(&ok_params_node)
        },
        CoreToken::RPAREN   => {
            let ok_params_node = OkParamsNode::new_with_single_param(&first_param_node);
            return ParamsNode::new(&ok_params_node)
        },
        _                   => {
            parser.log_missing_token_error_for_multiple_expected_symbols(
                &[",", ")"], token
            );
            return ParamsNode::new_with_missing_tokens(
                &Rc::new([",", ")"].to_vec()), 
                token,
                parser.curr_lookahead(),
            )
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