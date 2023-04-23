use crate::ast::ast::ErrornousNode;
use crate::ast::ast::{OkParamsNode, ParamsNode};
use crate::lexer::token::CoreToken;
use crate::parser::parser::PackratParser;
use std::rc::Rc;

pub fn params(parser: &mut PackratParser) -> ParamsNode {
    let first_param_node = parser.expr();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let remaining_params_node = parser.params();
            let ok_params_node = OkParamsNode::new_with_params(
                &first_param_node,
                &remaining_params_node,
                &comma_node,
            );
            return ParamsNode::new(&ok_params_node);
        }
        CoreToken::RPAREN => {
            let ok_params_node = OkParamsNode::new_with_single_param(&first_param_node);
            return ParamsNode::new(&ok_params_node);
        }
        _ => {
            parser.log_missing_token_error(&[",", ")"], token);
            return ParamsNode::new_with_missing_tokens(&Rc::new([",", ")"].to_vec()), token);
        }
    }
}
