use crate::ast::ast::{NameTypeSpecNode, OkFunctionDeclarationNode, OkNameTypeSpecsNode, FunctionDeclarationNode};
use crate::{parser::parser::PackratParser, constants::common::IDENTIFIER, ast::ast::NameTypeSpecsNode};
use crate::lexer::token::{CoreToken, Token};
use std::rc::Rc;
use crate::ast::ast::ErrornousNode;

use super::statement::core::is_statement_starting_with;

pub fn name_type_spec(parser: &mut PackratParser) -> NameTypeSpecNode {
    let name_node = parser.expect(IDENTIFIER, true);
    let colon_node = parser.expect(":", false);
    let type_expr_node = parser.type_expr();
    NameTypeSpecNode::new(&name_node, &type_expr_node)
}

pub fn name_type_specs(parser: &mut PackratParser) ->NameTypeSpecsNode {
    let first_arg_node = parser.name_type_spec();
    parser.ignore_newlines();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",", false);
            let remaining_args_node = parser.name_type_specs();
            let ok_name_type_specs_node = OkNameTypeSpecsNode::new_with_args(
                &first_arg_node, &remaining_args_node
            );
            return NameTypeSpecsNode::new(&ok_name_type_specs_node)
        },
        CoreToken::RPAREN => {
            let ok_name_type_specs_node = OkNameTypeSpecsNode::new_with_single_arg(&first_arg_node);
            return NameTypeSpecsNode::new(&ok_name_type_specs_node)
        }
        _ => {
            parser.log_missing_token_error_for_multiple_expected_symbols(
                &[",", ")"], token
            );
            return NameTypeSpecsNode::new_with_missing_tokens(
                &Rc::new([",", ")"].to_vec()), 
                token,
                parser.curr_lookahead(),
            )
        }
    }
}

pub fn name_type_specs_within_parenthesis(parser: &mut PackratParser) -> Option<NameTypeSpecsNode> {
    let lparen_node = parser.expect("(", false);
    let mut args: Option<NameTypeSpecsNode> = None;
    if !parser.check_curr_token(")") {
        args = Some(parser.name_type_specs());
    }
    let rparen_node = parser.expect(")", true);
    args
}

pub const STATEMENT_WITH_FUNCTION_EXPECTED_STARTING_SYMBOLS: [&'static str; 11]
= ["let", "def", "for", "while", "if", "type", "interface", "impl", IDENTIFIER, "expression", "return"];

pub fn is_statement_within_function_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::RETURN => true,
        _                 => is_statement_starting_with(token),
    }
}

pub fn function_decl(parser: &mut PackratParser) -> FunctionDeclarationNode {
    let def_node = parser.expect("def", false);
    let name_node = parser.expect(IDENTIFIER, false);
    let args_node = parser.name_type_specs_within_parenthesis();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::RIGHT_ARROW  => {
            let r_arrow_node = parser.expect("->", false);
            let return_type = parser.type_expr();
            let colon_node = parser.expect(":", false);
            let func_block_node = parser.block(
                |token| {is_statement_within_function_starting_with(token)}, 
                &STATEMENT_WITH_FUNCTION_EXPECTED_STARTING_SYMBOLS
            );
            return FunctionDeclarationNode::new(&name_node, &args_node, &Some(return_type), &func_block_node)
        },
        CoreToken::COLON        => {
            let colon_node = parser.expect(":", false);
            let func_block_node = parser.block(
                |token| {is_statement_within_function_starting_with(token)}, 
                &STATEMENT_WITH_FUNCTION_EXPECTED_STARTING_SYMBOLS
            );
            return FunctionDeclarationNode::new(&name_node, &args_node, &None, &func_block_node)
        },
        _                       => {
            parser.log_missing_token_error_for_multiple_expected_symbols(
                &[":", "->"], token
            );
            return FunctionDeclarationNode::new_with_missing_tokens(
                &Rc::new([":", "->"].to_vec()), 
                token,
                parser.curr_lookahead(),
            )
        }
    }
}