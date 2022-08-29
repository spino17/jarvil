use crate::ast::ast::{ErrornousNode, IdentifierNode};
use crate::ast::ast::{
    FuncKeywordKind, FunctionDeclarationNode, NameTypeSpecNode, OkNameTypeSpecsNode, TokenNode,
};
use crate::lexer::token::CoreToken;
use crate::parser::components::statement::core::{
    is_statement_within_function_starting_with, STATEMENT_WITH_FUNCTION_EXPECTED_STARTING_SYMBOLS,
};
use crate::{ast::ast::NameTypeSpecsNode, parser::parser::PackratParser};
use std::rc::Rc;

pub fn name_type_spec(parser: &mut PackratParser) -> NameTypeSpecNode {
    let name_node = parser.expect_ident();
    let colon_node = parser.expect(":");
    let type_expr_node = parser.type_expr();
    NameTypeSpecNode::new(&name_node, &type_expr_node, &colon_node)
}

pub fn name_type_specs(parser: &mut PackratParser) -> NameTypeSpecsNode {
    let first_arg_node = parser.name_type_spec();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let remaining_args_node = parser.name_type_specs();
            let ok_name_type_specs_node = OkNameTypeSpecsNode::new_with_args(
                &first_arg_node,
                &remaining_args_node,
                &comma_node,
            );
            return NameTypeSpecsNode::new(&ok_name_type_specs_node);
        }
        CoreToken::RPAREN => {
            let ok_name_type_specs_node = OkNameTypeSpecsNode::new_with_single_arg(&first_arg_node);
            return NameTypeSpecsNode::new(&ok_name_type_specs_node);
        }
        _ => {
            parser.log_missing_token_error_for_multiple_expected_symbols(&[",", ")"], token);
            return NameTypeSpecsNode::new_with_missing_tokens(
                &Rc::new([",", ")"].to_vec()),
                token,
            );
        }
    }
}

pub fn function_name(parser: &mut PackratParser) -> (IdentifierNode, TokenNode) {
    let def_keyword_node = parser.expect("def");
    let name_node = parser.expect_ident();
    (name_node, def_keyword_node)
}

pub fn function_decl(
    parser: &mut PackratParser,
    name_node: Option<&IdentifierNode>,
    func_keyword_node: &FuncKeywordKind,
    is_lambda: bool,
) -> FunctionDeclarationNode {
    let lparen_node = parser.expect("(");
    let mut args_node: Option<&NameTypeSpecsNode> = None;
    let name_type_specs_node: NameTypeSpecsNode;
    if !parser.check_curr_token(")") {
        name_type_specs_node = parser.name_type_specs();
        args_node = Some(&name_type_specs_node);
    }
    let rparen_node = parser.expect(")");
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::RIGHT_ARROW => {
            let r_arrow_node = parser.expect("->");
            let return_type_node = parser.type_expr();
            let colon_node = parser.expect(":");
            let func_block_node = parser.block(
                |token| is_statement_within_function_starting_with(token),
                |parser| parser.stmt(),
                &STATEMENT_WITH_FUNCTION_EXPECTED_STARTING_SYMBOLS,
            );
            return FunctionDeclarationNode::new(
                name_node,
                args_node,
                Some(&return_type_node),
                &func_block_node,
                func_keyword_node,
                &lparen_node,
                &rparen_node,
                Some(&r_arrow_node),
                &colon_node,
                is_lambda,
            );
        }
        CoreToken::COLON => {
            let colon_node = parser.expect(":");
            let func_block_node = parser.block(
                |token| is_statement_within_function_starting_with(token),
                |parser| parser.stmt(),
                &STATEMENT_WITH_FUNCTION_EXPECTED_STARTING_SYMBOLS,
            );
            return FunctionDeclarationNode::new(
                name_node,
                args_node,
                None,
                &func_block_node,
                func_keyword_node,
                &lparen_node,
                &rparen_node,
                None,
                &colon_node,
                is_lambda,
            );
        }
        _ => {
            parser.log_missing_token_error_for_multiple_expected_symbols(&[":", "->"], token);
            return FunctionDeclarationNode::new_with_missing_tokens(
                &Rc::new([":", "->"].to_vec()),
                token,
            );
        }
    }
}
