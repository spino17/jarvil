use super::expression::core::is_expression_starting_with;
use super::statement::core::{
    is_statement_within_function_starting_with, STATEMENT_WITHIN_FUNCTION_EXPECTED_STARTING_SYMBOLS,
};
use crate::ast::ast::{
    BlockKind, CallableBodyNode, CallablePrototypeNode, ErrornousNode, IdentifierNode,
    LambdaDeclarationNode, NameTypeSpecNode, NameTypeSpecsNode, OkNameTypeSpecsNode,
    RAssignmentNode, OkTypeTupleNode, TypeTupleNode,
};
use crate::constants::common::LAMBDA_KEYWORD;
use crate::lexer::token::{CoreToken, Token};
use crate::parser::parser::PackratParser;
use std::rc::Rc;

pub fn is_r_assign_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::LAMBDA_KEYWORD => true,
        _ => is_expression_starting_with(token),
    }
}

pub const R_ASSIGNMENT_STARTING_SYMBOLS: [&'static str; 2] = ["<expression>", "lambda"];

pub fn r_assign(
    parser: &mut PackratParser,
    identifier_name: Option<&IdentifierNode>,
) -> RAssignmentNode {
    let token = &parser.curr_token();
    if !is_r_assign_starting_with(token) {
        parser.log_missing_token_error(&R_ASSIGNMENT_STARTING_SYMBOLS, token);
        return RAssignmentNode::new_with_missing_tokens(
            &Rc::new(R_ASSIGNMENT_STARTING_SYMBOLS.to_vec()),
            token,
        );
    }
    match token.core_token {
        CoreToken::LAMBDA_KEYWORD => {
            let lambda_keyword = parser.expect(LAMBDA_KEYWORD);
            let callable_body = parser.callable_body();
            let lambda_decl_node =
                LambdaDeclarationNode::new(identifier_name, &lambda_keyword, &callable_body);
            RAssignmentNode::new_with_lambda(&lambda_decl_node)
        }
        _ => {
            let expr_node = parser.expr();
            let newline = parser.expect_terminators();
            RAssignmentNode::new_with_expr(&expr_node, &newline)
        }
    }
}

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
            parser.log_missing_token_error(&[",", ")"], token);
            return NameTypeSpecsNode::new_with_missing_tokens(
                &Rc::new([",", ")"].to_vec()),
                token,
            );
        }
    }
}

pub fn type_tuple(parser: &mut PackratParser) -> TypeTupleNode {
    let first_type_node = parser.type_expr();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let remaining_types = parser.type_tuple();
            let ok_type_tuple_node = OkTypeTupleNode::new_with_args(&first_type_node, &remaining_types, &comma_node);
            return TypeTupleNode::new(&ok_type_tuple_node);
        }
        CoreToken::RPAREN => {
            let ok_type_tuple_node = OkTypeTupleNode::new_with_single_data_type(&first_type_node);
            return TypeTupleNode::new(&ok_type_tuple_node);
        }
        _ => {
            parser.log_missing_token_error(&[",", ")"], token);
            return TypeTupleNode::new_with_missing_tokens(&Rc::new([",", ")"].to_vec()), token)
        }
    }
}

pub fn callable_prototype(parser: &mut PackratParser) -> CallablePrototypeNode {
    let mut args_node: Option<&NameTypeSpecsNode> = None;
    let name_type_specs_node: NameTypeSpecsNode;
    let lparen_node = parser.expect("(");
    if !parser.check_curr_token(")") {
        name_type_specs_node = parser.name_type_specs();
        args_node = Some(&name_type_specs_node);
    }
    let rparen_node = parser.expect(")");
    if parser.check_curr_token("->") {
        let r_arrow_node = parser.expect("->");
        let return_type_node = parser.type_expr();
        return CallablePrototypeNode::new(
            args_node,
            Some(&return_type_node),
            &lparen_node,
            &rparen_node,
            Some(&r_arrow_node),
        );
    } else {
        return CallablePrototypeNode::new(args_node, None, &lparen_node, &rparen_node, None);
    }
}

pub fn callable_body(parser: &mut PackratParser) -> CallableBodyNode {
    let callable_prototype = parser.callable_prototype();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COLON => {
            let colon_node = parser.expect(":");
            let func_block_node = parser.block(
                |token| is_statement_within_function_starting_with(token),
                |parser| parser.stmt(),
                &STATEMENT_WITHIN_FUNCTION_EXPECTED_STARTING_SYMBOLS,
                BlockKind::FUNC,
            );
            return CallableBodyNode::new(&func_block_node, &colon_node, &callable_prototype);
        }
        _ => {
            parser.log_missing_token_error(&[":"], token);
            return CallableBodyNode::new_with_missing_tokens(&Rc::new([":"].to_vec()), token);
        }
    }
}
