use super::statement::core::{
    is_statement_within_function_starting_with, STATEMENT_WITHIN_FUNCTION_EXPECTED_STARTING_SYMBOLS,
};
use crate::ast::ast::{
    BlockKind, BoundedMethodWrapperNode, CallableBodyNode, CallableKind, CallablePrototypeNode,
    ErrornousNode, FunctionDeclarationNode, FunctionWrapperNode, NameTypeSpecNode,
    NameTypeSpecsNode, OkNameTypeSpecsNode, OkTypeTupleNode, StatementNode, TypeTupleNode,
};
use crate::lexer::token::CoreToken;
use crate::parser::parser::JarvilParser;
use std::rc::Rc;

pub fn name_type_spec(parser: &mut JarvilParser) -> NameTypeSpecNode {
    let name_node = parser.expect_ident();
    let colon_node = parser.expect(":");
    let type_expr_node = parser.type_expr();
    NameTypeSpecNode::new(&name_node, &type_expr_node, &colon_node)
}

pub fn name_type_specs(parser: &mut JarvilParser) -> NameTypeSpecsNode {
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

pub fn type_tuple(parser: &mut JarvilParser) -> TypeTupleNode {
    let first_type_node = parser.type_expr();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let remaining_types_node = parser.type_tuple();
            let ok_type_tuple_node = OkTypeTupleNode::new_with_args(
                &first_type_node,
                &remaining_types_node,
                &comma_node,
            );
            return TypeTupleNode::new(&ok_type_tuple_node);
        }
        CoreToken::RPAREN => {
            let ok_type_tuple_node = OkTypeTupleNode::new_with_single_data_type(&first_type_node);
            return TypeTupleNode::new(&ok_type_tuple_node);
        }
        _ => {
            parser.log_missing_token_error(&[",", ")"], token);
            return TypeTupleNode::new_with_missing_tokens(&Rc::new([",", ")"].to_vec()), token);
        }
    }
}

pub fn callable_prototype(parser: &mut JarvilParser) -> CallablePrototypeNode {
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

pub fn callable_body(parser: &mut JarvilParser) -> CallableBodyNode {
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

pub fn function_stmt(parser: &mut JarvilParser, callable_kind: CallableKind) -> StatementNode {
    let def_keyword_node = parser.expect("def");
    let func_name_node = parser.expect_ident();
    let callable_body = parser.callable_body();
    let func_decl_node =
        FunctionDeclarationNode::new(&func_name_node, &def_keyword_node, &callable_body);
    match callable_kind {
        CallableKind::FUNC => {
            return StatementNode::new_with_function_wrapper(&FunctionWrapperNode::new(
                &func_decl_node,
            ))
        }
        CallableKind::METHOD => {
            return StatementNode::new_with_bounded_method_wrapper(&BoundedMethodWrapperNode::new(
                &func_decl_node,
            ))
        }
    }
}
