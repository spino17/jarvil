use super::statement::core::{
    is_statement_within_function_starting_with, STATEMENT_WITHIN_FUNCTION_STARTING_SYMBOLS,
};
use crate::ast::ast::{
    BoundedMethodWrapperNode, CallableBodyNode, CallableKind, CallablePrototypeNode,
    FunctionDeclarationNode, FunctionWrapperNode, NameTypeSpecNode, StatementNode,
    SymbolSeparatedSequenceNode, TypeExpressionNode,
};
use crate::lexer::token::CoreToken;
use crate::parser::parser::JarvilParser;
use crate::parser::resolver::BlockKind;

pub fn name_type_spec(parser: &mut JarvilParser) -> NameTypeSpecNode {
    let name_node = parser.expect_identifier();
    let colon_node = parser.expect(":");
    let type_expr_node = parser.type_expr();
    NameTypeSpecNode::new(name_node, type_expr_node, colon_node)
}

pub fn name_type_specs(parser: &mut JarvilParser) -> SymbolSeparatedSequenceNode<NameTypeSpecNode> {
    let first_arg_node = parser.name_type_spec();
    let token = parser.curr_token();
    match token.core_token() {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let remaining_args_node = parser.name_type_specs();
            SymbolSeparatedSequenceNode::new_with_entities(
                first_arg_node,
                remaining_args_node,
                comma_node,
            )
        }
        _ => SymbolSeparatedSequenceNode::new_with_single_entity(first_arg_node),
    }
}

pub fn type_tuple(
    parser: &mut JarvilParser,
) -> (SymbolSeparatedSequenceNode<TypeExpressionNode>, usize) {
    let first_type_node = parser.type_expr();
    let token = parser.curr_token();
    // TODO - check that atleast two types are parsed inside tuple type expression
    match token.core_token() {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let (remaining_types_node, num_types) = parser.type_tuple();
            (
                SymbolSeparatedSequenceNode::new_with_entities(
                    first_type_node,
                    remaining_types_node,
                    comma_node,
                ),
                num_types + 1,
            )
        }
        _ => (
            SymbolSeparatedSequenceNode::new_with_single_entity(first_type_node),
            1,
        ),
    }
}

pub fn callable_prototype(parser: &mut JarvilParser) -> CallablePrototypeNode {
    let mut args_node: Option<SymbolSeparatedSequenceNode<NameTypeSpecNode>> = None;
    let lparen_node = parser.expect("(");
    if !parser.check_curr_token(")") {
        args_node = Some(parser.name_type_specs());
    }
    let rparen_node = parser.expect(")");
    if parser.check_curr_token("->") {
        let r_arrow_node = parser.expect("->");
        let return_type_node = parser.type_expr();
        CallablePrototypeNode::new(
            args_node,
            Some((r_arrow_node, return_type_node)),
            lparen_node,
            rparen_node,
        )
    } else {
        CallablePrototypeNode::new(args_node, None, lparen_node, rparen_node)
    }
}

pub fn callable_body(parser: &mut JarvilParser, block_kind: BlockKind) -> CallableBodyNode {
    let callable_prototype = parser.callable_prototype();
    let colon_node = parser.expect(":");
    let func_block_node = parser.block(
        is_statement_within_function_starting_with,
        |parser| parser.stmt(),
        &STATEMENT_WITHIN_FUNCTION_STARTING_SYMBOLS,
        block_kind,
    );
    CallableBodyNode::new(func_block_node, colon_node, callable_prototype)
}

pub fn function_stmt(parser: &mut JarvilParser, callable_kind: CallableKind) -> StatementNode {
    let def_keyword_node = parser.expect("def");
    let func_name_node = parser.expect_identifier_in_decl();
    let block_kind = match callable_kind {
        CallableKind::Function => BlockKind::Function,
        CallableKind::Method => BlockKind::Method,
    };
    let callable_body = parser.callable_body(block_kind);
    let func_decl_node =
        FunctionDeclarationNode::new(func_name_node, def_keyword_node, callable_body);
    match callable_kind {
        CallableKind::Function => {
            StatementNode::new_with_function_wrapper(FunctionWrapperNode::new(func_decl_node))
        }
        CallableKind::Method => StatementNode::new_with_bounded_method_wrapper(
            BoundedMethodWrapperNode::new(func_decl_node),
        ),
    }
}
