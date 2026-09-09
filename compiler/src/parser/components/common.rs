use super::statement::{
    is_statement_within_func_starting_with, STATEMENT_WITHIN_FUNC_STARTING_SYMBOLS,
};
use crate::ast::ast::{
    BoundedMethodWrapperNode, CallableBodyNode, CallableKind, CallablePrototypeNode,
    DeclareCallablePrototypeNode, DeclareFunctionPrototypeNode, FunctionDeclarationNode,
    FunctionWrapperNode, NameTypeSpecNode, StatementNode, SymbolSeparatedSequenceNode,
    TypeExpressionNode,
};
use crate::lexer::token::CoreToken;
use crate::parser::parser::JarvilParser;
use crate::parser::resolver::BlockKind;

pub fn name_ty_spec(parser: &mut JarvilParser) -> NameTypeSpecNode {
    let name_node = parser.expect_identifier();
    let colon_node = parser.expect(":");
    let ty_expr_node = parser.ty_expr();

    NameTypeSpecNode::new(name_node, ty_expr_node, colon_node)
}

pub fn name_ty_specs(parser: &mut JarvilParser) -> SymbolSeparatedSequenceNode<NameTypeSpecNode> {
    let first_arg_node = parser.name_ty_spec();
    let token = parser.curr_token();

    match token.core_token() {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let remaining_args_node = parser.name_ty_specs();

            SymbolSeparatedSequenceNode::new_with_entities(
                first_arg_node,
                remaining_args_node,
                comma_node,
            )
        }
        _ => SymbolSeparatedSequenceNode::new_with_single_entity(first_arg_node),
    }
}

pub fn ty_tuple(
    parser: &mut JarvilParser,
) -> (SymbolSeparatedSequenceNode<TypeExpressionNode>, usize) {
    let first_ty_node = parser.ty_expr();
    let token = parser.curr_token();

    // TODO - check that atleast two types are parsed inside tuple type expression
    match token.core_token() {
        CoreToken::COMMA => {
            let comma_node = parser.expect(",");
            let (remaining_types_node, num_types) = parser.ty_tuple();

            (
                SymbolSeparatedSequenceNode::new_with_entities(
                    first_ty_node,
                    remaining_types_node,
                    comma_node,
                ),
                num_types + 1,
            )
        }
        _ => (
            SymbolSeparatedSequenceNode::new_with_single_entity(first_ty_node),
            1,
        ),
    }
}

pub fn callable_prototype(parser: &mut JarvilParser) -> CallablePrototypeNode {
    let mut args_node: Option<SymbolSeparatedSequenceNode<NameTypeSpecNode>> = None;

    let lparen_node = parser.expect("(");

    if !parser.check_curr_token(")") {
        args_node = Some(parser.name_ty_specs());
    }

    let rparen_node = parser.expect(")");

    if parser.check_curr_token("->") {
        let r_arrow_node = parser.expect("->");
        let return_ty_node = parser.ty_expr();

        CallablePrototypeNode::new(
            args_node,
            Some((r_arrow_node, return_ty_node)),
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
        is_statement_within_func_starting_with,
        |parser| parser.stmt(),
        &STATEMENT_WITHIN_FUNC_STARTING_SYMBOLS,
        block_kind,
    );

    CallableBodyNode::new(func_block_node, colon_node, callable_prototype)
}

pub fn func_stmt(parser: &mut JarvilParser, callable_kind: CallableKind) -> StatementNode {
    let block_kind = match callable_kind {
        CallableKind::Function => BlockKind::Function,
        CallableKind::Method => BlockKind::Method,
    };

    let def_keyword_node = parser.expect("def");
    let func_name_node = parser.expect_identifier_in_decl();
    let callable_body = parser.callable_body(block_kind);

    let func_decl_node =
        FunctionDeclarationNode::new(func_name_node, def_keyword_node, callable_body);

    match callable_kind {
        CallableKind::Function => {
            StatementNode::new_with_func_wrapper(FunctionWrapperNode::new(func_decl_node))
        }
        CallableKind::Method => StatementNode::new_with_bounded_method_wrapper(
            BoundedMethodWrapperNode::new(func_decl_node),
        ),
    }
}

pub fn decl_callable_prototype(parser: &mut JarvilParser) -> DeclareCallablePrototypeNode {
    let def_keyword_node = parser.expect("def");
    let func_name_node = parser.expect_identifier_in_decl(); // decl
    let prototype = parser.callable_prototype();
    let newline = parser.expect_terminators();

    DeclareCallablePrototypeNode::new(def_keyword_node, func_name_node, prototype, newline)
}

pub fn decl_func_prototype(parser: &mut JarvilParser) -> DeclareFunctionPrototypeNode {
    let declare_keyword_node = parser.expect("declare");
    let decl_node = parser.decl_callable_prototype();

    DeclareFunctionPrototypeNode::new(declare_keyword_node, decl_node)
}
