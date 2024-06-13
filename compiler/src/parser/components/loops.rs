use crate::{
    ast::ast::{ForLoopStatementNode, WhileLoopStatementNode},
    parser::{
        components::statement::{
            is_statement_within_control_flow_starting_with,
            STATEMENT_WITHIN_CONTROL_FLOW_STARTING_SYMBOLS,
        },
        parser::JarvilParser,
        resolver::BlockKind,
    },
};

pub fn while_loop_stmt(parser: &mut JarvilParser) -> WhileLoopStatementNode {
    let while_keyword_node = parser.expect("while");
    let condition_expr_node = parser.expr();
    let colon_node = parser.expect(":");

    let block_node = parser.block(
        is_statement_within_control_flow_starting_with,
        |parser| parser.stmt(),
        &STATEMENT_WITHIN_CONTROL_FLOW_STARTING_SYMBOLS,
        BlockKind::Loop,
    );

    WhileLoopStatementNode::new(
        while_keyword_node,
        condition_expr_node,
        colon_node,
        block_node,
    )
}

pub fn for_loop_stmt(parser: &mut JarvilParser) -> ForLoopStatementNode {
    let for_keyword_node = parser.expect("for");
    let loop_variable_node = parser.expect_identifier();
    let in_keyword_node = parser.expect("in");
    let iterable_expr_node = parser.expr();
    let colon_node = parser.expect(":");

    let block_node = parser.block(
        is_statement_within_control_flow_starting_with,
        |parser| parser.stmt(),
        &STATEMENT_WITHIN_CONTROL_FLOW_STARTING_SYMBOLS,
        BlockKind::Loop,
    );

    ForLoopStatementNode::new(
        for_keyword_node,
        loop_variable_node,
        in_keyword_node,
        iterable_expr_node,
        colon_node,
        block_node,
    )
}
