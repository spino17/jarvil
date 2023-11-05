use crate::{
    ast::ast::WhileLoopStatementNode,
    parser::{
        components::statement::core::{
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
