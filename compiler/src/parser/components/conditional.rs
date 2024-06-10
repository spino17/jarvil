use crate::{
    ast::ast::{BlockNode, ConditionalBlockNode, ConditionalStatementNode, TokenNode},
    parser::{
        components::statement::{
            is_statement_within_control_flow_starting_with,
            STATEMENT_WITHIN_CONTROL_FLOW_STARTING_SYMBOLS,
        },
        parser::JarvilParser,
        resolver::BlockKind,
    },
};

pub fn conditional_block(
    parser: &mut JarvilParser,
    conditional_keyword_str: &'static str,
) -> ConditionalBlockNode {
    let conditional_keyword_node = parser.expect(conditional_keyword_str);
    let conditional_expr_node = parser.expr();
    let colon_node = parser.expect(":");
    let block_node = parser.block(
        is_statement_within_control_flow_starting_with,
        |parser| parser.stmt(),
        &STATEMENT_WITHIN_CONTROL_FLOW_STARTING_SYMBOLS,
        BlockKind::Conditional,
    );

    ConditionalBlockNode::new(
        conditional_keyword_node,
        conditional_expr_node,
        colon_node,
        block_node,
    )
}

pub fn conditional(parser: &mut JarvilParser) -> ConditionalStatementNode {
    let if_block_node = parser.conditional_block("if");
    let mut elifs: Vec<ConditionalBlockNode> = vec![];
    let mut else_block_node: Option<(TokenNode, TokenNode, BlockNode)> = None;

    while parser.check_curr_token("elif") {
        let elif_block_node = parser.conditional_block("elif");
        elifs.push(elif_block_node);
    }

    if parser.check_curr_token("else") {
        let else_keyword_node = parser.expect("else");
        let colon_node = parser.expect(":");
        let block_node = parser.block(
            is_statement_within_control_flow_starting_with,
            |parser| parser.stmt(),
            &STATEMENT_WITHIN_CONTROL_FLOW_STARTING_SYMBOLS,
            BlockKind::Conditional,
        );

        else_block_node = Some((else_keyword_node, colon_node, block_node));
    }

    ConditionalStatementNode::new(if_block_node, elifs, else_block_node)
}
