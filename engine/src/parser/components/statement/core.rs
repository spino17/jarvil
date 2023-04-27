use crate::ast::ast::{CallableKind, FunctionDeclarationNode, StatementNode, StructStatementNode};
use crate::constants::common::IDENTIFIER;
use crate::lexer::token::{CoreToken, Token};
use crate::parser::components::expression::core::is_expression_starting_with;
use crate::parser::parser::PackratParser;

pub fn is_statement_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::LET => true,
        CoreToken::DEF => true,
        CoreToken::FOR => true,
        CoreToken::WHILE => true,
        CoreToken::IF => true,
        CoreToken::TYPE_KEYWORD => true,
        CoreToken::INTERFACE_KEYWORD => true,
        CoreToken::IMPL => true,
        CoreToken::IDENTIFIER => true,
        _ => is_expression_starting_with(token),
    }
}

pub const STATEMENT_EXPECTED_STARTING_SYMBOLS: [&'static str; 10] = [
    "let",
    "def",
    "for",
    "while",
    "if",
    "type",
    "interface",
    "impl",
    IDENTIFIER,
    "<expression>",
];

pub fn stmt(parser: &mut PackratParser) -> StatementNode {
    let token = &parser.curr_token();
    let statement_node = match token.core_token {
        CoreToken::LET => {
            let variable_decl_node = parser.variable_decl();
            StatementNode::new_with_variable_declaration(&variable_decl_node)
        }
        CoreToken::DEF => {
            let def_keyword = parser.expect("def");
            let func_name = parser.expect_ident();
            let callable_body = parser.callable_body();
            let func_decl = FunctionDeclarationNode::new(
                &func_name,
                &def_keyword,
                CallableKind::FUNC,
                &callable_body,
            );
            StatementNode::new_with_function_declaration(&func_decl)
        }
        CoreToken::FOR => todo!(),
        CoreToken::WHILE => todo!(),
        CoreToken::IF => todo!(),
        CoreToken::TYPE_KEYWORD => {
            let type_decl_node = parser.type_decl();
            StatementNode::new_with_type_declaration(&type_decl_node)
        }
        CoreToken::INTERFACE_KEYWORD => todo!(),
        CoreToken::IMPL => todo!(),
        CoreToken::RETURN => {
            let return_node = parser.expect("return");
            let token = &parser.curr_token();
            match token.core_token {
                CoreToken::NEWLINE | CoreToken::ENDMARKER => {
                    let newline = parser.expect_terminators();
                    StatementNode::new_with_return_statement(&return_node, None, &newline)
                }
                _ => {
                    let expr_node = parser.expr();
                    let newline = parser.expect_terminators();
                    StatementNode::new_with_return_statement(
                        &return_node,
                        Some(&expr_node),
                        &newline,
                    )
                }
            }
        }
        CoreToken::BREAK => todo!(),
        CoreToken::CONTINUE => todo!(),
        _ => {
            let expr_node = parser.expr();
            let token = &parser.curr_token();
            match token.core_token {
                CoreToken::EQUAL => {
                    let assignment_node = parser.assignment(&expr_node);
                    StatementNode::new_with_assignment(&assignment_node)
                }
                _ => {
                    let newline_node = parser.expect_terminators();
                    StatementNode::new_with_expression(&expr_node, &newline_node)
                }
            }
        }
    };
    statement_node
}

pub const STATEMENT_WITHIN_FUNCTION_EXPECTED_STARTING_SYMBOLS: [&'static str; 11] = [
    "let",
    "def",
    "for",
    "while",
    "if",
    "type",
    "interface",
    "impl",
    IDENTIFIER,
    "<expression>",
    "return",
];

pub fn is_statement_within_function_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::RETURN => true,
        _ => is_statement_starting_with(token),
    }
}

pub fn struct_stmt(parser: &mut PackratParser) -> StatementNode {
    let name_type_spec = parser.name_type_spec();
    let newline_node = parser.expect_terminators();
    let struct_stmt = StructStatementNode::new(&name_type_spec, &newline_node);
    StatementNode::new_with_struct_stmt(&struct_stmt)
}
