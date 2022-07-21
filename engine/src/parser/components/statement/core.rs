use crate::ast::ast::{StatementNode, StructStatementNode};
use crate::constants::common::{IDENTIFIER, ENDMARKER};
use crate::parser::parser::{PackratParser};
use crate::lexer::token::{Token,CoreToken};
use crate::parser::components::expression::core::is_expression_starting_with;
use crate::ast::ast::ErrornousNode;
use crate::scope::function;
use std::rc::Rc;

pub fn is_statement_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::LET                  => true,
        CoreToken::DEF                  => true,
        CoreToken::FOR                  => true,
        CoreToken::WHILE                => true,
        CoreToken::IF                   => true,
        CoreToken::TYPE_KEYWORD         => true,
        CoreToken::INTERFACE_KEYWORD    => true,
        CoreToken::IMPL                 => true,
        CoreToken::IDENTIFIER(_)        => true,
        _                               => is_expression_starting_with(token),
    }
}

pub const STATEMENT_EXPECTED_STARTING_SYMBOLS: [&'static str; 10]
= ["let", "def", "for", "while", "if", "type", "interface", "impl", IDENTIFIER, "<expression>"];

pub fn stmt(parser: &mut PackratParser) -> StatementNode {
    let token = &parser.curr_token();
    if !is_statement_starting_with(token) {
        parser.log_missing_token_error_for_multiple_expected_symbols(
            &STATEMENT_EXPECTED_STARTING_SYMBOLS, token
        );
        return StatementNode::new_with_missing_tokens(
            &Rc::new(STATEMENT_EXPECTED_STARTING_SYMBOLS.to_vec()), 
            token,
            parser.curr_lookahead(),
        )
    }
    let statement_node = match token.core_token {
        CoreToken::LET                  => {
            let variable_decl_node = parser.variable_decl();
            StatementNode::new_with_variable_declaration(&variable_decl_node)
        },
        CoreToken::DEF                  => {
            let function_name = parser.function_name();
            let function_decl_node = parser.function_decl(Some(function_name));
            StatementNode::new_with_function_declaration(&function_decl_node)
        },
        CoreToken::FOR                  => todo!(),
        CoreToken::WHILE                => todo!(),
        CoreToken::IF                   => todo!(),
        CoreToken::TYPE_KEYWORD         => {
            let type_decl_node = parser.type_decl();
            StatementNode::new_with_type_declaration(&type_decl_node)
        },
        CoreToken::INTERFACE_KEYWORD    => todo!(),
        CoreToken::IMPL                 => todo!(),
        CoreToken::RETURN               => todo!(),
        CoreToken::BREAK                => todo!(),
        CoreToken::CONTINUE             => todo!(),
        _                               => {
            let expr_node = parser.expr();
            StatementNode::new_with_expression(&expr_node)
        }
    };
    statement_node
}

pub const STATEMENT_WITH_FUNCTION_EXPECTED_STARTING_SYMBOLS: [&'static str; 11]
= ["let", "def", "for", "while", "if", "type", "interface", "impl", IDENTIFIER, "<expression>", "return"];

pub fn is_statement_within_function_starting_with(token: &Token) -> bool {
    match token.core_token {
        CoreToken::RETURN => true,
        _                 => is_statement_starting_with(token),
    }
}

pub fn struct_stmt(parser: &mut PackratParser) -> StatementNode {
    let struct_name = parser.expect(IDENTIFIER, false);
    let colon_node = parser.expect(":", false);
    let type_expr_node = parser.type_expr();
    let newline_node = parser.expects(&["\n", ENDMARKER], false);
    let struct_stmt = StructStatementNode::new(&struct_name, &type_expr_node);
    StatementNode::new_with_struct_stmt(&struct_stmt)
}