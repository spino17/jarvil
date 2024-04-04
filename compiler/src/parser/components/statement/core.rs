use crate::ast::ast::{
    BreakStatementNode, CallableKind, CaseBranchStatementNode, ContinueStatementNode,
    EnumVariantDeclarationNode, IdentifierInDeclNode, StatementNode, StructPropertyDeclarationNode,
    TokenNode, TypeExpressionNode,
};
use crate::lexer::token::{CoreToken, Token};
use crate::parser::components::expression::core::is_expression_starting_with;
use crate::parser::parser::JarvilParser;
use crate::parser::resolver::BlockKind;

pub const STATEMENT_AT_GLOBAL_SCOPE_STARTING_SYMBOLS: [&str; 4] =
    ["def", "type", "interface", "declare"];

pub const STATEMENT_WITHIN_FUNCTION_STARTING_SYMBOLS: [&str; 10] = [
    "let",
    "def",
    "for",
    "while",
    "if",
    "type",
    "interface",
    "<expression>",
    "return",
    "match",
];

pub const STATEMENT_WITHIN_CONTROL_FLOW_STARTING_SYMBOLS: [&str; 12] = [
    "let",
    "def",
    "for",
    "while",
    "if",
    "type",
    "interface",
    "<expression>",
    "return",
    "break",
    "continue",
    "match",
];

pub fn is_statement_at_global_scope_starting_with(token: &Token) -> bool {
    match token.core_token() {
        CoreToken::DEF => true,
        CoreToken::TYPE_KEYWORD => true,
        CoreToken::INTERFACE_KEYWORD => true,
        CoreToken::DECLARE_KEYWORD => true,
        _ => false,
    }
}

pub fn is_statement_within_function_starting_with(token: &Token) -> bool {
    match token.core_token() {
        CoreToken::LET => true,
        CoreToken::DEF => true,
        CoreToken::FOR => true,
        CoreToken::WHILE => true,
        CoreToken::IF => true,
        CoreToken::TYPE_KEYWORD => true,
        CoreToken::INTERFACE_KEYWORD => true,
        CoreToken::RETURN => true,
        CoreToken::MATCH => true,
        _ => is_expression_starting_with(token),
    }
}

pub fn is_statement_within_control_flow_starting_with(token: &Token) -> bool {
    match token.core_token() {
        CoreToken::LET => true,
        CoreToken::DEF => true,
        CoreToken::FOR => true,
        CoreToken::WHILE => true,
        CoreToken::IF => true,
        CoreToken::TYPE_KEYWORD => true,
        CoreToken::INTERFACE_KEYWORD => true,
        CoreToken::RETURN => true,
        CoreToken::BREAK => true,
        CoreToken::CONTINUE => true,
        CoreToken::MATCH => true,
        _ => is_expression_starting_with(token),
    }
}

// Below method parsers every possible statement in Jarvil.
// It is the responsiblity of the `is_starting_with_fn` passed into `block` parsing
// method to allow or disallow certain statements inside the block for example: in struct block
// no other statement is allowed except ones starting with `<identifier>` and `def`.
pub fn stmt(parser: &mut JarvilParser) -> StatementNode {
    let token = parser.curr_token();
    let statement_node = match token.core_token() {
        CoreToken::LET => {
            let variable_decl_node = parser.variable_decl();
            StatementNode::new_with_variable_declaration(variable_decl_node)
        }
        CoreToken::DEF => parser.function_stmt(CallableKind::Function),
        CoreToken::DECLARE_KEYWORD => {
            // TODO - parse declaration statement for function in `.d.jv` files
            todo!()
        }
        CoreToken::FOR => {
            let for_loop_node = parser.for_loop_stmt();
            StatementNode::new_with_for_loop(for_loop_node)
        }
        CoreToken::WHILE => {
            let while_loop_node = parser.while_loop_stmt();
            StatementNode::new_with_while_loop(while_loop_node)
        }
        CoreToken::MATCH => {
            let match_case_node = parser.match_case();
            StatementNode::new_with_match_case_statement(match_case_node)
        }
        CoreToken::IF => {
            let conditional_node = parser.conditional();
            StatementNode::new_with_conditional(conditional_node)
        }
        CoreToken::TYPE_KEYWORD => {
            let ty_decl_node = parser.ty_decl();
            StatementNode::new_with_ty_declaration(ty_decl_node)
        }
        CoreToken::INTERFACE_KEYWORD => {
            let interface_decl = parser.interface_decl();
            StatementNode::new_with_interface_declaration(interface_decl)
        }
        CoreToken::RETURN => {
            let return_node = parser.expect("return");
            let token = parser.curr_token();
            match token.core_token() {
                CoreToken::NEWLINE | CoreToken::ENDMARKER => {
                    let newline_node = parser.expect_terminators();
                    StatementNode::new_with_return_statement(return_node, None, newline_node)
                }
                _ => {
                    let expr_node = parser.expr();
                    let newline_node = parser.expect_terminators();
                    StatementNode::new_with_return_statement(
                        return_node,
                        Some(expr_node),
                        newline_node,
                    )
                }
            }
        }
        CoreToken::BREAK => {
            let break_keyword_node = parser.expect("break");
            let newline_node = parser.expect_terminators();
            StatementNode::new_with_break_statment(BreakStatementNode::new(
                break_keyword_node,
                newline_node,
            ))
        }
        CoreToken::CONTINUE => {
            let continue_keyword_node = parser.expect("continue");
            let newline_node = parser.expect_terminators();
            StatementNode::new_with_continue_statment(ContinueStatementNode::new(
                continue_keyword_node,
                newline_node,
            ))
        }
        _ => {
            let expr_node = parser.expr();
            let token = parser.curr_token();
            match token.core_token() {
                CoreToken::EQUAL => {
                    let assignment_node = parser.assignment(expr_node);
                    StatementNode::new_with_assignment(assignment_node)
                }
                _ => {
                    let newline_node = parser.expect_terminators();
                    StatementNode::new_with_expression(expr_node, newline_node)
                }
            }
        }
    };
    statement_node
}

pub fn struct_stmt(parser: &mut JarvilParser) -> StatementNode {
    let token = parser.curr_token();
    match token.core_token() {
        CoreToken::IDENTIFIER => {
            let name_ty_spec_node = parser.name_ty_spec();
            let newline_node = parser.expect_terminators();
            let struct_stmt = StructPropertyDeclarationNode::new(name_ty_spec_node, newline_node);
            StatementNode::new_with_struct_stmt(struct_stmt)
        }
        CoreToken::DEF => parser.function_stmt(CallableKind::Method),
        _ => unreachable!(),
    }
}

pub fn interface_stmt(parser: &mut JarvilParser) -> StatementNode {
    let token = parser.curr_token();
    match token.core_token() {
        CoreToken::IDENTIFIER => {
            let name_ty_spec_node = parser.name_ty_spec();
            let newline_node = parser.expect_terminators();
            let struct_stmt = StructPropertyDeclarationNode::new(name_ty_spec_node, newline_node);
            StatementNode::new_with_struct_stmt(struct_stmt)
        }
        CoreToken::DEF => {
            let interface_method_prototype_wrapper_node =
                parser.interface_method_prototype_wrapper();
            StatementNode::new_with_interface_method_prototype_wrapper(
                interface_method_prototype_wrapper_node,
            )
        }
        _ => unreachable!(),
    }
}

pub fn enum_stmt(parser: &mut JarvilParser) -> StatementNode {
    let mut optional_ty_node: Option<(TokenNode, TypeExpressionNode, TokenNode)> = None;
    let variant_name_node = parser.expect_identifier();
    if parser.curr_token().is_eq("(") {
        let lparen_node = parser.expect("(");
        let ty_node = parser.ty_expr();
        let rparen_node = parser.expect(")");
        optional_ty_node = Some((lparen_node, ty_node, rparen_node));
    }
    let newline_node = parser.expect_terminators();
    StatementNode::new_with_enum_stmt(EnumVariantDeclarationNode::new(
        variant_name_node,
        optional_ty_node,
        newline_node,
    ))
}

pub fn case_branch_stmt(parser: &mut JarvilParser) -> StatementNode {
    let mut optional_variable_name_node: Option<(TokenNode, IdentifierInDeclNode, TokenNode)> =
        None;
    let case_keyword_node = parser.expect("case");
    // TODO - here it can be `_` for handling default case
    let enum_name_node = parser.expect_identifier();
    let double_colon_node = parser.expect("::");
    let variant_name_node = parser.expect_identifier();
    if parser.curr_token().is_eq("(") {
        let lparen_node = parser.expect("(");
        let variable_name_node = parser.expect_identifier();
        let rparen_node = parser.expect(")");
        optional_variable_name_node = Some((lparen_node, variable_name_node, rparen_node));
    }
    let colon_node = parser.expect(":");
    let block_node = parser.block(
        is_statement_within_control_flow_starting_with,
        |parser| parser.stmt(),
        &STATEMENT_WITHIN_CONTROL_FLOW_STARTING_SYMBOLS,
        BlockKind::Case,
    );
    StatementNode::new_with_case_branch_statement(CaseBranchStatementNode::new(
        case_keyword_node,
        enum_name_node,
        double_colon_node,
        variant_name_node,
        optional_variable_name_node,
        colon_node,
        block_node,
    ))
}
