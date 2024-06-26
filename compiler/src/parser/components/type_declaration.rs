use crate::ast::ast::{
    IdentifierInUseNode, SymbolSeparatedSequenceNode, TokenNode, TypeExpressionNode,
};
use crate::ast::ast::{LambdaTypeDeclarationNode, TypeDeclarationNode};
use crate::ast::traits::ErrornousNode;
use crate::constants::common::DEF;
use crate::lexer::token::CoreToken;
use crate::parser::resolver::BlockKind;
use crate::{constants::common::IDENTIFIER, parser::parser::JarvilParser};

pub fn ty_decl(parser: &mut JarvilParser) -> TypeDeclarationNode {
    let type_keyword_node = parser.expect("type");
    let ty_name_node = parser.expect_identifier_in_decl();
    let token = parser.curr_token();

    match token.core_token() {
        CoreToken::STRUCT_KEYWORD => {
            let mut implementing_interfaces_node: Option<(
                TokenNode,
                SymbolSeparatedSequenceNode<IdentifierInUseNode>,
            )> = None;
            let struct_keyword_node = parser.expect("struct");

            if parser.check_curr_token("implements") {
                let implements_keyword_node = parser.expect("implements");
                let interfaces_nodes = parser.expect_symbol_separated_sequence(
                    |parser: &mut JarvilParser| parser.expect_identifier_in_use(),
                    ",",
                );

                implementing_interfaces_node = Some((implements_keyword_node, interfaces_nodes));
            }

            let colon_node = parser.expect(":");
            let block_node = parser.block(
                |token| match token.core_token() {
                    CoreToken::IDENTIFIER => true,
                    CoreToken::DEF => true,
                    _ => false,
                },
                |parser| parser.struct_stmt(),
                &[IDENTIFIER, DEF],
                BlockKind::Struct,
            );

            TypeDeclarationNode::new_with_struct(
                ty_name_node,
                block_node,
                type_keyword_node,
                struct_keyword_node,
                implementing_interfaces_node,
                colon_node,
            )
        }
        CoreToken::ENUM_KEYWORD => {
            let enum_keyword_node = parser.expect("enum");
            let colon_node = parser.expect(":");
            let block_node = parser.block(
                |token| match token.core_token() {
                    CoreToken::IDENTIFIER => true,
                    _ => false,
                },
                |parser| parser.enum_stmt(),
                &[IDENTIFIER],
                BlockKind::Enum,
            );

            TypeDeclarationNode::new_with_enum(
                type_keyword_node,
                ty_name_node,
                enum_keyword_node,
                colon_node,
                block_node,
            )
        }
        CoreToken::LAMBDA_KEYWORD => {
            let mut ty_tuple_node: Option<SymbolSeparatedSequenceNode<TypeExpressionNode>> = None;
            let mut return_ty_node: Option<(TokenNode, TypeExpressionNode)> = None;
            let lambda_keyword_node = parser.expect("lambda");
            let equal_node = parser.expect("=");
            let lparen_node = parser.expect("(");

            if !parser.check_curr_token(")") {
                ty_tuple_node = Some(parser.ty_tuple().0);
            }

            let rparen_node = parser.expect(")");

            if parser.check_curr_token("->") {
                let r_arrow_node = parser.expect("->");
                return_ty_node = Some((r_arrow_node, parser.ty_expr()));
            }

            let newline_node = parser.expect_terminators();
            let lambda_node = LambdaTypeDeclarationNode::new(
                ty_name_node,
                type_keyword_node,
                lambda_keyword_node,
                equal_node,
                lparen_node,
                rparen_node,
                ty_tuple_node,
                return_ty_node,
                newline_node,
            );

            TypeDeclarationNode::new_with_lambda(lambda_node)
        }
        _ => {
            parser.log_missing_token_error(&["struct", "lambda"], token);

            TypeDeclarationNode::new_with_missing_tokens(
                ["struct", "lambda"].to_vec(),
                token.clone(),
            )
        }
    }
}
