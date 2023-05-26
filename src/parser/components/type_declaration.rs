use crate::ast::ast::{BlockKind, ErrornousNode, TokenNode, TypeExpressionNode, TypeTupleNode};
use crate::ast::ast::{LambdaTypeDeclarationNode, TypeDeclarationNode};
use crate::constants::common::DEF;
use crate::lexer::token::CoreToken;
use crate::{constants::common::IDENTIFIER, parser::parser::JarvilParser};
use std::rc::Rc;

pub fn type_decl(parser: &mut JarvilParser) -> TypeDeclarationNode {
    let type_keyword_node = parser.expect("type");
    let type_name_node = parser.expect_ident();
    let token = &parser.curr_token();
    let type_decl_node = match token.core_token {
        CoreToken::STRUCT_KEYWORD => {
            let struct_keyword_node = parser.expect("struct");
            let colon_node = parser.expect(":");
            let block_node = parser.block(
                |token| match token.core_token {
                    CoreToken::IDENTIFIER => true,
                    CoreToken::DEF => true,
                    _ => false,
                },
                |parser| parser.struct_stmt(),
                &[IDENTIFIER, DEF],
            );
            TypeDeclarationNode::new_with_struct(
                &type_name_node,
                &block_node,
                &type_keyword_node,
                &struct_keyword_node,
                &colon_node,
            )
        }
        CoreToken::LAMBDA_KEYWORD => {
            let mut type_tuple_node: Option<&TypeTupleNode> = None;
            let mut r_arrow_node: Option<&TokenNode> = None;
            let mut return_type_node: Option<&TypeExpressionNode> = None;
            let temp_type_tuple_node: TypeTupleNode;
            let temp_r_arrow_node: TokenNode;
            let temp_return_type_node: TypeExpressionNode;

            let lambda_keyword_node = parser.expect("lambda");
            let equal_node = parser.expect("=");
            let lparen_node = parser.expect("(");
            if !parser.check_curr_token(")") {
                temp_type_tuple_node = parser.type_tuple();
                type_tuple_node = Some(&temp_type_tuple_node);
            }
            let rparen_node = parser.expect(")");
            if parser.check_curr_token("->") {
                temp_r_arrow_node = parser.expect("->");
                temp_return_type_node = parser.type_expr();
                r_arrow_node = Some(&temp_r_arrow_node);
                return_type_node = Some(&temp_return_type_node);
            }
            let token = &parser.curr_token();
            let lambda_node = match token.core_token {
                CoreToken::NEWLINE | CoreToken::ENDMARKER => {
                    let newline_node = parser.expect_terminators();
                    LambdaTypeDeclarationNode::new(
                        &type_name_node,
                        &type_keyword_node,
                        &lambda_keyword_node,
                        &equal_node,
                        &lparen_node,
                        &rparen_node,
                        type_tuple_node,
                        r_arrow_node,
                        return_type_node,
                        &newline_node,
                    )
                }
                _ => {
                    parser.log_missing_token_error(&["\n"], token);
                    let lambda_node = LambdaTypeDeclarationNode::new_with_missing_tokens(
                        &Rc::new(["\n"].to_vec()),
                        token,
                    );
                    return TypeDeclarationNode::new_with_lambda(&lambda_node);
                }
            };
            TypeDeclarationNode::new_with_lambda(&lambda_node)
        }
        _ => {
            parser.log_missing_token_error(&["struct", "lambda"], token);
            return TypeDeclarationNode::new_with_missing_tokens(
                &Rc::new(["struct", "lambda"].to_vec()),
                token,
            );
        }
    };
    type_decl_node
}
