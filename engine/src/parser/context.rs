use std::rc::Rc;
use crate::lexer::token::Token;
use crate::parser::components::expression::type_expression::is_type_expr_starting_with;

pub struct ParserContext(Rc<CoreParserContext>);
pub struct CoreParserContext {
    kind: ParserContextKind,
    parent_context: Option<ParserContext>,
}
impl CoreParserContext {
    pub fn new(kind: ParserContextKind) -> Self {
        CoreParserContext{
            kind,
            parent_context: None,
        }
    }

    pub fn new_with_parent_context(kind: ParserContextKind, parent_context: &ParserContext) -> Self {
        CoreParserContext{
            kind,
            parent_context: Some(ParserContext(parent_context.0.clone())),
        }
    }
}

pub enum ParserContextKind {
    BLOCK,
    STATEMENT,
    TYPE_EXPRESSION,
}

pub fn is_valid_starting_token_for_context(context: &ParserContext, token: &Token) -> bool {
    match context.0.as_ref().kind {
        ParserContextKind::BLOCK => token.is_eq("\n"),
        ParserContextKind::STATEMENT => todo!(),
        ParserContextKind::TYPE_EXPRESSION => is_type_expr_starting_with(token),
    }
}

