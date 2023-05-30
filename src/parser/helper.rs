use crate::ast::ast::SkippedTokenNode;
use crate::constants::common::NEWLINE;

pub enum IndentResultKind {
    CorrectIndentation,
    IncorrectIndentation((i64, i64)),
    BlockOver,
}

pub struct IndentResult {
    pub kind: IndentResultKind,
    pub skipped_tokens: Vec<SkippedTokenNode>,
    pub extra_newlines: Vec<SkippedTokenNode>,
}

pub fn format_symbol(symbol: &str) -> &str {
    if symbol == "\n" {
        return NEWLINE;
    }
    return symbol;
}
