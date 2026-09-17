//! Traits every syntax node implements.
//!
//! [`Node`] gives a node its source range, which is what diagnostics,
//! go-to-definition and hover are all ultimately expressed in terms of.
//! [`ErrornousNode`] is how the parser builds a node that stands in for input it
//! could not parse, so recovery produces a tree rather than nothing.

use crate::lexer::token::Token;
use text_size::TextRange;

/// Locates a syntax node in the source.
///
/// Implemented for every node. A node stores no text of its own -- it computes
/// its range from its children -- so this is the only route from the tree back
/// to a position, and therefore what diagnostics, go-to-definition and hover
/// are all ultimately built on.
pub trait Node {
    /// The byte range this node covers, spanning all of its children.
    fn range(&self) -> TextRange;

    /// The 1-based line the node starts on.
    fn start_line_number(&self) -> usize;
}

/// Builds a placeholder node standing in for input that failed to parse.
///
/// This is what makes error recovery possible: rather than abandoning the
/// parse, the parser records a diagnostic and substitutes one of these, so a
/// malformed file still yields a tree the later passes can walk. An editor
/// depends on it, since a file being typed into is almost never valid.
pub trait ErrornousNode {
    /// Creates a node recording what was expected and what was actually found.
    fn new_with_missing_tokens(expected_symbols: Vec<&'static str>, received_token: Token) -> Self;
}
