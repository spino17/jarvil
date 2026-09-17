//! Unit tests that need access to private internals.
//!
//! Most testing is done from `compiler/tests/` against the public API; what
//! lives here is what cannot be reached from outside the crate.

pub mod lexer;
