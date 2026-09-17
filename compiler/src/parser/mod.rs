//! The three passes that turn tokens into a checked semantic model.
//!
//! [`parser`] builds the syntax tree, [`resolver`] binds names to symbols, and
//! [`type_checker`] infers and checks types. Resolution and type checking both
//! live here rather than under a separate `sema` module because both are
//! implemented as walks over the parser's output.

pub mod components;
pub mod errors;
pub mod helper;
pub mod parser;
pub mod resolver;
pub mod type_checker;
