//! The parser, split by the construct each file parses.
//!
//! Every function here takes the parser and consumes one construct, so the
//! grammar can be read a piece at a time rather than as one large file.

pub mod assignment;
pub mod block;
pub mod code;
pub mod common;
pub mod conditional;
pub mod expression;
pub mod interface_declaration;
pub mod loops;
pub mod match_case;
pub mod statement;
pub mod type_declaration;
pub mod variable_declaration;
