//! `anyon` -- Jarvil's build tool and package manager.
//!
//! Wraps the [`jarvil_parser`] and [`jarvil_py`] crates with the file handling a command line needs:
//! locating `main.jv`, writing the transpiled Python next to it, and running
//! that Python through the system interpreter.
//!
//! # Commands
//!
//! | Command | Effect |
//! |---------|--------|
//! | `anyon new <name>` | scaffolds a project directory containing a `main.jv` |
//! | `anyon build` | compiles `main.jv` to Python beside it |
//! | `anyon run` | builds, then executes the generated Python |
//!
//! Compilation failures are written to stderr and exit non-zero, so the tool
//! composes with shell scripts and CI.

pub mod build;
pub mod error;
pub mod helper;
pub mod new;
