//! The error type the CLI reports.
//!
//! Wraps the distinct ways a command can fail -- a compile error, a missing
//! file, a bad invocation -- so `main` has one thing to render.

use miette::Report;
use std::{
    fmt::{Debug, Result},
    io::Error,
    str::Utf8Error,
};

#[derive(Debug)]
pub struct VanillaError {
    msg: String,
}

pub enum CliError {
    Report(Report),
    Io(Error),
    Vanilla(VanillaError),
    UTF8(Utf8Error),
    Command(VanillaError),
}

impl CliError {
    pub fn new_with_report(report_err: Report) -> Self {
        CliError::Report(report_err)
    }

    pub fn new_with_io(io_err: Error) -> Self {
        CliError::Io(io_err)
    }

    pub fn new_with_utf8(utf8: Utf8Error) -> Self {
        CliError::UTF8(utf8)
    }

    pub fn new_with_vanilla(msg: String) -> Self {
        CliError::Vanilla(VanillaError { msg })
    }

    pub fn new_with_command(msg: String) -> Self {
        CliError::Command(VanillaError { msg })
    }
}

impl From<Report> for CliError {
    fn from(value: Report) -> Self {
        CliError::new_with_report(value)
    }
}

impl From<Error> for CliError {
    fn from(value: Error) -> Self {
        CliError::new_with_io(value)
    }
}

impl From<Utf8Error> for CliError {
    fn from(value: Utf8Error) -> Self {
        CliError::new_with_utf8(value)
    }
}

impl Debug for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            CliError::Report(report) => report.fmt(f),
            CliError::Vanilla(vanilla) => write!(f, "{}", vanilla.msg),
            CliError::Io(io) => write!(f, "{}", io),
            CliError::UTF8(utf8) => write!(f, "{}", utf8),
            CliError::Command(command) => {
                write!(f, "{}\nView all commands with `jarvil help`", command.msg)
            }
        }
    }
}
