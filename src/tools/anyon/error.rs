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

pub enum AnyonError {
    Report(Report),
    Io(Error),
    Vanilla(VanillaError),
    UTF8(Utf8Error),
    Command(VanillaError),
}

impl AnyonError {
    pub fn new_with_report(report_err: Report) -> Self {
        AnyonError::Report(report_err)
    }

    pub fn new_with_io(io_err: Error) -> Self {
        AnyonError::Io(io_err)
    }

    pub fn new_with_utf8(utf8: Utf8Error) -> Self {
        AnyonError::UTF8(utf8)
    }

    pub fn new_with_vanilla(msg: String) -> Self {
        AnyonError::Vanilla(VanillaError { msg })
    }

    pub fn new_with_command(msg: String) -> Self {
        AnyonError::Command(VanillaError { msg })
    }
}

impl From<Report> for AnyonError {
    fn from(value: Report) -> Self {
        AnyonError::new_with_report(value)
    }
}

impl From<Error> for AnyonError {
    fn from(value: Error) -> Self {
        AnyonError::new_with_io(value)
    }
}

impl From<Utf8Error> for AnyonError {
    fn from(value: Utf8Error) -> Self {
        AnyonError::new_with_utf8(value)
    }
}

impl Debug for AnyonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            AnyonError::Report(report) => report.fmt(f),
            AnyonError::Vanilla(vanilla) => write!(f, "{}", vanilla.msg),
            AnyonError::Io(io) => write!(f, "{}", io.to_string()),
            AnyonError::UTF8(utf8) => write!(f, "{}", utf8.to_string()),
            AnyonError::Command(command) => {
                write!(f, "{}\nView all commands with `jarvil help`", command.msg)
            }
        }
    }
}
