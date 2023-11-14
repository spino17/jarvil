// Anyon is the official build system and package manager for the Jarvil programming language.
// It is a command-line tool that provides a set of commands and features to manage Jarvil projects, including building,
// testing, and managing dependencies.

use super::{
    build::BuildCommand, error::AnyonError, fmt::FmtCommand, help::HelpCommand, new::NewCommand,
    version::VersionCommand,
};

pub trait AbstractCommand {
    fn check_cmd(&mut self) -> Result<(), AnyonError>;
    fn execute_cmd(&self) -> Result<(), AnyonError>;
    fn help_str(&self) -> String;
}

#[derive(Debug)]
pub enum AnyonCommand {
    New(NewCommand),
    Build(BuildCommand),
    Fmt(FmtCommand),
    Version(VersionCommand),
    Help(HelpCommand),
}

impl AbstractCommand for AnyonCommand {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        impl_variants!(self, check_cmd)
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        impl_variants!(self, execute_cmd)
    }

    fn help_str(&self) -> String {
        impl_variants!(self, help_str)
    }
}
