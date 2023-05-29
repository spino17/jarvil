// Anyon is the official build system and package manager for the Jarvil programming language.
// It is a command-line tool that provides a set of commands and features to manage Jarvil projects, including building,
// testing, and managing dependencies.

use super::{
    build::{BuildDriver, BuildMode},
    error::AnyonError,
    fmt::FmtDriver,
    help::HelpDriver,
    new::NewDriver,
    version::VersionDriver,
};

pub trait AbstractCommand {
    fn check_cmd(&mut self) -> Result<(), AnyonError>;
    fn execute_cmd(&self) -> Result<(), AnyonError>;
    fn help_str(&self) -> String;
}

#[derive(Debug)]
pub enum AnyonCommand {
    New(NewDriver),
    Build(BuildDriver),
    Fmt(FmtDriver),
    Version(VersionDriver),
    Help(HelpDriver),
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

pub fn get_cmd_from_command_line_args(args: Vec<String>) -> Result<AnyonCommand, AnyonError> {
    if args.len() < 2 {
        return Ok(AnyonCommand::Help(HelpDriver::new(args)));
    } else {
        let core_cmd = &args[1];
        if core_cmd.eq("new") {
            return Ok(AnyonCommand::New(NewDriver::new(args)));
        } else if core_cmd.eq("build") {
            return Ok(AnyonCommand::Build(BuildDriver::new(
                args,
                BuildMode::Build,
            )));
        } else if core_cmd.eq("run") {
            return Ok(AnyonCommand::Build(BuildDriver::new(args, BuildMode::Run)));
        } else if core_cmd.eq("version") {
            return Ok(AnyonCommand::Version(VersionDriver::new(args)));
        } else if core_cmd.eq("help") {
            return Ok(AnyonCommand::Help(HelpDriver::new(args)));
        } else {
            return Err(AnyonError::new_with_command(format!(
                "no such command: {}",
                core_cmd
            )));
        }
    }
}
