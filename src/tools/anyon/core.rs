// Anyon is the package manager for Jarvil
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
}

#[derive(Debug)]
pub enum AnyonCommand {
    NEW(NewDriver),
    BUILD(BuildDriver),
    FMT(FmtDriver),
    VERSION(VersionDriver),
    HELP(HelpDriver),
}

impl AbstractCommand for AnyonCommand {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        match self {
            AnyonCommand::NEW(new_driver) => new_driver.check_cmd(),
            AnyonCommand::BUILD(build_driver) => build_driver.check_cmd(),
            AnyonCommand::FMT(fmt_driver) => fmt_driver.check_cmd(),
            AnyonCommand::VERSION(version_driver) => version_driver.check_cmd(),
            AnyonCommand::HELP(help_driver) => help_driver.check_cmd(),
        }
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        match self {
            AnyonCommand::NEW(new_driver) => new_driver.execute_cmd(),
            AnyonCommand::BUILD(build_driver) => build_driver.execute_cmd(),
            AnyonCommand::FMT(fmt_driver) => fmt_driver.execute_cmd(),
            AnyonCommand::VERSION(version_driver) => version_driver.execute_cmd(),
            AnyonCommand::HELP(help_driver) => help_driver.execute_cmd(),
        }
    }
}

pub fn get_cmd_from_command_line_args(args: Vec<String>) -> Result<AnyonCommand, AnyonError> {
    if args.len() < 2 {
        return Ok(AnyonCommand::HELP(HelpDriver::new(args)));
    } else {
        let core_cmd = &args[1];
        if core_cmd.eq("new") {
            return Ok(AnyonCommand::NEW(NewDriver::new(args)));
        } else if core_cmd.eq("build") {
            return Ok(AnyonCommand::BUILD(BuildDriver::new(
                args,
                BuildMode::BUILD,
            )));
        } else if core_cmd.eq("run") {
            return Ok(AnyonCommand::BUILD(BuildDriver::new(args, BuildMode::RUN)));
        } else if core_cmd.eq("version") {
            return Ok(AnyonCommand::VERSION(VersionDriver::new(args)));
        } else if core_cmd.eq("help") {
            return Ok(AnyonCommand::HELP(HelpDriver::new(args)));
        } else {
            return Err(AnyonError::new_with_vanilla(
                "command not found".to_string(),
            ));
        }
    }
}
