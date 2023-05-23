// Anyon is the package manager for Jarvil
use super::{
    build::BuildDriver, error::AnyonError, fmt::FmtDriver, help::HelpDriver, version::VersionDriver,
};

pub trait AbstractCommand {
    fn check_cmd(&self) -> Result<(), AnyonError>;
    fn execute_cmd(&self);
}

#[derive(Debug)]
pub enum AnyonCommandKind {
    BUILD(BuildDriver),
    FMT(FmtDriver),
    VERSION(VersionDriver),
    HELP(HelpDriver),
}

impl AbstractCommand for AnyonCommandKind {
    fn check_cmd(&self) -> Result<(), AnyonError> {
        match self {
            AnyonCommandKind::BUILD(build_driver) => build_driver.check_cmd(),
            AnyonCommandKind::FMT(fmt_driver) => fmt_driver.check_cmd(),
            AnyonCommandKind::VERSION(version_driver) => version_driver.check_cmd(),
            AnyonCommandKind::HELP(help_driver) => help_driver.check_cmd(),
        }
    }

    fn execute_cmd(&self) {
        match self {
            AnyonCommandKind::BUILD(build_driver) => build_driver.execute_cmd(),
            AnyonCommandKind::FMT(fmt_driver) => fmt_driver.execute_cmd(),
            AnyonCommandKind::VERSION(version_driver) => version_driver.execute_cmd(),
            AnyonCommandKind::HELP(help_driver) => help_driver.execute_cmd(),
        }
    }
}

pub fn get_cmd_from_command_line_args(args: Vec<String>) -> Result<AnyonCommandKind, AnyonError> {
    if args.len() < 2 {
        return Ok(AnyonCommandKind::VERSION(VersionDriver::new(args)));
    } else {
        let core_cmd = &args[1];
        if core_cmd.eq("build") {
            return Ok(AnyonCommandKind::BUILD(BuildDriver::new(args)));
        } else if core_cmd.eq("version") {
            return Ok(AnyonCommandKind::VERSION(VersionDriver::new(args)));
        } else if core_cmd.eq("help") {
            return Ok(AnyonCommandKind::HELP(HelpDriver::new(args)));
        } else {
            return Err(AnyonError::new(format!("no such command: `{}`", core_cmd)));
        }
    }
}
