// Anyon is the package manager for Jarvil

use super::build::BuildDriver;

pub trait CheckCommand {
    fn check_cmd(&self) -> Result<(), ()>;
}

pub enum AnyonCommandKind {
    BUILD(BuildDriver),
    FMT,
    VERSION,
}

pub fn get_kind_from_command_line_args(args: Vec<String>) -> AnyonCommandKind {
    todo!()
}
