use super::core::CheckCommand;
use crate::error::diagnostics::Diagnostics;

pub struct VersionDriver {
    command_line_args: Vec<String>,
}

impl VersionDriver {
    pub fn new(command_line_args: Vec<String>) -> Self {
        VersionDriver { command_line_args }
    }

    pub fn get_version(&self) -> String {
        todo!()
    }
}

impl CheckCommand for VersionDriver {
    fn check_cmd(&self) -> Result<(), ()> {
        todo!()
    }
}
