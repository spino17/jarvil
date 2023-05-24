use super::{core::AbstractCommand, error::AnyonError};
use crate::error::diagnostics::Diagnostics;

#[derive(Debug)]
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

impl AbstractCommand for VersionDriver {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        todo!()
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        todo!()
    }

    fn help_str(&self) -> String {
        todo!()
    }
}
