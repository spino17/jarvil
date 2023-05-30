use super::{core::AbstractCommand, error::AnyonError};
use compiler::{code::JarvilCode, error::diagnostics::Diagnostics};

#[derive(Debug)]
pub struct FmtCommand {
    command_line_args: Vec<String>,
}

impl FmtCommand {
    pub fn new(command_line_args: Vec<String>) -> Self {
        FmtCommand { command_line_args }
    }

    pub fn format_code(&self, code: JarvilCode) -> String {
        todo!()
    }
}

impl AbstractCommand for FmtCommand {
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
