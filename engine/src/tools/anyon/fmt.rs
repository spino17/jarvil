use super::{core::AbstractCommand, error::AnyonError};
use crate::{code::JarvilCode, error::diagnostics::Diagnostics};

#[derive(Debug)]
pub struct FmtDriver {
    command_line_args: Vec<String>,
}

impl FmtDriver {
    pub fn new(command_line_args: Vec<String>) -> Self {
        FmtDriver { command_line_args }
    }

    pub fn format_code(&self, code: JarvilCode) -> String {
        todo!()
    }
}

impl AbstractCommand for FmtDriver {
    fn check_cmd(&self) -> Result<(), AnyonError> {
        todo!()
    }

    fn execute_cmd(&self) {
        todo!()
    }
}
