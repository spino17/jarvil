use super::core::CheckCommand;
use crate::{code::JarvilCode, error::diagnostics::Diagnostics};

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

impl CheckCommand for FmtDriver {
    fn check_cmd(&self) -> Result<(), ()> {
        todo!()
    }
}
