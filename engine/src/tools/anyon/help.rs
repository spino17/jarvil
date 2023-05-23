use super::{core::AbstractCommand, error::AnyonError};

#[derive(Debug)]
pub struct HelpDriver {
    command_line_args: Vec<String>,
}

impl HelpDriver {
    pub fn new(command_line_args: Vec<String>) -> Self {
        HelpDriver { command_line_args }
    }

    pub fn get_help_str(&self) -> String {
        todo!()
    }
}

impl AbstractCommand for HelpDriver {
    fn check_cmd(&self) -> Result<(), AnyonError> {
        todo!()
    }

    fn execute_cmd(&self) {
        todo!()
    }
}
