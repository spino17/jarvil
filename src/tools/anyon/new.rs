use super::{core::AbstractCommand, error::AnyonError};

#[derive(Debug)]
pub struct NewDriver {
    command_line_args: Vec<String>,
}

impl NewDriver {
    pub fn new(command_line_args: Vec<String>) -> Self {
        NewDriver { command_line_args }
    }

    pub fn setup_new_project(&self) -> String {
        todo!()
    }
}

impl AbstractCommand for NewDriver {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        todo!()
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        todo!()
    }
}
