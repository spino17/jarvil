use super::{core::AbstractCommand, error::AnyonError};
use compiler::error::{
    constants::TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG
};

#[derive(Debug)]
pub struct VersionCommand {
    command_line_args: Vec<String>,
}

impl VersionCommand {
    pub fn new(command_line_args: Vec<String>) -> Self {
        VersionCommand { command_line_args }
    }
}

impl AbstractCommand for VersionCommand {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        let len = self.command_line_args.len();
        if len > 2 {
            return Err(AnyonError::new_with_command(
                TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG.to_string(),
            ));
        }
        Ok(())
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        // TODO - change this to take the value of version from `.toml` file
        println!("jarvil 0.1.0");
        Ok(())
    }

    fn help_str(&self) -> String {
        todo!()
    }
}
