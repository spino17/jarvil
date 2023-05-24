use crate::error::constants::TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG;
use super::{core::AbstractCommand, error::AnyonError};

#[derive(Debug)]
pub struct HelpDriver {
    command_line_args: Vec<String>,
}

impl HelpDriver {
    pub fn new(command_line_args: Vec<String>) -> Self {
        HelpDriver { command_line_args }
    }
}

impl AbstractCommand for HelpDriver {
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
        let help_str = "
Jarvil's Package Manager

Usage: jarvil COMMAND [OPTIONS]

`OPTIONS` are optional arguments you can provide along with `COMMAND` to have custom build behaviour.

Below are all the Anyon `COMMAND`:
    build    Compile the Jarvil project and generate nessesary Python files
    new      Create a new Jarvil project
    run      Run the generated Python code using system-wide interpreter for runtime
    version  Gives the current installed version of Jarvil
    help     Shows this page

Below are valid `OPTIONS` for specific Anyon `COMMAND`:
    build    Can take other `.jv` file to build. If not provided, `main.jv` will be used.
    run      Can take other `.jv` file to run. If not provided, `main.jv` will be used.
";
        println!("{}", help_str);
        Ok(())
    }

    fn help_str(&self) -> String {
        todo!()
    }
}
