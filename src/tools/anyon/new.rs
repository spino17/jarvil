use super::{core::AbstractCommand, error::AnyonError};
use crate::{
    codegen::python::get_whitespaces_from_indent_level, context,
    error::constants::TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG,
};
use std::{fs, mem, process::Command};

#[derive(Debug)]
pub struct NewDriver {
    command_line_args: Vec<String>,
    project_name: Option<String>,
}

impl NewDriver {
    pub fn new(command_line_args: Vec<String>) -> Self {
        NewDriver {
            command_line_args,
            project_name: None,
        }
    }
}

impl AbstractCommand for NewDriver {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        let len = self.command_line_args.len();
        if len < 3 {
            return Err(AnyonError::new_with_vanilla(
                "project name is missing".to_string(),
            ));
        } else if len == 3 {
            self.project_name = Some(mem::take(&mut self.command_line_args[2]));
        } else {
            return Err(AnyonError::new_with_vanilla(
                TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG.to_string(),
            ));
        }
        Ok(())
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        let curr_dir_path = context::curr_dir_path();
        let project_name = match &self.project_name {
            Some(project_name) => project_name,
            None => unreachable!(), // `project_name` is set in `check_cmd`
        };
        let main_file_path = format!("{}/{}/main.jv", curr_dir_path, project_name);
        let _ = Command::new("mkdir").arg(project_name).output()?;
        // let _ = Command::new("touch").arg(&main_file_path).output()?;
        let default_main_func = format!(
            "def main():\n{}print(\"Hello, World!\")",
            get_whitespaces_from_indent_level(1)
        );
        fs::write(main_file_path, default_main_func)?;
        Ok(())
    }

    fn help_str(&self) -> String {
        todo!()
    }
}
