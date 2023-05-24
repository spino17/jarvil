use std::{fs, mem, process::Command};

use crate::{codegen::python::get_whitespaces_from_indent_level, context};

use super::{core::AbstractCommand, error::AnyonError};

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
        // TODO - add logic to check the command
        // argument at index `2` should exist
        self.project_name = Some(mem::take(&mut self.command_line_args[2]));
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
        let _ = Command::new("touch").arg(&main_file_path).output()?;
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
