use super::error::AnyonError;
use crate::helper::curr_dir_path;
use compiler::codegen::python::whitespaces_from_indent_level;
use std::{fs, process::Command};

pub fn execute_new(project_name: &str) -> Result<(), AnyonError> {
    let curr_dir_path = curr_dir_path();
    let main_file_path = format!("{}/{}/main.jv", curr_dir_path, project_name);
    let _ = Command::new("mkdir").arg(project_name).output()?;
    let default_main_func = format!(
        "def main():\n{}print(\"Hello, World!\")",
        whitespaces_from_indent_level(1)
    );
    fs::write(main_file_path, default_main_func)?;
    Ok(())
}
