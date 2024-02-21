use super::core::AbstractCommand;
use super::error::AnyonError;
use super::helper::check_jarvil_code_file_extension;
use compiler::code::JarvilCode;
use compiler::error::constants::TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG;
use compiler::{build_code, curr_dir_path};
use std::fs;
use std::process::Command;
use std::str;

#[derive(Debug)]
pub enum BuildMode {
    Build,
    Run,
}

#[derive(Debug)]
pub struct BuildCommand {
    command_line_args: Vec<String>,
    mode: BuildMode,
    alternate_code_file_name: Option<String>,
}

impl BuildCommand {
    pub fn new(command_line_args: Vec<String>, mode: BuildMode) -> Self {
        BuildCommand {
            command_line_args,
            mode,
            alternate_code_file_name: None,
        }
    }

    pub fn code_file_name(&self) -> String {
        match &self.alternate_code_file_name {
            Some(file_name) => file_name.to_string(),
            None => "main".to_string(),
        }
    }
}

impl AbstractCommand for BuildCommand {
    fn check_cmd(&mut self) -> Result<(), AnyonError> {
        let len = self.command_line_args.len();
        if len == 2 {
            Ok(())
        } else if len == 3 {
            let file_name = check_jarvil_code_file_extension(&self.command_line_args[2])?;
            self.alternate_code_file_name = Some(file_name.to_string());
            Ok(())
        } else {
            Err(AnyonError::new_with_command(
                TOO_MANY_COMMAND_LINE_ARGUMENTS_PASSED_ERROR_MSG.to_string(),
            ))
        }
    }

    fn execute_cmd(&self) -> Result<(), AnyonError> {
        let curr_dir_path = curr_dir_path();
        let code_file_name = self.code_file_name();
        let jarvil_code_file_path = format!("{}/{}.jv", curr_dir_path, code_file_name);
        let transpiled_py_code_file_path = format!(
            "{}/__transpiled_{}_py_code__.py",
            curr_dir_path, code_file_name
        );

        let code_str = fs::read_to_string(jarvil_code_file_path)?;
        let code = JarvilCode::new(&code_str);
        let py_code = build_code(code, code_str)?;
        fs::write(&transpiled_py_code_file_path, py_code)?;
        // format the Python code using `black` if available
        let _ = Command::new("python3")
            .arg("-m")
            .arg("black")
            .arg(&transpiled_py_code_file_path)
            .output()?;
        match self.mode {
            BuildMode::Run => {
                let output = Command::new("python3")
                    .arg(transpiled_py_code_file_path)
                    .output()?;
                let std_out_len = output.stdout.len();
                let std_err_len = output.stderr.len();
                let output_str = if std_err_len > 0 {
                    let msg = str::from_utf8(&output.stderr)?;
                    format!("\nPython Runtime Error Occured\n\n{}", msg)
                } else if std_out_len > 0 {
                    let msg = str::from_utf8(&output.stdout[..std_out_len - 1])?;
                    msg.to_string()
                } else {
                    "".to_string()
                };
                println!("{}", output_str);
            }
            BuildMode::Build => {}
        }
        Ok(())
    }

    fn help_str(&self) -> String {
        todo!()
    }
}
