use crate::{
    build::{BuildCommand, BuildMode},
    core::AnyonCommand,
    help::HelpCommand,
    new::NewCommand,
    version::VersionCommand,
};

use super::error::AnyonError;
use compiler::curr_dir_path;
use std::path::Path;

pub fn get_cmd_from_command_line_args(args: Vec<String>) -> Result<AnyonCommand, AnyonError> {
    if args.len() < 2 {
        return Ok(AnyonCommand::Help(HelpCommand::new(args)));
    } else {
        let core_cmd = &args[1];
        if core_cmd.eq("new") {
            return Ok(AnyonCommand::New(NewCommand::new(args)));
        } else if core_cmd.eq("build") {
            return Ok(AnyonCommand::Build(BuildCommand::new(
                args,
                BuildMode::Build,
            )));
        } else if core_cmd.eq("run") {
            return Ok(AnyonCommand::Build(BuildCommand::new(args, BuildMode::Run)));
        } else if core_cmd.eq("version") {
            return Ok(AnyonCommand::Version(VersionCommand::new(args)));
        } else if core_cmd.eq("help") {
            return Ok(AnyonCommand::Help(HelpCommand::new(args)));
        } else {
            return Err(AnyonError::new_with_command(format!(
                "no such command: {}",
                core_cmd
            )));
        }
    }
}

pub fn check_jarvil_code_file_extension(file_name: &str) -> Result<String, AnyonError> {
    let complete_file_path = format!("{}/{}", curr_dir_path(), file_name);
    let path = Path::new(&complete_file_path);
    match path.extension() {
        Some(extension) => {
            if extension == "jv" {
                match path.file_stem() {
                    Some(file_name) => match file_name.to_str() {
                        Some(valid_file_name) => return Ok(valid_file_name.to_string()),
                        None => {
                            return Err(AnyonError::new_with_vanilla(
                                "provided file does not have a valid name".to_string(),
                            ))
                        }
                    },
                    None => {
                        return Err(AnyonError::new_with_vanilla(
                            "provided file does not have a valid name".to_string(),
                        ))
                    }
                }
            } else {
                return Err(AnyonError::new_with_vanilla(
                    "provided file does not have `.jv` extension".to_string(),
                ));
            }
        }
        None => {
            return Err(AnyonError::new_with_vanilla(
                "provided file does not have an extension".to_string(),
            ))
        }
    }
}
