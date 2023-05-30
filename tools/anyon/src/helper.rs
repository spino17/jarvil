use super::error::AnyonError;
use compiler::context;
use std::path::Path;

pub fn check_jarvil_code_file_extension(file_name: &str) -> Result<String, AnyonError> {
    let complete_file_path = format!("{}/{}", context::curr_dir_path(), file_name);
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
