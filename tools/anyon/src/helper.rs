use super::error::AnyonError;
use std::env;
use std::path::Path;

pub fn curr_dir_path() -> String {
    let curr_dir = env::current_dir().expect("failed to get current directory");
    let curr_dir_str = curr_dir.to_string_lossy();
    curr_dir_str.to_string()
}

pub fn check_jarvil_code_file_extension(file_name: &str) -> Result<String, AnyonError> {
    let complete_file_path = format!("{}/{}", curr_dir_path(), file_name);
    let path = Path::new(&complete_file_path);
    let Some(extension) = path.extension() else {
        return Err(AnyonError::new_with_vanilla(
            "provided file does not have an extension".to_string(),
        ));
    };
    if extension != "jv" {
        return Err(AnyonError::new_with_vanilla(
            "provided file does not have `.jv` extension".to_string(),
        ));
    }
    let Some(file_name) = path.file_stem() else {
        return Err(AnyonError::new_with_vanilla(
            "provided file does not have a valid name".to_string(),
        ));
    };
    match file_name.to_str() {
        Some(valid_file_name) => Ok(valid_file_name.to_string()),
        None => Err(AnyonError::new_with_vanilla(
            "provided file does not have a valid name".to_string(),
        )),
    }
}
