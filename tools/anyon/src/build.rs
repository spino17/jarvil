use super::error::AnyonError;
use crate::helper::curr_dir_path;
use compiler::build_code;
use compiler::code::JarvilCode;
use std::fs;
use std::process::Command;
use std::str;

#[derive(Debug)]
pub enum BuildMode {
    Build,
    Run,
}

pub fn execute_build_or_run(mode: BuildMode) -> Result<(), AnyonError> {
    let curr_dir_path = curr_dir_path();
    let code_file_name = "main";
    let jarvil_code_file_path = format!("{}/{}.jv", curr_dir_path, code_file_name);
    let transpiled_py_code_file_path = format!(
        "{}/__transpiled_{}_py_code__.py",
        curr_dir_path, code_file_name
    );
    let ast_file_path = format!("{}/__ast_{}.json", curr_dir_path, code_file_name);
    let code_str = fs::read_to_string(jarvil_code_file_path)?;
    let code = JarvilCode::new(&code_str);
    let (build_result, ast_str) = build_code(code);

    fs::write(&ast_file_path, ast_str)?;

    let py_code = match build_result {
        Ok(py_code) => py_code,
        Err(err) => return Err(err.into()),
    };

    fs::write(&transpiled_py_code_file_path, py_code)?;

    // format the Python code using `black` if available
    let _ = Command::new("python3")
        .arg("-m")
        .arg("black")
        .arg(&transpiled_py_code_file_path)
        .output()?;

    if matches!(mode, BuildMode::Build) {
        return Ok(());
    }

    // if mode is `Run` then go on to execute the generated python program
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

    Ok(())
}
