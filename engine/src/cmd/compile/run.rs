use super::build::build;
use crate::{backend::vm::VM, error::core::JarvilError};

pub fn run(code_vec: Vec<char>) -> Result<(), JarvilError> {
    let chunk = build(code_vec)?;
    let mut vm = VM::new(chunk);
    vm.run(); // TODO - check for runtime errors here
    Ok(())
}
