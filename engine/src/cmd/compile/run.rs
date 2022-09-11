use super::build::build;
use crate::{
    backend::vm::VM,
    error::{core::JarvilError, diagnostics::Diagnostics},
};

pub fn run(code_vec: Vec<char>) -> Result<(), Diagnostics> {
    let chunk = build(code_vec)?;
    let mut vm = VM::new(chunk);
    vm.run(); // TODO - check for runtime errors here
    Ok(())
}
