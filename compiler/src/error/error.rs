use super::diagnostics::Diagnostics;
use std::cell::UnsafeCell;

pub struct JarvilProgramAnalysisErrors {
    core: UnsafeCell<Vec<Diagnostics>>,
}

impl JarvilProgramAnalysisErrors {
    pub fn log_error(&self, err: Diagnostics) {
        // This method is unsafe! in favour of performance. This code is safe
        // if we guarentee that there will only be one mutable reference to the
        // `errors`. This condition currently holds true as throughout the AST
        // pass we are only pushing `err` to it with no other mutable or immutable
        // references.
        unsafe {
            let errors_ref = &mut *self.core.get();
            errors_ref.push(err);
        };
    }
}
