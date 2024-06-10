use super::diagnostics::Diagnostics;
use miette::Report;
use std::cell::UnsafeCell;

#[derive(Debug, Default)]
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

    pub fn is_empty(&self) -> bool {
        unsafe {
            let errors_ref = &*self.core.get();
            errors_ref.is_empty()
        }
    }

    pub fn first_error_report(&self) -> Option<Report> {
        unsafe {
            let errors_ref = &*self.core.get();

            if !errors_ref.is_empty() {
                return Some(errors_ref[0].report());
            }

            None
        }
    }
}
