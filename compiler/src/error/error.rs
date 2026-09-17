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

    // Borrows every diagnostic logged so far, in discovery order.
    //
    // Safe on the same terms as `log_error`: callers hold this only long enough
    // to read the diagnostics, and nothing logs during that read.
    pub fn iter(&self) -> std::slice::Iter<'_, Diagnostics> {
        unsafe {
            let errors_ref = &*self.core.get();

            errors_ref.iter()
        }
    }

    // Every diagnostic logged during the run, in the order they were found.
    //
    // `build_code` still surfaces only the first one; this exists so that tests
    // can assert on the full set, and is the accessor the driver should move to
    // when multi-error reporting lands.
    pub fn reports(&self) -> Vec<Report> {
        unsafe {
            let errors_ref = &*self.core.get();

            errors_ref.iter().map(|err| err.report()).collect()
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
