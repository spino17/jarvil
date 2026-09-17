use super::diagnostics::Diagnostics;
use miette::Report;
use std::sync::Mutex;

// Collects diagnostics across the analysis passes.
//
// This was an `UnsafeCell<Vec<_>>` guarded by a comment arguing that only one
// mutable reference ever existed. That argument is unverifiable by the compiler
// and false the moment anything reads while a pass is running -- and it made
// the whole type `!Sync`, which is what kept the compiler single-threaded.
//
// A `Mutex` costs an uncontended lock per diagnostic. Diagnostics are the cold
// path by definition: a program that logs enough of them for the lock to matter
// is already not compiling.
#[derive(Debug, Default)]
pub struct JarvilProgramAnalysisErrors {
    core: Mutex<Vec<Diagnostics>>,
}

impl JarvilProgramAnalysisErrors {
    fn lock(&self) -> std::sync::MutexGuard<'_, Vec<Diagnostics>> {
        self.core.lock().expect("diagnostics mutex poisoned")
    }

    pub fn log_error(&self, err: Diagnostics) {
        self.lock().push(err);
    }

    pub fn is_empty(&self) -> bool {
        self.lock().is_empty()
    }

    pub fn len(&self) -> usize {
        self.lock().len()
    }

    // Every diagnostic logged so far, in discovery order.
    //
    // Returns owned clones rather than a borrowing iterator: nothing can hand
    // out a reference into data behind a lock without holding the guard, and
    // holding it across a caller's loop invites a deadlock the first time that
    // loop logs something.
    pub fn diagnostics(&self) -> Vec<Diagnostics> {
        self.lock().clone()
    }

    pub fn reports(&self) -> Vec<Report> {
        self.lock().iter().map(|err| err.report()).collect()
    }

    pub fn first_error_report(&self) -> Option<Report> {
        self.lock().first().map(|err| err.report())
    }
}
