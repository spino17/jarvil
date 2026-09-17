//! The collector every pass pushes diagnostics into.
//!
//! One collector is threaded through lexing, parsing, resolution and type
//! checking, which is what lets a single compile report problems found by
//! several passes rather than stopping at the first.

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

    /// Records a diagnostic. Callable from any pass, and from any thread.
    pub fn log_error(&self, err: Diagnostics) {
        self.lock().push(err);
    }

    /// Whether anything has been reported.
    pub fn is_empty(&self) -> bool {
        self.lock().is_empty()
    }

    /// How many diagnostics have been reported.
    pub fn len(&self) -> usize {
        self.lock().len()
    }

    /// Every diagnostic logged so far, in discovery order.
    ///
    /// Returns owned clones rather than a borrowing iterator: a reference into
    /// data behind the lock would require holding the guard, and holding it
    /// across a caller's loop invites deadlock the first time that loop logs
    /// something.
    pub fn diagnostics(&self) -> Vec<Diagnostics> {
        self.lock().clone()
    }

    /// Every diagnostic as a renderable miette report.
    pub fn reports(&self) -> Vec<Report> {
        self.lock().iter().map(|err| err.report()).collect()
    }

    /// The first diagnostic reported, if any.
    ///
    /// What `jarvil_py::build_code` surfaces. Prefer [`Self::diagnostics`]
    /// when all of them are wanted.
    pub fn first_error_report(&self) -> Option<Report> {
        self.lock().first().map(|err| err.report())
    }
}
