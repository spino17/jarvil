// Process-wide code generation settings.
//
// This was a `thread_local!`, which under threads is a silent correctness bug
// rather than merely a `Sync` problem: `set_indent` on one thread is invisible
// to code generation running on another, which would quietly fall back to the
// default and emit Python indented differently from what was asked for.
//
// An atomic makes the setting genuinely global and costs a relaxed load per
// indent, which is noise next to the string building around it.

use std::sync::atomic::{AtomicUsize, Ordering};

// default indentation is 4 spaces
const DEFAULT_INDENT_SPACES: usize = 4;

static INDENT_SPACES: AtomicUsize = AtomicUsize::new(DEFAULT_INDENT_SPACES);

pub fn set_indent(indent_spaces: usize) {
    INDENT_SPACES.store(indent_spaces, Ordering::Relaxed);
}

pub fn indent_spaces() -> usize {
    INDENT_SPACES.load(Ordering::Relaxed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indent_is_visible_across_threads() {
        // The point of the change: a setting made on one thread has to be seen
        // by another. The old thread-local would have returned the default here.
        set_indent(2);

        let observed = std::thread::spawn(indent_spaces).join().unwrap();

        assert_eq!(observed, 2);

        set_indent(DEFAULT_INDENT_SPACES);
    }
}
