// This code is taken from the amazing blog on `Fast and Simple Rust Interner` by `matklad`:
// `https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html`

use rustc_hash::FxHashMap;
use std::sync::Mutex;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct IdentName(u32);

#[derive(Debug, Default)]
struct CoreInterner {
    map: FxHashMap<String, u32>,
    vec: Vec<String>,
}

// A `Mutex` rather than a `RefCell` so that the interner -- which every part of
// the semantic database reaches through -- is `Sync`.
//
// Contention is not a concern: interning happens during name resolution and the
// lock is held only for a map lookup or a push, never across a call back into
// the interner.
#[derive(Debug, Default)]
pub struct Interner(Mutex<CoreInterner>);

impl Interner {
    pub fn intern(&self, name: &str) -> IdentName {
        // One acquisition for the whole operation. The previous version took
        // the lock four separate times and then called `intern` again from a
        // `debug_assert`; under a non-reentrant lock that recursion is a
        // deadlock, so the invariants it checked are asserted directly instead.
        let mut core = self.0.lock().expect("interner mutex poisoned");

        if let Some(&index) = core.map.get(name) {
            return IdentName(index);
        }

        let index = core.vec.len() as u32;

        core.map.insert(name.to_owned(), index);
        core.vec.push(name.to_owned());

        debug_assert!(core.vec[index as usize] == name);
        debug_assert!(core.map[name] == index);

        IdentName(index)
    }

    pub fn lookup(&self, index: IdentName) -> String {
        self.0.lock().expect("interner mutex poisoned").vec[index.0 as usize].to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interning_is_idempotent() {
        let interner = Interner::default();

        let first = interner.intern("hello");
        let again = interner.intern("hello");
        let other = interner.intern("world");

        assert_eq!(first, again);
        assert_ne!(first, other);
        assert_eq!(interner.lookup(first), "hello");
        assert_eq!(interner.lookup(other), "world");
    }

    #[test]
    fn indices_are_dense_and_ordered() {
        // The original `intern` derived the new index from `map.len()` rather
        // than `vec.len()`. Those agree here, but only because the two are
        // pushed in lockstep; deriving it from the vec is what `lookup`
        // actually indexes into.
        let interner = Interner::default();

        for (expected, name) in ["a", "b", "c"].iter().enumerate() {
            let index = interner.intern(name);

            assert_eq!(index, IdentName(expected as u32));
            assert_eq!(interner.lookup(index), *name);
            assert_eq!(interner.intern(name), index);
        }
    }

    #[test]
    fn interning_from_many_threads_agrees() {
        use std::sync::Arc;
        use std::thread;

        let interner = Arc::new(Interner::default());
        let names = ["alpha", "beta", "gamma", "delta"];

        let handles: Vec<_> = (0..8)
            .map(|_| {
                let interner = Arc::clone(&interner);

                thread::spawn(move || {
                    names
                        .iter()
                        .map(|name| interner.intern(name))
                        .collect::<Vec<_>>()
                })
            })
            .collect();

        let results: Vec<Vec<IdentName>> = handles.into_iter().map(|h| h.join().unwrap()).collect();

        // every thread must have seen the same symbol for the same string
        for result in &results {
            assert_eq!(result, &results[0]);
        }

        for (index, name) in names.iter().enumerate() {
            assert_eq!(interner.lookup(results[0][index]), *name);
        }
    }
}
