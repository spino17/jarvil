// This code is taken from the amazing blog on `Fast and Simple Rust Interner` by `matklad`:
// `https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html`

use rustc_hash::FxHashMap;
use std::cell::RefCell;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct IdentName(u32);

#[derive(Debug, Default)]
struct CoreInterner {
    map: FxHashMap<String, u32>,
    vec: Vec<String>,
}

#[derive(Debug, Default)]
pub struct Interner(RefCell<CoreInterner>);

impl Interner {
    pub fn intern(&self, name: &str) -> IdentName {
        if let Some(&idx) = self.0.borrow().map.get(name) {
            return IdentName(idx);
        }

        let idx = self.0.borrow().map.len() as u32;
        self.0.borrow_mut().map.insert(name.to_owned(), idx);
        self.0.borrow_mut().vec.push(name.to_owned());

        debug_assert!(self.lookup(IdentName(idx)) == name);
        debug_assert!(self.intern(name) == IdentName(idx));

        IdentName(idx)
    }

    pub fn lookup(&self, idx: IdentName) -> String {
        self.0.borrow().vec[idx.0 as usize].to_string()
    }
}
