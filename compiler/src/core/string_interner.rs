// This code is taken from the amazing blog on `Fast and Simple Rust Interner` by `matklad`:
// `https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html`

use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct StrId(u32);

#[derive(Default)]
pub struct Interner {
    map: FxHashMap<String, u32>,
    vec: Vec<String>,
}

impl Interner {
    pub fn intern(&mut self, name: &str) -> StrId {
        if let Some(&idx) = self.map.get(name) {
            return StrId(idx);
        }
        let idx = self.map.len() as u32;
        self.map.insert(name.to_owned(), idx);
        self.vec.push(name.to_owned());
        debug_assert!(self.lookup(StrId(idx)) == name);
        debug_assert!(self.intern(name) == StrId(idx));
        StrId(idx)
    }

    pub fn lookup(&self, idx: StrId) -> &str {
        self.vec[idx.0 as usize].as_str()
    }
}