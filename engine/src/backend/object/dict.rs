use crate::backend::data::Data;
use std::alloc;
use std::fmt::Display;
use std::ptr;
use std::{alloc::Layout, ptr::NonNull};

const INIT_CAPACITY: usize = 8;
const LOAD_FACTOR: f64 = 2.0 / 3.0;
// const SEED: byte = ...

#[derive(Debug, Clone)]
pub struct OkEntry {
    hash: usize,
    key: Data,
    value: Data,
}

#[derive(Debug, Clone)]
pub enum Entry {
    OK(OkEntry), // active entries
    TOMBSTONE,   // soft-deleted entries
    NULL,        // empty slots
}

impl Entry {
    fn is_ok(&self) -> bool {
        match self {
            Entry::OK(_) => true,
            _ => false,
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Entry::NULL => true,
            _ => false,
        }
    }

    fn is_tombstone(&self) -> bool {
        match self {
            Entry::TOMBSTONE => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct CoreDictObject {
    count: usize,
    cap: usize,
    pub ptr: NonNull<Entry>,
}

impl CoreDictObject {
    pub fn new() -> Self {
        let new_ptr = CoreDictObject::allocate(INIT_CAPACITY);
        unsafe {
            // initialize the dictionary with NULL slots
            for i in 0..INIT_CAPACITY {
                ptr::write(new_ptr.as_ptr().add(i), Entry::NULL);
            }
        }
        CoreDictObject {
            count: 0,
            cap: INIT_CAPACITY,
            ptr: new_ptr,
        }
    }

    fn layout(len: usize) -> Layout {
        Layout::array::<Entry>(len).unwrap()
    }

    fn allocate(len: usize) -> NonNull<Entry> {
        let layout = CoreDictObject::layout(len);
        assert!(layout.size() <= isize::MAX as usize, "allocation too large");
        let ptr = unsafe { alloc::alloc(layout) };
        let new_ptr = match NonNull::new(ptr as *mut Entry) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        new_ptr
    }

    pub fn grow(&mut self) {
        let new_cap = 2 * self.cap;
        let new_ptr = CoreDictObject::allocate(new_cap);
        unsafe {
            for i in 0..new_cap {
                ptr::write(new_ptr.as_ptr().add(i), Entry::NULL);
            }
            for i in 0..self.cap {
                let (key, value) = match &*self.ptr.as_ptr().add(i) {
                    Entry::OK(ok_entry) => (&ok_entry.key, &ok_entry.value),
                    _ => continue,
                };
                let (entry_index, hash) = self.find_entry(new_ptr.clone(), new_cap, key);
                *new_ptr.as_ptr().add(entry_index) = Entry::OK(OkEntry {
                    hash,
                    key: key.clone(),
                    value: value.clone(),
                });
            }
            alloc::dealloc(
                self.ptr.as_ptr() as *mut u8,
                CoreDictObject::layout(self.cap),
            );
        }
        self.cap = new_cap;
        self.ptr = new_ptr;
    }

    fn len(&self) -> usize {
        self.count
    }

    fn cap(&self) -> usize {
        self.cap
    }

    fn find_entry(&self, ptr: NonNull<Entry>, cap: usize, key: &Data) -> (usize, usize) {
        // (entry_index, hash)
        // finds entry with key `key` for capacity `cap` in the array pointed by `ptr`
        // either finds the index where it matches the key or if slot is empty
        todo!()
    }

    fn insert(&mut self, key: Data, value: Data) -> Option<Data> {
        if self.count as f64 > (self.cap as f64 * LOAD_FACTOR) {
            self.grow();
        }
        let (entry_index, hash) = self.find_entry(self.ptr.clone(), self.cap, &key);
        unsafe {
            let old_value = match &*self.ptr.as_ptr().add(entry_index) {
                Entry::OK(ok_entry) => Some(ok_entry.value.clone()),
                _ => {
                    self.count = self.count + 1;
                    None
                }
            };
            *self.ptr.as_ptr().add(entry_index) = Entry::OK(OkEntry { hash, key, value });
            return old_value;
        }
    }

    fn delete(&self, key: Data) -> Data {
        // returns the deleted value
        todo!()
    }

    fn has_key(&self, key: Data) -> bool {
        todo!()
    }
}

impl Drop for CoreDictObject {
    fn drop(&mut self) {
        let layout = CoreDictObject::layout(self.cap);
        unsafe {
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

impl Display for CoreDictObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = "{ ".to_string();
        if self.count != 0 {
            unsafe {
                let mut flag = false;
                for i in 0..self.cap {
                    if flag {
                        s.push_str(", ");
                    } else {
                        flag = true;
                    }
                    match &*self.ptr.as_ptr().add(i) {
                        Entry::OK(ok_entry) => {
                            s.push_str(&format!("{}: {}", ok_entry.key, ok_entry.value))
                        }
                        _ => continue,
                    }
                }
            }
        }
        s.push_str(" }");
        write!(f, "{}", s)
    }
}

#[derive(Clone)]
pub struct ListObject(NonNull<CoreDictObject>);
