use crate::backend::data::Data;
use std::alloc;
use std::ptr;
use std::{alloc::Layout, ptr::NonNull};

const INIT_CAPACITY: usize = 8;
const LOAD_FACTOR: f64 = 2.0 / 3.0;
// const SEED: byte = ...

#[derive(Debug)]
pub struct OkEntry {
    hash: usize,
    key: Data,
    value: Data,
}

#[derive(Debug)]
pub enum Entry {
    OK(OkEntry), // active entries
    TOMBSTONE,   // soft-deleted entries
    NULL,        // empty slots
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
        let layout = CoreDictObject::layout(INIT_CAPACITY);
        assert!(layout.size() <= isize::MAX as usize, "allocation too large");
        let ptr = unsafe { alloc::alloc(layout) };
        let new_ptr = match NonNull::new(ptr as *mut Entry) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        new_ptr
    }

    fn grow() {
        // as soon as count goes up by load_factor * capacity we call grow(). We check this on insert.
        // grow basically takes each entry in the dictionary, calculate it's new hash based on new capacity
        // and insert into the new dictionary array.
        // finally override the entry pointer into the object to new array location.
        todo!()
    }

    fn find_entry(&self, ptr: NonNull<Entry>, cap: usize, key: Data) -> usize {
        // finds entry with key `key` for capacity `cap` in the array pointed by `ptr`
        // either finds the index where it matches the key or if slot is empty
        todo!()
    }

    fn insert(&self, key: Data, value: Data) -> Data {
        // returns the old value if key already exists
        todo!()
    }

    fn delete(&self, key: Data) -> Data {
        // returns the deleted value
        todo!()
    }

    fn has_key(&self, key: Data) -> bool {
        todo!()
    }
}

#[derive(Clone)]
pub struct ListObject(NonNull<CoreDictObject>);
