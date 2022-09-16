use crate::backend::data::Data;
use std::alloc;
use std::fmt::Display;
use std::marker::PhantomData;
use std::ptr;
use std::{alloc::Layout, ptr::NonNull};

const INIT_CAPACITY: usize = 8;
const LOAD_FACTOR: f64 = 2.0 / 3.0;
// const SEED: byte = ...

#[derive(Debug, Clone)]
struct OkEntry {
    hash: usize,
    key: Data,
    value: Data,
}

#[derive(Debug, Clone)]
enum Entry {
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
    count: usize, // count here is = active entries + deleted entries (TOMBSTONE)
    cap: usize,
    ptr: NonNull<Entry>,
    _marker: PhantomData<Entry>,
}

impl CoreDictObject {
    fn new() -> Self {
        let new_ptr = CoreDictObject::allocate(INIT_CAPACITY);
        unsafe {
            // initialize the dictionary with `NULL` slots
            for i in 0..INIT_CAPACITY {
                ptr::write(new_ptr.as_ptr().add(i), Entry::NULL);
            }
        }
        CoreDictObject {
            count: 0,
            cap: INIT_CAPACITY,
            ptr: new_ptr,
            _marker: PhantomData,
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

    fn find_entry(ptr: NonNull<Entry>, cap: usize, key: &Data) -> (usize, usize) {
        let mut tombstone_index: Option<usize> = None;
        let hash = key.hash();
        let mut index = hash % cap;
        unsafe {
            loop {
                let entry = &*ptr.as_ptr().add(index);
                match entry {
                    Entry::OK(ok_entry) => {
                        let curr_key = &ok_entry.key;
                        if curr_key == key {
                            return (index, hash);
                        }
                    }
                    Entry::NULL => match tombstone_index {
                        Some(tombstone_index) => return (tombstone_index, hash),
                        None => return (index, hash),
                    },
                    Entry::TOMBSTONE => {
                        if let None = tombstone_index {
                            tombstone_index = Some(index);
                        }
                    }
                }
                index = (index + 1) % cap;
            }
        }
    }

    fn grow(&mut self) {
        let new_cap = 2 * self.cap;
        let new_ptr = CoreDictObject::allocate(new_cap);
        let mut new_count = 0;
        unsafe {
            // initialize new array with `NULL` slots
            for i in 0..new_cap {
                ptr::write(new_ptr.as_ptr().add(i), Entry::NULL);
            }
            // take entries from old array, compute the new hash and insert into the new array
            for i in 0..self.cap {
                let (key, value) = match &*self.ptr.as_ptr().add(i) {
                    Entry::OK(ok_entry) => {
                        new_count = new_count + 1;
                        (&ok_entry.key, &ok_entry.value)
                    }
                    _ => continue,
                };
                let (entry_index, hash) = CoreDictObject::find_entry(new_ptr.clone(), new_cap, key);
                *new_ptr.as_ptr().add(entry_index) = Entry::OK(OkEntry {
                    hash,
                    key: key.clone(),
                    value: value.clone(),
                });
            }
            // free up the old array allocation
            alloc::dealloc(
                self.ptr.as_ptr() as *mut u8,
                CoreDictObject::layout(self.cap),
            );
        }
        self.count = new_count;
        self.cap = new_cap;
        self.ptr = new_ptr;
    }

    fn len(&self) -> usize {
        self.count
    }

    fn cap(&self) -> usize {
        self.cap
    }

    fn insert(&mut self, key: Data, value: Data) -> Option<Data> {
        if self.count as f64 > (self.cap as f64 * LOAD_FACTOR) {
            self.grow();
        }
        let (entry_index, hash) = CoreDictObject::find_entry(self.ptr.clone(), self.cap, &key);
        unsafe {
            let old_value = match &*self.ptr.as_ptr().add(entry_index) {
                Entry::OK(ok_entry) => Some(ok_entry.value.clone()),
                Entry::NULL => {
                    self.count = self.count + 1;
                    None
                }
                Entry::TOMBSTONE => None,
            };
            *self.ptr.as_ptr().add(entry_index) = Entry::OK(OkEntry { hash, key, value });
            return old_value;
        }
    }

    fn lookup(&self, key: &Data) -> Option<Data> {
        if self.count == 0 {
            return None;
        }
        let (entry_index, _) = CoreDictObject::find_entry(self.ptr.clone(), self.cap, key);
        unsafe {
            match &*self.ptr.as_ptr().add(entry_index) {
                Entry::OK(ok_entry) => return Some(ok_entry.value.clone()),
                _ => return None,
            }
        }
    }

    fn delete(&self, key: &Data) -> Option<Data> {
        if self.count == 0 {
            return None;
        }
        let (entry_index, _) = CoreDictObject::find_entry(self.ptr.clone(), self.cap, key);
        unsafe {
            let entry = match &*self.ptr.as_ptr().add(entry_index) {
                Entry::OK(ok_entry) => &ok_entry.value,
                _ => return None,
            };
            *self.ptr.as_ptr().add(entry_index) = Entry::TOMBSTONE;
            Some(entry.clone())
        }
    }

    fn has_key(&self, key: &Data) -> bool {
        let (entry_index, _) = CoreDictObject::find_entry(self.ptr.clone(), self.cap, key);
        unsafe {
            match &*self.ptr.as_ptr().add(entry_index) {
                Entry::OK(_) => return true,
                _ => return false,
            }
        }
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
pub struct DictObject(NonNull<CoreDictObject>);

impl DictObject {
    fn new() -> Self {
        let x_ptr = Box::into_raw(Box::new(CoreDictObject::new()));
        let ptr = unsafe { NonNull::new_unchecked(x_ptr) };
        DictObject(ptr)
    }

    fn len(&self) -> usize {
        unsafe { (&*self.0.as_ptr()).len() }
    }

    fn cap(&self) -> usize {
        unsafe { (&*self.0.as_ptr()).cap() }
    }

    fn insert(&self, key: Data, value: Data) -> Option<Data> {
        unsafe { (&mut *self.0.as_ptr()).insert(key, value) }
    }

    fn lookup(&self, key: &Data) -> Option<Data> {
        unsafe { (&*self.0.as_ptr()).lookup(key) }
    }

    fn delete(&self, key: &Data) -> Option<Data> {
        unsafe { (&*self.0.as_ptr()).delete(key) }
    }

    fn has_key(&self, key: &Data) -> bool {
        unsafe { (&*self.0.as_ptr()).has_key(key) }
    }

    fn manual_drop(&self) {
        unsafe {
            Box::from_raw(self.0.as_ptr());
        }
    }
}

impl Display for DictObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "{}", (*self.0.as_ptr()).to_string()) }
    }
}