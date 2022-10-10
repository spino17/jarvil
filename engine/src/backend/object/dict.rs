use crate::backend::data::Data;
use crate::backend::vm::VM;
use std::alloc;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
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

    fn find_entry(ptr: NonNull<Entry>, hash: usize, cap: usize, key: &Data, vm: &mut VM) -> usize {
        let mut tombstone_index: Option<usize> = None;
        let mut index = hash % cap;
        unsafe {
            loop {
                let entry = &*ptr.as_ptr().add(index);
                match entry {
                    Entry::OK(ok_entry) => {
                        let curr_key = &ok_entry.key;
                        let curr_hash = ok_entry.hash;
                        // TODO - complete this function
                        /*
                        if curr_hash == hash && curr_key == key {
                            return index
                        }
                         */
                    }
                    Entry::NULL => match tombstone_index {
                        Some(tombstone_index) => return tombstone_index,
                        None => return index,
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

    fn grow(&mut self, vm: &mut VM) {
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
                let (key, value, hash) = match &*self.ptr.as_ptr().add(i) {
                    Entry::OK(ok_entry) => {
                        new_count = new_count + 1;
                        (&ok_entry.key, &ok_entry.value, ok_entry.hash)
                    }
                    _ => continue,
                };
                let entry_index =
                    CoreDictObject::find_entry(new_ptr.clone(), hash, new_cap, key, vm);
                *new_ptr.as_ptr().add(entry_index) = Entry::OK(OkEntry {
                    hash,
                    key: key.clone(),
                    value: value.clone(),
                });
            }
            // free up the old array allocation
            for i in 0..self.cap {
                ptr::drop_in_place(self.ptr.as_ptr().add(i));
            }
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

    fn insert(&mut self, key: Data, hash: usize, value: Data, vm: &mut VM) -> Option<Data> {
        if self.count as f64 > (self.cap as f64 * LOAD_FACTOR) {
            self.grow(vm);
        }
        let entry_index = CoreDictObject::find_entry(self.ptr.clone(), hash, self.cap, &key, vm);
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

    fn lookup(&self, key: &Data, hash: usize, vm: &mut VM) -> Option<Data> {
        if self.count == 0 {
            return None;
        }
        let entry_index = CoreDictObject::find_entry(self.ptr.clone(), hash, self.cap, key, vm);
        unsafe {
            match &*self.ptr.as_ptr().add(entry_index) {
                Entry::OK(ok_entry) => return Some(ok_entry.value.clone()),
                _ => return None,
            }
        }
    }

    fn delete(&self, key: &Data, hash: usize, vm: &mut VM) -> Option<Data> {
        if self.count == 0 {
            return None;
        }
        let entry_index = CoreDictObject::find_entry(self.ptr.clone(), hash, self.cap, key, vm);
        unsafe {
            let value = match &*self.ptr.as_ptr().add(entry_index) {
                Entry::OK(ok_entry) => &ok_entry.value,
                _ => return None,
            };
            *self.ptr.as_ptr().add(entry_index) = Entry::TOMBSTONE;
            Some(value.clone())
        }
    }

    fn has_key(&self, key: &Data, hash: usize, vm: &mut VM) -> bool {
        let entry_index = CoreDictObject::find_entry(self.ptr.clone(), hash, self.cap, key, vm);
        unsafe {
            match &*self.ptr.as_ptr().add(entry_index) {
                Entry::OK(_) => return true,
                _ => return false,
            }
        }
    }

    fn clear(&mut self) {
        todo!()
    }
}

impl Drop for CoreDictObject {
    fn drop(&mut self) {
        let layout = CoreDictObject::layout(self.cap);
        unsafe {
            // std::mem::needs_drop::<Entry> = false
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

    fn insert(&self, key: Data, value: Data, hash: usize, vm: &mut VM) -> Option<Data> {
        unsafe { (&mut *self.0.as_ptr()).insert(key, hash, value, vm) }
    }

    fn lookup(&self, key: &Data, hash: usize, vm: &mut VM) -> Option<Data> {
        unsafe { (&*self.0.as_ptr()).lookup(key, hash, vm) }
    }

    fn delete(&self, key: &Data, hash: usize, vm: &mut VM) -> Option<Data> {
        unsafe { (&*self.0.as_ptr()).delete(key, hash, vm) }
    }

    fn has_key(&self, key: &Data, hash: usize, vm: &mut VM) -> bool {
        unsafe { (&*self.0.as_ptr()).has_key(key, hash, vm) }
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
