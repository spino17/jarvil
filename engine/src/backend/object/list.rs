use super::data::Data;
use std::alloc::{self, Layout};
use std::fmt::Display;
use std::marker::PhantomData;
use std::ptr;
use std::{mem::ManuallyDrop, ptr::NonNull};

// This unsafe code is heavily taken from the `Rustonomicon` book.
// See Implementing Vec section in `https://github.com/rust-lang/nomicon` and `https://doc.rust-lang.org/nomicon/` for more information.

#[derive(Clone)]
pub struct CoreListObject {
    ptr: NonNull<Data>,
    len: usize,
    cap: usize,
    _marker: PhantomData<Data>,
}

impl CoreListObject {
    fn pop(&mut self) -> Option<Data> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe { Some(ptr::read(self.ptr.as_ptr().add(self.len))) }
        }
    }
}

#[derive(Clone)]
pub struct ListObject(ManuallyDrop<CoreListObject>);

impl ListObject {
    pub fn new() -> Self {
        ListObject(ManuallyDrop::new(CoreListObject{
            ptr: NonNull::dangling(),
            len: 0,
            cap: 0,
            _marker: PhantomData,
        }))
    }

    pub fn layout(len: usize) -> Layout {
        Layout::array::<Data>(len).unwrap()
    }

    fn grow(&mut self) {
        let (new_cap, new_layout) = if self.0.cap == 0 {
            (1, Layout::array::<Data>(1).unwrap())
        } else {
            let new_cap = 2 * self.0.cap;
            let new_layout = Layout::array::<Data>(new_cap).unwrap();
            (new_cap, new_layout)
        };
        assert!(
            new_layout.size() <= isize::MAX as usize,
            "allocation too large"
        );
        let new_ptr = if self.0.cap == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            let old_layout = Layout::array::<Data>(self.0.cap).unwrap();
            let old_ptr = self.0.ptr.as_ptr() as *mut u8;
            unsafe { alloc::realloc(old_ptr, old_layout, new_layout.size()) }
        };
        self.0.ptr = match NonNull::new(new_ptr as *mut Data) {
            Some(p) => p,
            None => alloc::handle_alloc_error(new_layout),
        };
        self.0.cap = new_cap;
    }

    pub fn push(&mut self, elem: Data) {
        if self.0.len == self.0.cap {
            println!("capacity full!");
            self.grow();
        }
        unsafe {
            ptr::write(self.0.ptr.as_ptr().add(self.len()), elem);
        }
        self.0.len += 1;
    }

    pub fn pop(&mut self) -> Option<Data> {
        self.0.pop()
    }

    fn len(&self) -> usize {
        self.0.len
    }

    fn cap(&self) -> usize {
        self.0.cap
    }

    fn manual_drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.0) }
    }
}

impl Drop for CoreListObject {
    fn drop(&mut self) {
        if self.cap != 0 {
            while let Some(_) = self.pop() {}
            let layout = Layout::array::<Data>(self.cap).unwrap();
            unsafe {
                alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
            }
        }
    }
}

impl Display for ListObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = "[".to_string();
        unsafe {
            s.push_str(&format!("{}", *self.0.ptr.as_ptr().add(0)));
            for i in 1..self.0.len {
                s.push_str(&format!(", {}", *self.0.ptr.as_ptr().add(i)));
            }
        }
        s.push_str("]");
        write!(f, "{}", s)
    }
}
