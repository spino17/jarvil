use crate::backend::data::Data;
use std::alloc::{self, Layout};
use std::fmt::Display;
use std::marker::PhantomData;
use std::ptr;
use std::ptr::NonNull;

// This unsafe code is heavily taken from the `Rustonomicon` book.
// See Implementing Vec section in `https://github.com/rust-lang/nomicon` and `https://doc.rust-lang.org/nomicon/` for more information.

struct CoreListObject {
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

    fn len(&self) -> usize {
        self.len
    }

    fn cap(&self) -> usize {
        self.cap
    }

    fn grow(&mut self) {
        let (new_cap, new_layout) = if self.cap == 0 {
            (1, Layout::array::<Data>(1).unwrap())
        } else {
            let new_cap = 2 * self.cap;
            let new_layout = Layout::array::<Data>(new_cap).unwrap();
            (new_cap, new_layout)
        };
        assert!(
            new_layout.size() <= isize::MAX as usize,
            "allocation too large"
        );
        let new_ptr = if self.cap == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            let old_layout = Layout::array::<Data>(self.cap).unwrap();
            let old_ptr = self.ptr.as_ptr() as *mut u8;
            unsafe { alloc::realloc(old_ptr, old_layout, new_layout.size()) }
        };
        self.ptr = match NonNull::new(new_ptr as *mut Data) {
            Some(p) => p,
            None => alloc::handle_alloc_error(new_layout),
        };
        self.cap = new_cap;
    }

    fn push(&mut self, elem: Data) {
        if self.len == self.cap {
            self.grow();
        }
        unsafe {
            ptr::write(self.ptr.as_ptr().add(self.len()), elem);
        }
        self.len += 1;
    }
}

impl Display for CoreListObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = "[ ".to_string();
        if self.len != 0 {
            unsafe {
                s.push_str(&format!("{}", *self.ptr.as_ptr().add(0)));
                for i in 1..self.len {
                    s.push_str(&format!(", {}", *self.ptr.as_ptr().add(i)));
                }
            }
        }
        s.push_str(" ]");
        write!(f, "{}", s)
    }
}

impl Drop for CoreListObject {
    fn drop(&mut self) {
        //println!("{} dropping!", self);
        if self.cap != 0 {
            while let Some(_) = self.pop() {}
            let layout = Layout::array::<Data>(self.cap).unwrap();
            unsafe {
                alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ListObject(NonNull<CoreListObject>);

impl ListObject {
    pub fn new() -> Self {
        let x = Box::new(CoreListObject {
            ptr: NonNull::dangling(),
            len: 0,
            cap: 0,
            _marker: PhantomData,
        });
        // Below we are using `Box` instead of directly doing `let x_ptr = &mut x as *mut ManuallyDrop<CoreListObject>`
        // because ManuallyDrop<T> is not heap-allocated (it's a local stack variable) and so reference we obtain directly
        // to it would be valid only to this function. So beyond this function `ListObject` would carry a reference to unallocated memory!
        // `Box` makes sure that `ManuallyDrop<CoreListObject>` is heap-allocated and any reference to it survive even
        // beyond this function.
        let x_ptr = Box::into_raw(x);
        let ptr = unsafe { NonNull::new_unchecked(x_ptr) };
        ListObject(ptr)
    }

    pub fn push(&self, elem: Data) {
        unsafe { (&mut *self.0.as_ptr()).push(elem) }
    }

    pub fn pop(&self) -> Option<Data> {
        unsafe { (&mut *self.0.as_ptr()).pop() }
    }

    pub fn len(&self) -> usize {
        unsafe { (&*self.0.as_ptr()).len() }
    }

    pub fn cap(&self) -> usize {
        unsafe { (&*self.0.as_ptr()).cap() }
    }

    // This method will be called by the garbage collector
    pub fn manual_drop(&self) {
        unsafe {
            // We are converting back to `Box` here so that rust will propertly drop the owned structures.
            // See `https://doc.rust-lang.org/stable/std/boxed/struct.Box.html#method.into_raw` for more information.
            // We could have done this manually but it's buggy to get it right (which was with previous implementation).
            Box::from_raw(self.0.as_ptr());
        }
        // value will be dropped here!
    }
}

impl Display for ListObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "{}", (*self.0.as_ptr()).to_string()) }
    }
}
