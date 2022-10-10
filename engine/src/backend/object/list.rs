use crate::backend::data::Data;
use crate::backend::operators::eval_binary_op;
use crate::backend::vm::VM;
use crate::lexer::token::BinaryOperatorKind;
use std::alloc::{self, Layout};
use std::fmt::Display;
use std::marker::PhantomData;
use std::ops::Add;
use std::ptr;
use std::ptr::NonNull;

use super::core::ObjectTracker;

// This unsafe code is heavily taken from the `Rustonomicon` book.
// See Implementing Vec section in `https://github.com/rust-lang/nomicon` and `https://doc.rust-lang.org/nomicon/` for more information.

struct CoreListObject {
    ptr: NonNull<Data>,
    len: usize,
    cap: usize,
    _marker: PhantomData<Data>,
}

impl CoreListObject {
    fn new() -> Self {
        CoreListObject {
            ptr: NonNull::dangling(),
            len: 0,
            cap: 0,
            _marker: PhantomData,
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

    fn pop(&mut self) -> Option<Data> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe { Some(ptr::read(self.ptr.as_ptr().add(self.len))) }
        }
    }

    fn add(l1: &CoreListObject, l2: &CoreListObject) -> CoreListObject {
        let len1 = l1.len();
        let len2 = l2.len();
        if len1 == 0 && len2 == 0 {
            return CoreListObject::new();
        }
        let cap = 2 * (len1 + len2);
        let layout = Layout::array::<Data>(cap).unwrap();
        assert!(layout.size() <= isize::MAX as usize, "allocation too large");
        let ptr = unsafe { alloc::alloc(layout) };
        let new_ptr = match NonNull::new(ptr as *mut Data) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        unsafe {
            ptr::copy_nonoverlapping(l1.ptr.as_ptr(), new_ptr.as_ptr(), l1.len());
            ptr::copy_nonoverlapping(l2.ptr.as_ptr(), new_ptr.as_ptr().add(l1.len()), l2.len());
        }
        CoreListObject {
            ptr: new_ptr,
            len: len1 + len2,
            cap,
            _marker: PhantomData,
        }
    }

    fn is_equal(l1: &CoreListObject, l2: &CoreListObject, tracker: &mut ObjectTracker) -> bool {
        let len1 = l1.len();
        let len2 = l2.len();
        if len1 != len2 {
            return false;
        }
        unsafe {
            for i in 0..len1 {
                if !eval_binary_op(
                    (&*l1.ptr.as_ptr().add(i)).clone(),
                    (&*l2.ptr.as_ptr().add(i)).clone(),
                    BinaryOperatorKind::DoubleEqual,
                    tracker,
                )
                .as_bool()
                {
                    return false;
                }
            }
        }
        return true;
    }

    fn clear(&mut self) {
        todo!()
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
        println!("{} dropping!", self);
        if self.cap != 0 {
            // while let Some(_) = self.pop() {}  // std::mem::needs_drop::<Data> = false
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
        let x = Box::new(CoreListObject::new());
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

    pub fn add(l1: &ListObject, l2: &ListObject) -> ListObject {
        let core_str = unsafe { CoreListObject::add(&*l1.0.as_ptr(), &*l2.0.as_ptr()) };
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(core_str))) };
        ListObject(ptr)
    }

    pub fn is_equal(l1: &ListObject, l2: &ListObject, tracker: &mut ObjectTracker) -> bool {
        unsafe { CoreListObject::is_equal(&*l1.0.as_ptr(), &*l2.0.as_ptr(), tracker) }
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

impl Add for ListObject {
    type Output = ListObject;
    fn add(self, rhs: Self) -> Self::Output {
        ListObject::add(&self, &rhs)
    }
}
