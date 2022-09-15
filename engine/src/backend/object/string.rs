use std::alloc::{self, Layout};
use std::fmt::Display;
use std::marker::PhantomData;
use std::ptr;
use std::ptr::NonNull;

struct CoreStringObject {
    ptr: NonNull<u8>,
    len: usize,
    _marker: PhantomData<u8>,
}

impl CoreStringObject {
    fn len(&self) -> usize {
        self.len
    }

    fn layout(len: usize) -> Layout {
        Layout::array::<u8>(len).unwrap()
    }

    fn allocate(len: usize) -> NonNull<u8> {
        let layout = CoreStringObject::layout(len);
        assert!(layout.size() <= isize::MAX as usize, "allocation too large");
        let ptr = unsafe { alloc::alloc(layout) };
        let new_ptr = match NonNull::new(ptr as *mut u8) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        new_ptr
    }

    fn byte(&self, index: usize) -> u8 {
        assert!(index < self.len());
        unsafe { *self.ptr.as_ptr().add(index) }
    }

    fn vector(&self) -> Vec<u8> {
        let mut v: Vec<u8> = Vec::with_capacity(self.len());
        let v_ptr = v.as_mut_ptr();
        unsafe {
            ptr::copy_nonoverlapping(self.ptr.as_ptr(), v_ptr, self.len());
            v.set_len(self.len());
        };
        v
    }

    fn add(s1: &CoreStringObject, s2: &CoreStringObject) -> CoreStringObject {
        let len1 = s1.len();
        let len2 = s2.len();
        let new_ptr = CoreStringObject::allocate(len1 + len2);
        unsafe {
            ptr::copy_nonoverlapping(s1.ptr.as_ptr(), new_ptr.as_ptr(), s1.len());
            ptr::copy_nonoverlapping(s2.ptr.as_ptr(), new_ptr.as_ptr().add(s1.len()), s2.len());
        }
        CoreStringObject {
            ptr: new_ptr,
            len: len1 + len2,
            _marker: PhantomData,
        }
    }

    fn is_equal(s1: &CoreStringObject, s2: &CoreStringObject) -> bool {
        let len1 = s1.len();
        let len2 = s2.len();
        if len1 != len2 {
            return false;
        }
        unsafe {
            for i in 0..len1 {
                // TODO - use in-built ptr comparison
                if s1.byte(i) != s2.byte(i) {
                    return false;
                }
            }
        }
        return true;
    }
}

impl Display for CoreStringObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut v: Vec<u8> = Vec::with_capacity(self.len);
        let v_ptr = v.as_mut_ptr();
        let s = unsafe {
            ptr::copy_nonoverlapping(self.ptr.as_ptr(), v_ptr, self.len);
            v.set_len(self.len);
            std::str::from_utf8_unchecked(&v)
        };
        write!(f, "'{}'", s)
    }
}

impl Drop for CoreStringObject {
    fn drop(&mut self) {
        println!("{} dropping!", self);
        let layout = CoreStringObject::layout(self.len);
        unsafe {
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

#[derive(Clone)]
pub struct StringObject(NonNull<CoreStringObject>);

impl StringObject {
    pub fn new_with_bytes(bytes: &str) -> Self {
        // TODO - construct hash for the string
        let len = bytes.len();
        let bytes_arr = bytes.as_bytes();
        let bytes_arr_ptr = bytes_arr.as_ptr();
        let new_ptr = CoreStringObject::allocate(len);
        unsafe {
            ptr::copy_nonoverlapping(bytes_arr_ptr, new_ptr.as_ptr(), len);
        }
        // see `/list.rs` for reason behind using `Box` here
        let x = Box::new(CoreStringObject {
            ptr: new_ptr,
            len,
            _marker: PhantomData,
        });
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(x)) };
        StringObject(ptr)
    }

    pub fn len(&self) -> usize {
        unsafe { (&mut *self.0.as_ptr()).len() }
    }

    pub fn vector(&self) -> Vec<u8> {
        unsafe { (&*self.0.as_ptr()).vector() }
    }

    pub fn add(s1: &StringObject, s2: &StringObject) -> StringObject {
        let core_str = unsafe { CoreStringObject::add(&*s1.0.as_ptr(), &*s2.0.as_ptr()) };
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(core_str))) };
        StringObject(ptr)
    }

    pub fn is_equal(s1: &StringObject, s2: &StringObject) -> bool {
        unsafe { CoreStringObject::is_equal(&*s1.0.as_ptr(), &*s2.0.as_ptr()) }
    }

    pub fn manual_drop(&self) {
        unsafe {
            Box::from_raw(self.0.as_ptr());
        }
    }
}

impl Display for StringObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "{}", (*self.0.as_ptr()).to_string()) }
    }
}
