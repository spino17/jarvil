use std::alloc::{self, Layout};
use std::fmt::Display;
use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ptr;
use std::ptr::NonNull;

#[derive(Clone)]
pub struct StringObject(ManuallyDrop<CoreStringObject>);

#[derive(Clone)]
pub struct CoreStringObject {
    ptr: NonNull<u8>,
    len: usize,
    _marker: PhantomData<u8>,
}

impl StringObject {
    pub fn new_with_bytes(bytes: &str) -> Self {
        // TODO - construct hash for the string
        let len = bytes.len();
        let bytes_arr = bytes.as_bytes();
        let bytes_arr_ptr = bytes_arr.as_ptr();
        let new_ptr = StringObject::allocate(len);
        unsafe {
            ptr::copy_nonoverlapping(bytes_arr_ptr, new_ptr.as_ptr(), len);
        }
        StringObject(ManuallyDrop::new(CoreStringObject {
            ptr: new_ptr,
            len,
            _marker: PhantomData,
        }))
    }

    fn layout(len: usize) -> Layout {
        Layout::array::<u8>(len).unwrap()
    }

    fn allocate(len: usize) -> NonNull<u8> {
        let layout = StringObject::layout(len);
        assert!(layout.size() <= isize::MAX as usize, "allocation too large");
        let ptr = unsafe { alloc::alloc(layout) };
        let new_ptr = match NonNull::new(ptr as *mut u8) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        new_ptr
    }

    pub fn len(&self) -> usize {
        self.0.len
    }

    fn byte(&self, index: usize) -> u8 {
        assert!(index < self.len());
        unsafe { *self.0.ptr.as_ptr().add(index) }
    }

    pub fn vector(&self) -> Vec<u8> {
        let mut v: Vec<u8> = Vec::with_capacity(self.len());
        let v_ptr = v.as_mut_ptr();
        unsafe {
            ptr::copy_nonoverlapping(self.0.ptr.as_ptr(), v_ptr, self.len());
            v.set_len(self.len());
        };
        v
    }

    pub fn add(s1: &StringObject, s2: &StringObject) -> StringObject {
        let len1 = s1.len();
        let len2 = s2.len();
        let new_ptr = StringObject::allocate(len1 + len2);
        unsafe {
            ptr::copy_nonoverlapping(s1.0.ptr.as_ptr(), new_ptr.as_ptr(), s1.len());
            ptr::copy_nonoverlapping(s2.0.ptr.as_ptr(), new_ptr.as_ptr().add(s1.len()), s2.len());
        }
        StringObject(ManuallyDrop::new(CoreStringObject {
            ptr: new_ptr,
            len: len1 + len2,
            _marker: PhantomData,
        }))
    }

    pub fn is_equal(s1: &StringObject, s2: &StringObject) -> bool {
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

    // This method will be called by the garbage collector
    pub fn manual_drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.0) }
    }
}

impl Drop for CoreStringObject {
    fn drop(&mut self) {
        let layout = StringObject::layout(self.len);
        unsafe {
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

impl Display for StringObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut v: Vec<u8> = Vec::with_capacity(self.len());
        let v_ptr = v.as_mut_ptr();
        let s = unsafe {
            ptr::copy_nonoverlapping(self.0.ptr.as_ptr(), v_ptr, self.len());
            v.set_len(self.len());
            std::str::from_utf8_unchecked(&v)
        };
        write!(f, "{}", s)
    }
}
