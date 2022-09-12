use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ptr;
use std::ptr::NonNull;

pub struct StringObject {
    ptr: NonNull<u8>,
    len: usize,
    _marker: PhantomData<u8>,
}

impl StringObject {
    fn new() -> Self {
        StringObject {
            ptr: NonNull::dangling(),
            len: 0,
            _marker: PhantomData,
        }
    }

    fn new_with_bytes(bytes: String) -> Self {
        let len = bytes.len();
        if len == 0 {
            return StringObject::new();
        }
        let bytes_arr = bytes.as_bytes();
        let layout = Layout::array::<u8>(len).unwrap();
        assert!(layout.size() <= isize::MAX as usize, "Allocation too large");
        let ptr = unsafe { alloc::alloc(layout) };
        let new_ptr = match NonNull::new(ptr as *mut u8) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        unsafe {
            for i in 0..len {
                ptr::write(new_ptr.as_ptr().add(i), bytes_arr[i]);
            }
        }
        StringObject {
            ptr: new_ptr,
            len,
            _marker: PhantomData,
        }
    }
}

impl Drop for StringObject {
    fn drop(&mut self) {
        let layout = Layout::array::<u8>(self.len).unwrap();
        unsafe {
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}
