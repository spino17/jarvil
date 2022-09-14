use std::alloc::{self, Layout};

pub struct Allocator {
    // TODO - need to turn these values atomic to make below functions thread-safe
    total_allocated_size: usize,
    mallocs: usize,
    frees: usize,
    reallocs: usize,
}

impl Allocator {
    pub const fn new() -> Self {
        Allocator {
            total_allocated_size: 0,
            mallocs: 0,
            frees: 0,
            reallocs: 0,
        }
    }

    pub unsafe fn allocate(&mut self, layout: Layout) -> *mut u8 {
        self.total_allocated_size = self.total_allocated_size + layout.size();
        self.mallocs = self.mallocs + 1;
        unsafe { alloc::alloc(layout) }
    }

    pub unsafe fn free(&mut self, ptr: *mut u8, layout: Layout) {
        self.total_allocated_size = self.total_allocated_size - layout.size();
        self.frees = self.frees + 1;
        unsafe { alloc::dealloc(ptr, layout) }
    }

    pub unsafe fn realloc(&mut self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        self.total_allocated_size = self.total_allocated_size - layout.size() + new_size;
        self.reallocs = self.reallocs + 1;
        unsafe { alloc::realloc(ptr, layout, new_size) }
    }
}
