use crate::backend::data::Data;
use std::mem::{self, MaybeUninit};

const STACK_MAX_SIZE: usize = 256;

pub struct Stack {
    stack: [Data; STACK_MAX_SIZE],
    top: usize, // change this to u8 as stack can have maximum of 256 elements in it, after which we throw stack overflow
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            stack: {
                // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
                // safe because the type we are claiming to have initialized here is a
                // bunch of `MaybeUninit`s, which do not require initialization.
                let mut x: [MaybeUninit<Data>; STACK_MAX_SIZE] =
                    unsafe { MaybeUninit::uninit().assume_init() };

                // Dropping a `MaybeUninit` does nothing. Thus using raw pointer
                // assignment instead of `ptr::write` does not cause the old
                // uninitialized value to be dropped.
                // Exception safety is not a concern because Box can't panic
                for i in 0..STACK_MAX_SIZE {
                    x[i] = MaybeUninit::new(Data::INT(0));
                }

                // Everything is initialized. Transmute the array to the
                // initialized type.
                unsafe { mem::transmute::<_, [Data; STACK_MAX_SIZE]>(x) }
            },
            top: 0,
        }
    }

    pub fn push(&mut self, value: Data) {
        self.stack[self.top] = value; // TODO - add a catch to out-of-bound index error as runtime error `stack overflow`
        self.top = self.top + 1;
    }

    pub fn pop(&mut self) -> Data {
        self.top = self.top - 1;
        self.stack[self.top].clone()
    }

    pub fn from_top(&self, depth: usize) -> &Data {
        assert!(self.top - 1 - depth >= 0);
        &self.stack[self.top - 1 - depth]
    }
}
