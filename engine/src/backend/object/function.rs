use crate::backend::chunk::Chunk;
use std::{fmt::Display, ptr::NonNull};

struct CoreFunctionObject {
    code: Chunk,
    arity: u8, // number of parameters of a function would always be less than 255
    name: String,
}

#[derive(Clone, Debug)]
pub struct FunctionObject(NonNull<CoreFunctionObject>);

impl FunctionObject {
    fn new(code: Chunk, arity: u8, name: String) -> Self {
        let ptr = unsafe {
            NonNull::new_unchecked(Box::into_raw(Box::new(CoreFunctionObject {
                code,
                arity,
                name,
            })))
        };
        FunctionObject(ptr)
    }

    pub fn manual_drop(&self) {
        unsafe {
            Box::from_raw(self.0.as_ptr());
        }
    }
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
