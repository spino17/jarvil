use super::string::StringObject;
use std::{fmt::Display, ptr::NonNull, alloc::Layout};

#[derive(Clone)]
pub enum Data {
    INT(i32),
    FLOAT(f64),
    // OBJ(Rc<RefCell<Object>>),
    BOOL(bool),
}
impl Data {}
impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::INT(val) => write!(f, "{}", val),
            Data::FLOAT(val) => write!(f, "{}", val),
            // Data::LITERAL(val) => write!(f, "{}", val),
            Data::BOOL(val) => write!(f, "{}", val),
        }
    }
}

pub trait HeapObject {
    type Data;
    fn layout(len: usize) -> Layout;
    fn allocate(len: usize) -> NonNull<Self::Data>;
}

// Heap-allocated datatypes
pub enum Object {
    STRING(StringObject),
}
