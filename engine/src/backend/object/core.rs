use super::string::StringObject;
use std::{alloc::Layout, fmt::Display, mem::ManuallyDrop, ptr::NonNull};

// Heap-allocated datatypes
// NOTE: All the objects are wrapped inside ManuallyDrop<T> in order to avoid automatic calling of drop.
// We need to avoid automatic calling of drop as our language does not have the concept of move and so
// when we clone a string, the raw pointer is cloned but points to the same heap memory. So when rust call
// drop on both of these pointers, the later drop will throw an error saying `drop is called on unallocated memory`!
// Also we don't have to worry about rust freeing up the memory as that task will be taken up by our garbage collector.
#[derive(Clone)]
pub enum Object {
    STRING(StringObject), // UTF-8 encoded string
}

impl Object {
    pub fn new_with_string(bytes: &str) -> Object {
        Object::STRING(StringObject::new_with_bytes(bytes))
    }

    pub fn eq_type(&self, obj: &Object) -> bool {
        match self {
            Object::STRING(_) => match obj {
                Object::STRING(_) => return true,
            },
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Object::STRING(_) => true,
        }
    }

    pub fn manual_drop(&self) {
        match self {
            Object::STRING(str_obj) => str_obj.manual_drop(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Object::STRING(str_obj) => str_obj.to_string(),
        };
        write!(f, "{}", s)
    }
}
