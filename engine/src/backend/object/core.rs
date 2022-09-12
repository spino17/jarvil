use super::string::StringObject;
use std::{alloc::Layout, fmt::Display, mem::ManuallyDrop, ptr::NonNull};

#[derive(Clone)]
pub enum Data {
    INT(i32),
    FLOAT(f64),
    OBJ(Object),
    BOOL(bool),
}
impl Data {
    pub fn eq_type(&self, data: &Data) -> bool {
        match self {
            Data::INT(_) => match data {
                Data::INT(_) => return true,
                _ => return false,
            },
            Data::FLOAT(_) => match data {
                Data::FLOAT(_) => return true,
                _ => return false,
            },
            Data::BOOL(_) => match data {
                Data::BOOL(_) => return true,
                _ => return false,
            },
            Data::OBJ(obj_1) => match data {
                Data::OBJ(obj_2) => obj_1.eq_type(obj_2),
                _ => return false,
            },
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Data::INT(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Data::FLOAT(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Data::BOOL(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Data::OBJ(obj) => obj.is_string(),
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            Data::INT(_) | Data::FLOAT(_) => true,
            _ => false,
        }
    }
}
impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::INT(val) => write!(f, "{}", val),
            Data::FLOAT(val) => write!(f, "{}", val),
            Data::OBJ(val) => write!(f, "{}", val),
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
// NOTE: All the objects are wrapped inside ManuallyDrop in order to avoid automatic calling of drop.
// We need to avoid automatic calling of drop as our language does not have the concept of move and so
// when we clone a string, the raw pointer is cloned which points to the same heap memory. So when rust call
// drop on both of these pointers, the later drop will throw an error saying `drop is called on unallocated memory`!
// Also we don't have to worry about rust freeing up the memory as that task will be taken up by our garbage collector.
#[derive(Clone)]
pub enum Object {
    STRING(ManuallyDrop<StringObject>),
}

impl Object {
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
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Object::STRING(str_obj) => str_obj.to_string(),
        };
        write!(f, "{}", s)
    }
}
