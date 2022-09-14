use crate::backend::vm::VM;

use super::string::StringObject;
use std::{fmt::Display, ptr::NonNull};

// Heap-allocated datatypes
// NOTE: All the objects are wrapped inside NonNull<T> in order to avoid automatic calling of drop.
// We need to avoid automatic calling of drop as our language does not have the concept of move and so
// when we clone a string, the raw pointer is cloned but points to the same heap memory. So when rust call
// drop on both of these pointers, the later drop will throw an error saying `drop is called on unallocated memory`!
// Also we don't have to worry about rust freeing up the memory as that task will be taken up by our garbage collector
// by calling respective `manual_drop` on objects.

#[derive(Clone)]
pub struct Object {
    pub core: CoreObject,
    pub next: Option<NonNull<Object>>,
}

#[derive(Clone)]
pub enum CoreObject {
    STRING(StringObject), // UTF-8 encoded string
}

impl Object {
    pub fn new_with_string(str_obj: StringObject, vm: &mut VM) -> Object {
        let core_object = CoreObject::STRING(str_obj);
        /*
        Object {
            core: CoreObject::STRING(str_obj),
            next: None
        }
         */
        vm.set_object(core_object)
    }

    pub fn eq_type(&self, obj: &Object) -> bool {
        match self.core {
            CoreObject::STRING(_) => match obj.core {
                CoreObject::STRING(_) => return true,
            },
        }
    }

    pub fn is_string(&self) -> bool {
        match self.core {
            CoreObject::STRING(_) => true,
        }
    }

    pub fn inner_drop(&self) {
        match &self.core {
            CoreObject::STRING(str_obj) => str_obj.manual_drop(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match &self.core {
            CoreObject::STRING(str_obj) => str_obj.to_string(),
        };
        write!(f, "{}", s)
    }
}
