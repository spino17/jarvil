use super::{list::ListObject, string::StringObject};
use crate::backend::vm::VM;
use std::{fmt::Display, ptr::NonNull};

// Heap-allocated datatypes
// NOTE: All the objects are wrapped inside NonNull<T> in order to avoid automatic calling of drop.
// We need to avoid automatic calling of drop as our language does not have the concept of move and so
// when we clone a string, the raw pointer is cloned but points to the same heap memory. So when rust call
// drop on both of these pointers, the later drop will throw an error saying `drop is called on unallocated memory`!
// Also we don't have to worry about rust freeing up the memory as that task will be taken up by our garbage collector
// by calling respective `manual_drop` on objects.

#[derive(Clone, Debug)]
pub struct Object {
    pub core: CoreObject,
    pub next: Option<NonNull<Object>>,
}

#[derive(Clone, Debug)]
pub enum CoreObject {
    STRING(StringObject), // UTF-8 encoded string
    LIST(ListObject),     // dynamic arrays
}

impl Object {
    pub fn new_with_string(str_obj: StringObject, vm: &mut VM) -> Object {
        let core_object = CoreObject::STRING(str_obj);
        vm.set_object(core_object)
    }

    pub fn new_with_list(list_obj: ListObject, vm: &mut VM) -> Object {
        let core_object = CoreObject::LIST(list_obj);
        vm.set_object(core_object)
    }

    pub fn string(&self) -> Option<StringObject> {
        match &self.core {
            CoreObject::STRING(str_obj) => Some(str_obj.clone()),
            _ => None,
        }
    }

    pub fn list(&self) -> Option<ListObject> {
        match &self.core {
            CoreObject::LIST(list_obj) => Some(list_obj.clone()),
            _ => None,
        }
    }

    pub fn eq_type(&self, obj: &Object) -> bool {
        match self.core {
            CoreObject::STRING(_) => match obj.core {
                CoreObject::STRING(_) => true,
                _ => false,
            },
            CoreObject::LIST(_) => match obj.core {
                CoreObject::LIST(_) => true,
                _ => false,
            },
        }
    }

    pub fn is_string(&self) -> bool {
        match self.core {
            CoreObject::STRING(_) => true,
            _ => false,
        }
    }

    pub fn inner_drop(&self) {
        match &self.core {
            CoreObject::STRING(str_obj) => str_obj.manual_drop(),
            CoreObject::LIST(list_obj) => list_obj.manual_drop(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match &self.core {
            CoreObject::STRING(str_obj) => str_obj.to_string(),
            CoreObject::LIST(list_obj) => list_obj.to_string(),
        };
        write!(f, "{}", s)
    }
}
