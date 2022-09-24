use super::{function::FunctionObject, list::ListObject, string::StringObject};
use crate::{backend::vm::VM, error::constants::CASTING_OBJECT_ERROR_MSG};
use std::{borrow::Borrow, cell::RefCell, fmt::Display, ptr::NonNull, rc::Rc};

// Heap-allocated datatypes
// NOTE: All the objects are wrapped inside NonNull<T> in order to avoid automatic calling of drop.
// We need to avoid automatic calling of drop as our language does not have the concept of move and so
// when we clone a string, the raw pointer is cloned but points to the same heap memory. So when rust call
// drop on both of these pointers, the later drop will throw an error saying `drop is called on unallocated memory`!
// Also we don't have to worry about rust freeing up the memory as that task will be taken up by our garbage collector
// by calling respective `manual_drop` on objects.

// This trackes all the heap-allocated objects during the whole course of program compilation as well as runtime.
#[derive(Debug)]
pub struct CoreObjectTracker {
    objects: NonNull<Object>,
    len: usize,
}

impl CoreObjectTracker {
    fn set_objects(&mut self, objects: NonNull<Object>, len: usize) {
        self.objects = objects;
        self.len = len
    }
}

impl Display for CoreObjectTracker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = "".to_string();
        if self.len != 0 {
            unsafe {
                let mut next = Some(self.objects.clone());
                let mut flag = false;
                while let Some(ptr) = next {
                    next = (*ptr.as_ptr()).next;
                    if flag {
                        s.push_str(" -> ");
                    }
                    s.push_str(&(*ptr.as_ptr()).to_string());
                    flag = true;
                }
            }
        }
        write!(f, "{}", s)
    }
}

impl Drop for CoreObjectTracker {
    fn drop(&mut self) {
        if self.len != 0 {
            unsafe {
                let mut next = Some(self.objects.clone());
                while let Some(ptr) = next {
                    next = (*ptr.as_ptr()).next;
                    (&*ptr.as_ptr()).inner_drop(); // free the underlying heap-allocated memory
                    let _x = Box::from_raw(ptr.as_ptr()); // then free the pointer to the memory
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ObjectTracker(Rc<RefCell<CoreObjectTracker>>);

impl Default for ObjectTracker {
    fn default() -> Self {
        ObjectTracker(Rc::new(RefCell::new(CoreObjectTracker {
            objects: NonNull::dangling(),
            len: 0,
        })))
    }
}

impl ObjectTracker {
    pub fn set_objects(&self, objects: NonNull<Object>, len: usize) {
        self.0.as_ref().borrow_mut().set_objects(objects, len);
    }

    pub fn len(&self) -> usize {
        self.0.as_ref().borrow().len
    }

    pub fn objects(&self) -> NonNull<Object> {
        self.0.as_ref().borrow().objects.clone()
    }

    pub fn add_object(&self, core_object: CoreObject) -> Object {
        let obj = if self.len() == 0 {
            Object {
                core: core_object,
                next: None,
            }
        } else {
            let ptr = self.objects();
            Object {
                core: core_object,
                next: Some(ptr),
            }
        };
        let new_ptr = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(obj.clone()))) };
        let new_len = self.len() + 1;
        self.set_objects(new_ptr, new_len);
        obj
    }
}

impl Display for ObjectTracker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ref().borrow().to_string())
    }
}

#[derive(Clone, Debug)]
pub struct Object {
    pub core: CoreObject,
    pub next: Option<NonNull<Object>>,
}

#[derive(Clone, Debug)]
pub enum CoreObject {
    STRING(StringObject),     // UTF-8 encoded string
    LIST(ListObject),         // dynamic arrays
    FUNCTION(FunctionObject), // function objects
}

impl Object {
    pub fn new_with_string(str_obj: StringObject, tracker: &mut ObjectTracker) -> Object {
        let core_object = CoreObject::STRING(str_obj);
        tracker.add_object(core_object)
    }

    pub fn new_with_list(list_obj: ListObject, tracker: &mut ObjectTracker) -> Object {
        let core_object = CoreObject::LIST(list_obj);
        tracker.add_object(core_object)
    }

    pub fn new_with_function(func_obj: FunctionObject, tracker: &mut ObjectTracker) -> Object {
        let core_object = CoreObject::FUNCTION(func_obj);
        tracker.add_object(core_object)
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
            CoreObject::FUNCTION(_) => match obj.core {
                CoreObject::FUNCTION(_) => true,
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

    pub fn is_list(&self) -> bool {
        match self.core {
            CoreObject::LIST(_) => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self.core {
            CoreObject::FUNCTION(_) => true,
            _ => false,
        }
    }

    // NOTE: Below casting functions panics instead of safe returning Option<...> to have runtime performance.
    // If there is a panic that means there is a bug in type-checker!
    pub fn as_string(&self) -> StringObject {
        match &self.core {
            CoreObject::STRING(str_obj) => str_obj.clone(),
            _ => panic!("{}", CASTING_OBJECT_ERROR_MSG),
        }
    }

    pub fn as_list(&self) -> ListObject {
        match &self.core {
            CoreObject::LIST(list_obj) => list_obj.clone(),
            _ => panic!("{}", CASTING_OBJECT_ERROR_MSG),
        }
    }

    pub fn as_function(&self) -> FunctionObject {
        match &self.core {
            CoreObject::FUNCTION(func_obj) => func_obj.clone(),
            _ => panic!("{}", CASTING_OBJECT_ERROR_MSG),
        }
    }

    pub fn inner_drop(&self) {
        match &self.core {
            CoreObject::STRING(str_obj) => str_obj.manual_drop(),
            CoreObject::LIST(list_obj) => list_obj.manual_drop(),
            CoreObject::FUNCTION(func_obj) => func_obj.manual_drop(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match &self.core {
            CoreObject::STRING(str_obj) => str_obj.to_string(),
            CoreObject::LIST(list_obj) => list_obj.to_string(),
            CoreObject::FUNCTION(func_obj) => func_obj.to_string(),
        };
        write!(f, "{}", s)
    }
}
