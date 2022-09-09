use super::string::StringObject;
use std::fmt::Display;

#[derive(Clone)]
pub enum Data {
    INT(i32),
    FLOAT(f32),
    // LITERAL(Rc<String>),
    // OBJ(Rc<RefCell<Object>>),  // TODO - need a raw pointer to heap allocated objects Raw<T>
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

pub struct Raw<T> {
    ref_ptr: *mut T,
}

pub enum Object {
    // Heap-allocated data
    STRING(StringObject),
}
