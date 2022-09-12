use super::string::StringObject;
use std::fmt::Display;

#[derive(Clone)]
pub enum Data {
    INT(i32),
    FLOAT(f32),
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

// Heap-allocated datatypes
pub enum Object {
    STRING(StringObject),
}
