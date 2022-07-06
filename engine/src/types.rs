use std::rc::Rc;
use std::str;
use std::fmt::Formatter;
use crate::{constants::common::{INT, FLOAT, STRING, BOOL}, scope::FunctionData};

pub trait TypeCheck {
    fn is_eq(&self, base_type: &Type) -> bool;
    // fn get_memory_width(&self) -> usize;
}

#[derive(Debug)]
pub enum CoreType {
    ATOMIC(Atomic),
    STRUCT(Struct),
    LAMBDA(Lambda),
    // ENUMERATION,
    // TUPLES,
    // ARRAY,
    // REFERENCE,
    // GENERIC(Generic)
    NON_TYPED,
}

#[derive(Debug)]
pub struct Type(pub Rc<CoreType>);

impl Type {
    pub fn get_user_defined_type_name(&self) -> Option<Rc<String>> {
        match self.0.as_ref() {
            CoreType::STRUCT(struct_data) => Some(struct_data.name.clone()),
            CoreType::LAMBDA(lambda_data) => {
                match &lambda_data.name {
                    Some(name) => Some(name.clone()),
                    None => None,
                }
            },
            _ => None,
        }
    }

    pub fn is_atomic(&self, atomic_type_name: &str) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_type) => {
                match atomic_type_name {
                    INT       =>  atomic_type.is_int(),
                    FLOAT     =>  atomic_type.is_float(),
                    STRING    =>  atomic_type.is_string(),
                    BOOL      =>  atomic_type.is_bool(),
                    _               =>  unreachable!("atomic type name can only be 'int', 'float', 'string' or 'bool'"),
                }
            },
            _ => false
        }
    }

    pub fn get_atomic_type(atomic_type_name: &str) -> Self {
        match atomic_type_name {
            INT       =>  Type(Rc::new(CoreType::ATOMIC(Atomic::INT))),
            FLOAT     =>  Type(Rc::new(CoreType::ATOMIC(Atomic::FLOAT))),
            STRING    =>  Type(Rc::new(CoreType::ATOMIC(Atomic::STRING))),
            BOOL      =>  Type(Rc::new(CoreType::ATOMIC(Atomic::BOOL))),
            _               =>  unreachable!("atomic type name can only be 'int', 'float', 'string' or 'bool'"),
        }
    }
}

impl TypeCheck for Type {
    fn is_eq(&self, base_type: &Type) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => {
                atomic_data.is_eq(base_type)
            },
            CoreType::STRUCT(struct_data) => {
                struct_data.is_eq(base_type)
            },
            CoreType::LAMBDA(lambda_data) => {
                lambda_data.is_eq(base_type)
            },
            CoreType::NON_TYPED => {
                match base_type.0.as_ref() {
                    CoreType::NON_TYPED => true,
                    _ => false,
                }
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => write!(f, "{}", atomic_data.get_atomic_type()),
            CoreType::STRUCT(struct_data) => write!(f, "{}", struct_data.name),
            CoreType::LAMBDA(lambda_data) => {
                match &lambda_data.name {
                    Some(name) => write!(f, "{}", name),
                    None => write!(f, "lambda"),
                }
            },
            CoreType::NON_TYPED => write!(f, "non-typed"),
        }
    }
}

#[derive(Debug)]
pub enum Atomic {
    INT,
    FLOAT,
    STRING,
    BOOL,
}

impl Atomic {
    fn get_atomic_type(&self) -> &str {
        match self {
            Atomic::INT     =>  INT,
            Atomic::FLOAT   =>  FLOAT,
            Atomic::STRING  =>  STRING,
            Atomic::BOOL    =>  BOOL,
        }
    }

    fn is_int(&self) -> bool {
        match self {
            Atomic::INT => true,
            _ => false,
        }
    }

    fn is_float(&self) -> bool {
        match self {
            Atomic::FLOAT => true,
            _ => false,
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Atomic::STRING => true,
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            Atomic::BOOL => true,
            _ => false,
        }
    }
}

impl TypeCheck for Atomic {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => {
                match atomic_data {
                    Atomic::INT     =>  self.is_int(),
                    Atomic::FLOAT   =>  self.is_float(),
                    Atomic::STRING  =>  self.is_string(),
                    Atomic::BOOL    =>  self.is_bool(),
                }
            },
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct Struct {
    pub name: Rc<String>,
}

impl TypeCheck for Struct {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::STRUCT(struct_data) => {
                self.name.eq(&struct_data.name)
            },
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct Lambda {
    pub name: Option<Rc<String>>,
    pub function_data: FunctionData,
}

impl TypeCheck for Lambda {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::LAMBDA(lambda_data) => {
                let self_params_len = self.function_data.params.len();
                let base_params_len = lambda_data.function_data.params.len();
                if self_params_len != base_params_len {
                    return false
                }
                if (self.function_data.return_type.is_none() && lambda_data.function_data.return_type.is_some())
                || (self.function_data.return_type.is_some() && lambda_data.function_data.return_type.is_none()) {
                    return false
                }
                let is_return_type_eq = match self.function_data.return_type.as_ref() {
                    Some(self_return_type) => {
                        match lambda_data.function_data.return_type.as_ref() {
                            Some(base_return_type) => {
                                self_return_type.is_eq(base_return_type)
                            },
                            _ => unreachable!("both lambda types should have a return type")
                        }
                    },
                    None => {
                        match lambda_data.function_data.return_type.as_ref() {
                            None => {
                                true
                            }
                            _ => unreachable!("both lambda types should not have return type"),
                        }
                    }
                };
                if !is_return_type_eq {
                    return false
                }
                // TODO - match datatypes of params
                for index in 0..self_params_len {
                    if !self.function_data.params.as_ref()[index].1.is_eq(&lambda_data.function_data.params.as_ref()[index].1) {
                        return false
                    }
                }
                true
            },
            _ => false
        }
    }
}