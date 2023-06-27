use super::generic::Generic;
use super::hashmap::HashMap;
use super::lambda::Lambda;
use super::r#struct::Struct;
use super::tuple::Tuple;
use crate::constants::common::{ANY, BOOL, UNKNOWN, UNSET};
use crate::lexer::token::BinaryOperatorKind;
use crate::scope::concrete::ConcreteTypesRegistryKey;
use crate::scope::core::SymbolData;
use crate::scope::types::core::UserDefinedTypeData;
use crate::types::{array::Array, atomic::Atomic};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

pub trait AbstractType {
    fn is_eq(&self, base_type: &Type) -> bool;
    fn stringify(&self) -> String;
    fn has_generic_type(&self) {
        todo!()
    }
}

pub trait OperatorCompatiblity {
    fn check_add(&self, other: &Type) -> Option<Type>;
    fn check_subtract(&self, other: &Type) -> Option<Type>;
    fn check_multiply(&self, other: &Type) -> Option<Type>;
    fn check_divide(&self, other: &Type) -> Option<Type>;
    fn check_double_equal(&self, other: &Type) -> Option<Type>;
    fn check_greater(&self, other: &Type) -> Option<Type>;
    fn check_less(&self, other: &Type) -> Option<Type>;
    fn check_and(&self, other: &Type) -> Option<Type>;
    fn check_or(&self, other: &Type) -> Option<Type>;
    fn check_not_equal(&self, other: &Type) -> Option<Type> {
        if self.check_double_equal(other).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        return None;
    }
    fn check_greater_equal(&self, other: &Type) -> Option<Type> {
        if self.check_greater(other).is_some() && self.check_double_equal(other).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        return None;
    }
    fn check_less_equal(&self, other: &Type) -> Option<Type> {
        if self.check_less(other).is_some() && self.check_double_equal(other).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        return None;
    }
}

#[derive(Debug)]
pub enum CoreType {
    Atomic(Atomic),
    Struct(Struct),
    Lambda(Lambda),
    Array(Array),
    Tuple(Tuple),
    HashMap(HashMap),
    Generic(Generic),
    Unknown,
    Void,
    Unset,
    Any,
}

#[derive(Debug, Clone)]
pub struct Type(pub Rc<CoreType>, bool); // (core_type, has_generic_type)

impl Type {
    pub fn new_with_atomic(name: &str) -> Type {
        Type(Rc::new(CoreType::Atomic(Atomic::new(name))), false)
    }

    pub fn new_with_array(element_type: &Type) -> Type {
        Type(
            Rc::new(CoreType::Array(Array::new(element_type))),
            element_type.has_generics(),
        )
    }

    pub fn new_with_tuple(types: Vec<Type>) -> Type {
        let mut has_generics = false;
        let len = types.len();
        for i in 0..len {
            if types[i].has_generics() {
                has_generics = true;
                break;
            }
        }
        Type(Rc::new(CoreType::Tuple(Tuple::new(types))), has_generics)
    }

    pub fn new_with_hashmap(key_type: &Type, value_type: &Type) -> Type {
        Type(
            Rc::new(CoreType::HashMap(HashMap::new(key_type, value_type))),
            key_type.has_generics() || value_type.has_generics(),
        )
    }

    // user-defined-types
    pub fn new_with_struct(
        name: String,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        index: Option<ConcreteTypesRegistryKey>,
        has_generics: bool,
    ) -> Type {
        Type(
            Rc::new(CoreType::Struct(Struct::new(name, symbol_data, index))),
            has_generics,
        )
    }

    pub fn new_with_lambda(
        name: Option<String>,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        index: Option<ConcreteTypesRegistryKey>,
        has_generics: bool,
    ) -> Type {
        Type(
            Rc::new(CoreType::Lambda(Lambda::new(name, symbol_data, index))),
            has_generics, // change this
        )
    }

    pub fn new_with_generic(name: String, symbol_data: &SymbolData<UserDefinedTypeData>) -> Type {
        Type(
            Rc::new(CoreType::Generic(Generic::new(name, symbol_data, None))),
            true,
        )
    }

    pub fn new_with_unknown() -> Type {
        Type(Rc::new(CoreType::Unknown), false)
    }

    pub fn new_with_unset() -> Type {
        Type(Rc::new(CoreType::Unset), false)
    }

    pub fn new_with_void() -> Type {
        Type(Rc::new(CoreType::Void), false)
    }

    pub fn new_with_any() -> Type {
        Type(Rc::new(CoreType::Any), false)
    }

    pub fn is_void(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Void => true,
            _ => false,
        }
    }

    pub fn has_generics(&self) -> bool {
        self.1
    }

    pub fn is_string(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_string(),
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_int(),
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_float(),
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        if self.is_int() || self.is_float() {
            true
        } else {
            false
        }
    }

    pub fn is_lambda(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Lambda(_) => true,
            _ => false,
        }
    }

    pub fn is_hashmap(&self) -> bool {
        match self.0.as_ref() {
            CoreType::HashMap(_) => true,
            _ => false,
        }
    }

    pub fn is_immutable(&self) -> bool {
        self.is_string() || self.is_tuple()
    }

    pub fn is_tuple(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_hashable(&self) -> bool {
        // `int`, `float`, `str` and `tuple` with hashable sub_types are only hashable types
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => {
                return atomic.is_int() || atomic.is_string() || atomic.is_float()
            }
            CoreType::Tuple(tuple) => {
                for ty in &tuple.sub_types {
                    if !ty.is_hashable() {
                        return false;
                    }
                }
                return true;
            }
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Unknown => true,
            _ => false,
        }
    }

    pub fn concretize(&self) -> Vec<Type> {
        assert!(self.has_generics());
        match self.0.as_ref() {
            CoreType::Array(array_type) => {
                let element_concrete_types = array_type.element_type.concretize();
                let mut array_concrete_types = vec![];
                for ty in element_concrete_types {
                    array_concrete_types.push(Type::new_with_array(&ty));
                }
                return array_concrete_types;
            }
            CoreType::HashMap(hashmap_type) => {
                let key_ty = &hashmap_type.key_type;
                let value_ty = &hashmap_type.value_type;
                let key_concrete_types = key_ty.concretize();
                let value_concrete_types = value_ty.concretize();
                let mut hashmap_concrete_types = vec![];
                // TODO - currently key and value are separately concretized without
                // taking into accound any constraints that might exist between them.
                // for example the hashmap might be `{T: T}` which let's say T takes
                // concrete values from set [int, str, bool] then the concrete hashmap types
                // should be [{int: int}, {str: str}, {bool: bool}] but according to below logic
                // it will produce [{int: int}, {int: str}, {int: bool}, {str: int}, {str: str} ...]
                // Size of this vector is directly propotional to the code segments generated in Python code.
                // For now it is fine as mostly generic types are very rarely used in calling of the constructs.
                for key_ty in &key_concrete_types {
                    for value_ty in &value_concrete_types {
                        hashmap_concrete_types.push(Type::new_with_hashmap(key_ty, value_ty));
                    }
                }
                return hashmap_concrete_types;
            }
            CoreType::Tuple(tuple_type) => {
                fn concretize_tuple(tuple_vec: &[Type]) -> Vec<Vec<Type>> {
                    if tuple_vec.len() == 1 {
                        return vec![tuple_vec[0].concretize()];
                    }
                    let first_concrete_types = tuple_vec[0].concretize();
                    let mut remaining_concrete_types = concretize_tuple(&tuple_vec[1..]);
                    let mut tuple_sub_concrete_types = vec![];
                    for ty in first_concrete_types {
                        let mut v = vec![ty];
                        for types in &mut remaining_concrete_types {
                            v.append(types);
                        }
                        tuple_sub_concrete_types.push(v);
                    }
                    return tuple_sub_concrete_types;
                }
                let tuple_vec = &tuple_type.sub_types;
                let mut tuple_concrete_types = vec![];
                for sub_types in concretize_tuple(&tuple_vec[..]) {
                    tuple_concrete_types.push(Type::new_with_tuple(sub_types));
                }
                return tuple_concrete_types;
            }
            CoreType::Struct(struct_type) => {
                // do something similar like generic type where we maintain some state whether struct is
                // concretized (or whether it even require it) and replace internal data mutating the struct into concrete types only
                // atleast for it's own generics and not it's methods.
                todo!();
            }
            CoreType::Lambda(lambda_type) => {
                todo!()
            }
            CoreType::Generic(generic_type) => {
                let concrete_types = match &mut *generic_type
                    .semantic_data
                    .symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                {
                    UserDefinedTypeData::Generic(generic_data) => {
                        generic_data.concretize_generics();
                        generic_data.concrete_types.clone()
                    }
                    _ => unreachable!(),
                };
                concrete_types
            }
            CoreType::Atomic(_)
            | CoreType::Unknown
            | CoreType::Unset
            | CoreType::Void
            | CoreType::Any => unreachable!(),
        }
    }

    // This function returns Some if operation is possible and None otherwise
    pub fn check_operator(&self, other: &Type, op_kind: &BinaryOperatorKind) -> Option<Type> {
        match op_kind {
            BinaryOperatorKind::Add => {
                impl_op_compatiblity!(check_add, self, other)
            }
            BinaryOperatorKind::Subtract => {
                impl_op_compatiblity!(check_subtract, self, other)
            }
            BinaryOperatorKind::Multiply => {
                impl_op_compatiblity!(check_multiply, self, other)
            }
            BinaryOperatorKind::Divide => {
                impl_op_compatiblity!(check_divide, self, other)
            }
            BinaryOperatorKind::Less => {
                impl_op_compatiblity!(check_less, self, other)
            }
            BinaryOperatorKind::LessEqual => {
                impl_op_compatiblity!(check_less_equal, self, other)
            }
            BinaryOperatorKind::Greater => {
                impl_op_compatiblity!(check_greater, self, other)
            }
            BinaryOperatorKind::GreaterEqual => {
                impl_op_compatiblity!(check_greater_equal, self, other)
            }
            BinaryOperatorKind::DoubleEqual => {
                impl_op_compatiblity!(check_double_equal, self, other)
            }
            BinaryOperatorKind::NotEqual => {
                impl_op_compatiblity!(check_not_equal, self, other)
            }
            BinaryOperatorKind::And => {
                impl_op_compatiblity!(check_and, self, other)
            }
            BinaryOperatorKind::Or => {
                impl_op_compatiblity!(check_or, self, other)
            }
        }
    }
}

impl AbstractType for Type {
    fn is_eq(&self, base_type: &Type) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.is_eq(base_type),
            CoreType::Struct(struct_type) => struct_type.is_eq(base_type),
            CoreType::Lambda(lambda_type) => lambda_type.is_eq(base_type),
            CoreType::Array(array_type) => array_type.is_eq(base_type),
            CoreType::Tuple(tuple_type) => tuple_type.is_eq(base_type),
            CoreType::HashMap(hashmap_type) => hashmap_type.is_eq(base_type),
            CoreType::Generic(generic_type) => generic_type.is_eq(base_type),
            CoreType::Unknown => return false,
            CoreType::Void => match base_type.0.as_ref() {
                CoreType::Void => true,
                _ => false,
            },
            CoreType::Unset => return false,
            CoreType::Any => return true,
        }
    }

    fn stringify(&self) -> String {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.stringify(),
            CoreType::Struct(struct_type) => struct_type.stringify(),
            CoreType::Lambda(lambda_type) => lambda_type.stringify(),
            CoreType::Array(array_type) => array_type.stringify(),
            CoreType::Tuple(tuple_type) => tuple_type.stringify(),
            CoreType::HashMap(hashmap_type) => hashmap_type.stringify(),
            CoreType::Generic(generic_type) => generic_type.stringify(),
            CoreType::Unknown => unreachable!(),
            CoreType::Void => unreachable!(),
            CoreType::Unset => unreachable!(),
            CoreType::Any => unreachable!(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => write!(f, "{}", atomic_type.to_string()),
            CoreType::Struct(struct_type) => write!(f, "{}", struct_type.to_string()),
            CoreType::Lambda(lambda_type) => write!(f, "{}", lambda_type.to_string()),
            CoreType::Array(array_type) => write!(f, "{}", array_type.to_string()),
            CoreType::Tuple(tuple_type) => write!(f, "{}", tuple_type.to_string()),
            CoreType::HashMap(hashmap_type) => write!(f, "{}", hashmap_type.to_string()),
            CoreType::Generic(generic_type) => write!(f, "{}", generic_type.to_string()),
            CoreType::Unknown => write!(f, "{}", UNKNOWN),
            CoreType::Void => write!(f, "()"),
            CoreType::Unset => write!(f, "{}", UNSET),
            CoreType::Any => write!(f, "{}", ANY),
        }
    }
}
