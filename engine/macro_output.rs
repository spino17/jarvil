pub mod core {
    use super::lambda::Lambda;
    use super::r#struct::Struct;
    use crate::constants::common::{NON_TYPED, UNKNOWN};
    use crate::lexer::token::BinaryOperatorKind;
    use crate::scope::core::SymbolData;
    use crate::scope::user_defined_types::UserDefinedTypeData;
    use crate::types::{array::Array, atomic::Atomic};
    use std::fmt::{Debug, Formatter};
    use std::rc::Rc;
    pub trait AbstractType {
        fn is_eq(&self, base_type: &Type) -> bool;
    }
    pub trait OperatorCompatiblity {
        fn check_add(&self, other: &Type) -> Option<Type>;
        fn check_subtract(&self, other: &Type) -> Option<Type>;
        fn check_multiply(&self, other: &Type) -> Option<Type>;
        fn check_divide(&self, other: &Type) -> Option<Type>;
        fn check_double_equal(&self, other: &Type) -> Option<()>;
        fn check_not_equal(&self, other: &Type) -> Option<()>;
        fn check_greater(&self, other: &Type) -> Option<()>;
        fn check_greater_equal(&self, other: &Type) -> Option<()>;
        fn check_less(&self, other: &Type) -> Option<()>;
        fn check_less_equal(&self, other: &Type) -> Option<()>;
    }
    pub enum CoreType {
        ATOMIC(Atomic),
        STRUCT(Struct),
        LAMBDA(Lambda),
        ARRAY(Array),
        NON_TYPED,
        UNKNOWN,
        VOID,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreType {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreType::ATOMIC(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOMIC", &__self_0)
                }
                CoreType::STRUCT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "STRUCT", &__self_0)
                }
                CoreType::LAMBDA(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LAMBDA", &__self_0)
                }
                CoreType::ARRAY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ARRAY", &__self_0)
                }
                CoreType::NON_TYPED => ::core::fmt::Formatter::write_str(f, "NON_TYPED"),
                CoreType::UNKNOWN => ::core::fmt::Formatter::write_str(f, "UNKNOWN"),
                CoreType::VOID => ::core::fmt::Formatter::write_str(f, "VOID"),
            }
        }
    }
    pub struct Type(pub Rc<CoreType>);
    #[automatically_derived]
    impl ::core::fmt::Debug for Type {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Type", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Type {
        #[inline]
        fn clone(&self) -> Type {
            Type(::core::clone::Clone::clone(&self.0))
        }
    }
    impl Type {
        pub fn new_with_atomic(name: &str) -> Type {
            Type(Rc::new(CoreType::ATOMIC(Atomic::new(name))))
        }
        pub fn new_with_struct(
            name: String,
            symbol_data: &SymbolData<UserDefinedTypeData>,
        ) -> Type {
            Type(Rc::new(CoreType::STRUCT(Struct::new(name, symbol_data))))
        }
        pub fn new_with_lambda(
            name: Option<String>,
            symbol_data: &SymbolData<UserDefinedTypeData>,
        ) -> Type {
            Type(Rc::new(CoreType::LAMBDA(Lambda::new(name, symbol_data))))
        }
        pub fn new_with_array(element_type: &Type) -> Type {
            Type(Rc::new(CoreType::ARRAY(Array::new(element_type))))
        }
        pub fn new_with_unknown() -> Type {
            Type(Rc::new(CoreType::UNKNOWN))
        }
        pub fn new_with_void() -> Type {
            Type(Rc::new(CoreType::VOID))
        }
        pub fn is_void(&self) -> bool {
            match self.0.as_ref() {
                CoreType::VOID => true,
                _ => false,
            }
        }
        pub fn is_string(&self) -> bool {
            match self.0.as_ref() {
                CoreType::ATOMIC(atomic) => atomic.is_string(),
                _ => false,
            }
        }
        pub fn is_array(&self) -> bool {
            match self.0.as_ref() {
                CoreType::ARRAY(_) => true,
                _ => false,
            }
        }
        pub fn is_bool(&self) -> bool {
            match self.0.as_ref() {
                CoreType::ATOMIC(atomic) => atomic.is_bool(),
                _ => false,
            }
        }
        pub fn is_int(&self) -> bool {
            match self.0.as_ref() {
                CoreType::ATOMIC(atomic) => atomic.is_int(),
                _ => false,
            }
        }
        pub fn is_float(&self) -> bool {
            match self.0.as_ref() {
                CoreType::ATOMIC(atomic) => atomic.is_float(),
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
                CoreType::LAMBDA(_) => true,
                _ => false,
            }
        }
        pub fn is_unknown(&self) -> bool {
            match self.0.as_ref() {
                CoreType::UNKNOWN => true,
                _ => false,
            }
        }
        pub fn check_operator(&self, other: &Type, op_kind: BinaryOperatorKind) -> Option<Type> {
            match op_kind {
                BinaryOperatorKind::Add => match self.0.as_ref() {
                    CoreType::ATOMIC(atomic_type) => atomic_type.check_add(other),
                    CoreType::ARRAY(array_type) => array_type.check_add(other),
                    CoreType::STRUCT(struct_type) => struct_type.check_add(other),
                    CoreType::LAMBDA(lambda_type) => lambda_type.check_add(other),
                    CoreType::UNKNOWN => return None,
                    CoreType::NON_TYPED => return None,
                    CoreType::VOID => return None,
                },
                BinaryOperatorKind::Subtract => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::Multiply => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::Divide => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::Less => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::LessEqual => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::Greater => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::GreaterEqual => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::DoubleEqual => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::NotEqual => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::And => ::core::panicking::panic("not yet implemented"),
                BinaryOperatorKind::Or => ::core::panicking::panic("not yet implemented"),
            }
        }
    }
    impl AbstractType for Type {
        fn is_eq(&self, base_type: &Type) -> bool {
            match self.0.as_ref() {
                CoreType::ATOMIC(atomic_type) => atomic_type.is_eq(base_type),
                CoreType::STRUCT(struct_type) => struct_type.is_eq(base_type),
                CoreType::LAMBDA(lambda_type) => lambda_type.is_eq(base_type),
                CoreType::ARRAY(array_type) => array_type.is_eq(base_type),
                CoreType::UNKNOWN => match base_type.0.as_ref() {
                    CoreType::UNKNOWN => true,
                    _ => false,
                },
                CoreType::NON_TYPED => match base_type.0.as_ref() {
                    CoreType::NON_TYPED => true,
                    _ => false,
                },
                CoreType::VOID => match base_type.0.as_ref() {
                    CoreType::VOID => true,
                    _ => false,
                },
            }
        }
    }
    impl std::fmt::Display for Type {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
            match self.0.as_ref() {
                CoreType::ATOMIC(atomic_type) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(
                        &atomic_type.to_string(),
                    )],
                )),
                CoreType::STRUCT(struct_type) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(
                        &struct_type.to_string(),
                    )],
                )),
                CoreType::LAMBDA(lambda_type) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(
                        &lambda_type.to_string(),
                    )],
                )),
                CoreType::ARRAY(array_type) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(
                        &array_type.to_string(),
                    )],
                )),
                CoreType::UNKNOWN => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(&UNKNOWN)],
                )),
                CoreType::NON_TYPED => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(&NON_TYPED)],
                )),
                CoreType::VOID => f.write_fmt(::core::fmt::Arguments::new_v1(&["()"], &[])),
            }
        }
    }
    impl OperatorCompatiblity for Type {
        fn check_add(&self, other: &Type) -> Option<Type> {
            match self.0.as_ref() {
                CoreType::ATOMIC(atomic_type) => ::core::panicking::panic("not yet implemented"),
                CoreType::ARRAY(array_type) => ::core::panicking::panic("not yet implemented"),
                CoreType::STRUCT(struct_type) => ::core::panicking::panic("not yet implemented"),
                CoreType::LAMBDA(lambda_type) => ::core::panicking::panic("not yet implemented"),
                CoreType::UNKNOWN => ::core::panicking::panic("not yet implemented"),
                CoreType::NON_TYPED => ::core::panicking::panic("not yet implemented"),
                CoreType::VOID => ::core::panicking::panic("not yet implemented"),
            }
        }
        fn check_subtract(&self, other: &Type) -> Option<Type> {
            ::core::panicking::panic("not yet implemented")
        }
        fn check_multiply(&self, other: &Type) -> Option<Type> {
            ::core::panicking::panic("not yet implemented")
        }
        fn check_divide(&self, other: &Type) -> Option<Type> {
            ::core::panicking::panic("not yet implemented")
        }
        fn check_not_equal(&self, other: &Type) -> Option<()> {
            ::core::panicking::panic("not yet implemented")
        }
        fn check_double_equal(&self, other: &Type) -> Option<()> {
            ::core::panicking::panic("not yet implemented")
        }
        fn check_greater(&self, other: &Type) -> Option<()> {
            ::core::panicking::panic("not yet implemented")
        }
        fn check_greater_equal(&self, other: &Type) -> Option<()> {
            ::core::panicking::panic("not yet implemented")
        }
        fn check_less(&self, other: &Type) -> Option<()> {
            ::core::panicking::panic("not yet implemented")
        }
        fn check_less_equal(&self, other: &Type) -> Option<()> {
            ::core::panicking::panic("not yet implemented")
        }
    }
}
