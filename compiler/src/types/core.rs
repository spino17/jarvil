use super::generic::Generic;
use super::hashmap::core::HashMap;
use super::lambda::Lambda;
use super::r#enum::Enum;
use super::r#struct::Struct;
use super::traits::TypeLike;
use super::tuple::Tuple;
use crate::constants::common::{UNKNOWN, UNSET};
use crate::core::string_interner::Interner;
use crate::lexer::token::BinaryOperatorKind;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::{TurbofishTypes, TypeGenericsInstantiationContext};
use crate::scope::namespace::Namespace;
use crate::scope::symbol::core::SymbolIndex;
use crate::scope::symbol::function::CallablePrototypeData;
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::core::UserDefinedTypeData;
use crate::scope::symbol::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::scope::traits::InstantiationContext;
use crate::types::traits::OperatorCompatiblity;
use crate::types::{array::core::Array, atomic::Atomic};
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug)]
pub enum CoreType {
    Atomic(Atomic),
    Array(Array),
    Tuple(Tuple),
    HashMap(HashMap),
    Struct(Struct),
    Enum(Enum),
    Lambda(Lambda),
    Generic(Generic),
    Unknown,
    Void,
    Unset,
}

#[derive(Debug, Clone)]
pub struct Type(Rc<CoreType>);

impl Type {
    pub fn core_ty(&self) -> &CoreType {
        self.0.as_ref()
    }

    pub fn new_with_atomic(name: &str) -> Type {
        Type(Rc::new(CoreType::Atomic(Atomic::new(name))))
    }

    pub fn new_with_array(element_type: Type) -> Type {
        Type(Rc::new(CoreType::Array(Array::new(element_type))))
    }

    pub fn new_with_tuple(types: Vec<Type>) -> Type {
        Type(Rc::new(CoreType::Tuple(Tuple::new(types))))
    }

    pub fn new_with_hashmap(key_type: Type, value_type: Type) -> Type {
        Type(Rc::new(CoreType::HashMap(HashMap::new(
            key_type, value_type,
        ))))
    }

    // user-defined-types
    pub fn new_with_struct(
        symbol_index: SymbolIndex<UserDefinedTypeData>,
        concrete_types: Option<TurbofishTypes>,
    ) -> Type {
        Type(Rc::new(CoreType::Struct(Struct::new(
            symbol_index,
            concrete_types,
        ))))
    }

    pub fn new_with_enum(
        symbol_index: SymbolIndex<UserDefinedTypeData>,
        concrete_types: Option<TurbofishTypes>,
    ) -> Type {
        Type(Rc::new(CoreType::Enum(Enum::new(
            symbol_index,
            concrete_types,
        ))))
    }

    pub fn new_with_lambda_named(
        symbol_index: SymbolIndex<UserDefinedTypeData>,
        concrete_types: Option<TurbofishTypes>,
    ) -> Type {
        Type(Rc::new(CoreType::Lambda(Lambda::new_with_named(
            symbol_index,
            concrete_types,
        ))))
    }

    pub fn new_with_lambda_unnamed(function_prototype: CallablePrototypeData) -> Type {
        Type(Rc::new(CoreType::Lambda(Lambda::new_with_unnamed(
            function_prototype,
        ))))
    }

    pub fn new_with_generic(symbol_index: SymbolIndex<UserDefinedTypeData>) -> Type {
        Type(Rc::new(CoreType::Generic(Generic::new(symbol_index))))
    }

    pub fn new_with_unknown() -> Type {
        Type(Rc::new(CoreType::Unknown))
    }

    pub fn new_with_unset() -> Type {
        Type(Rc::new(CoreType::Unset))
    }

    pub fn new_with_void() -> Type {
        Type(Rc::new(CoreType::Void))
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

    pub fn is_bool(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_bool(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_string(),
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self.0.as_ref(), CoreType::Array(_))
    }

    pub fn is_hashmap(&self) -> bool {
        matches!(self.0.as_ref(), CoreType::HashMap(_))
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self.0.as_ref(), CoreType::Tuple(_))
    }

    pub fn is_enum(&self) -> bool {
        matches!(self.0.as_ref(), CoreType::Enum(_))
    }

    pub fn is_lambda(&self) -> bool {
        matches!(self.0.as_ref(), CoreType::Lambda(_))
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self.0.as_ref(), CoreType::Unknown)
    }

    pub fn is_unset(&self) -> bool {
        matches!(self.0.as_ref(), CoreType::Unset)
    }

    pub fn is_void(&self) -> bool {
        matches!(self.0.as_ref(), CoreType::Void)
    }

    pub fn is_numeric(&self) -> bool {
        self.is_int() || self.is_float()
    }

    pub fn is_immutable(&self) -> bool {
        self.is_string() || self.is_tuple()
    }

    pub fn is_hashable(&self) -> bool {
        // `int`, `float`, `str` and `tuple` with hashable sub_types are only hashable types
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_int() || atomic.is_string() || atomic.is_float(),
            CoreType::Tuple(tuple) => {
                for ty in tuple.sub_types() {
                    if !ty.is_hashable() {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }

    // This function returns Some if operation is possible and None otherwise
    pub fn check_operator(
        &self,
        other: &Type,
        op_kind: &BinaryOperatorKind,
        namespace: &Namespace,
    ) -> Option<Type> {
        match op_kind {
            BinaryOperatorKind::Add => {
                impl_op_compatiblity!(namespace, check_add, self, other)
            }
            BinaryOperatorKind::Subtract => {
                impl_op_compatiblity!(namespace, check_subtract, self, other)
            }
            BinaryOperatorKind::Multiply => {
                impl_op_compatiblity!(namespace, check_multiply, self, other)
            }
            BinaryOperatorKind::Divide => {
                impl_op_compatiblity!(namespace, check_divide, self, other)
            }
            BinaryOperatorKind::Less => {
                impl_op_compatiblity!(namespace, check_less, self, other)
            }
            BinaryOperatorKind::LessEqual => {
                impl_op_compatiblity!(namespace, check_less_equal, self, other)
            }
            BinaryOperatorKind::Greater => {
                impl_op_compatiblity!(namespace, check_greater, self, other)
            }
            BinaryOperatorKind::GreaterEqual => {
                impl_op_compatiblity!(namespace, check_greater_equal, self, other)
            }
            BinaryOperatorKind::DoubleEqual => {
                impl_op_compatiblity!(namespace, check_double_equal, self, other)
            }
            BinaryOperatorKind::NotEqual => {
                impl_op_compatiblity!(namespace, check_not_equal, self, other)
            }
            BinaryOperatorKind::And => {
                impl_op_compatiblity!(namespace, check_and, self, other)
            }
            BinaryOperatorKind::Or => {
                impl_op_compatiblity!(namespace, check_or, self, other)
            }
        }
    }
}

impl TypeLike for Type {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.is_eq(other_ty, namespace),
            CoreType::Struct(struct_type) => struct_type.is_eq(other_ty, namespace),
            CoreType::Lambda(lambda_type) => lambda_type.is_eq(other_ty, namespace),
            CoreType::Array(array_type) => array_type.is_eq(other_ty, namespace),
            CoreType::Tuple(tuple_type) => tuple_type.is_eq(other_ty, namespace),
            CoreType::HashMap(hashmap_type) => hashmap_type.is_eq(other_ty, namespace),
            CoreType::Generic(generic_type) => generic_type.is_eq(other_ty, namespace),
            CoreType::Enum(enum_type) => enum_type.is_eq(other_ty, namespace),
            CoreType::Void => match other_ty.0.as_ref() {
                CoreType::Void => true,
                _ => false,
            },
            CoreType::Unknown => false,
            CoreType::Unset => false,
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => {
                atomic_type.is_structurally_eq(other_ty, context, namespace)
            }
            CoreType::Struct(struct_type) => {
                struct_type.is_structurally_eq(other_ty, context, namespace)
            }
            CoreType::Enum(enum_type) => enum_type.is_structurally_eq(other_ty, context, namespace),
            CoreType::Lambda(lambda_type) => {
                lambda_type.is_structurally_eq(other_ty, context, namespace)
            }
            CoreType::Array(array_type) => {
                array_type.is_structurally_eq(other_ty, context, namespace)
            }
            CoreType::Tuple(tuple_type) => {
                tuple_type.is_structurally_eq(other_ty, context, namespace)
            }
            CoreType::HashMap(hashmap_type) => {
                hashmap_type.is_structurally_eq(other_ty, context, namespace)
            }
            CoreType::Generic(generic_type) => {
                generic_type.is_structurally_eq(other_ty, context, namespace)
            }
            CoreType::Void => match other_ty.0.as_ref() {
                CoreType::Void => true,
                _ => false,
            },
            CoreType::Unknown | CoreType::Unset => unreachable!(),
        }
    }

    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type {
        if context.is_empty() {
            return self.clone();
        }
        match self.0.as_ref() {
            CoreType::Struct(struct_type) => struct_type.concretize(context, namespace),
            CoreType::Enum(enum_type) => enum_type.concretize(context, namespace),
            CoreType::Lambda(lambda_type) => lambda_type.concretize(context, namespace),
            CoreType::Array(array_type) => array_type.concretize(context, namespace),
            CoreType::Tuple(tuple_type) => tuple_type.concretize(context, namespace),
            CoreType::HashMap(hashmap_type) => hashmap_type.concretize(context, namespace),
            CoreType::Generic(generic_type) => generic_type.concretize(context, namespace),
            CoreType::Atomic(_) | CoreType::Unknown | CoreType::Void | CoreType::Unset => {
                self.clone()
            }
        }
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        namespace: &Namespace,
    ) -> bool {
        if interface_bounds.len() == 0 {
            return true;
        }
        match self.0.as_ref() {
            CoreType::Struct(struct_ty) => {
                struct_ty.is_type_bounded_by_interfaces(interface_bounds, namespace)
            }
            CoreType::Generic(generic_ty) => {
                generic_ty.is_type_bounded_by_interfaces(interface_bounds, namespace)
            }
            CoreType::Array(array_ty) => {
                array_ty.is_type_bounded_by_interfaces(interface_bounds, namespace)
            }
            CoreType::HashMap(hashmap_ty) => {
                hashmap_ty.is_type_bounded_by_interfaces(interface_bounds, namespace)
            }
            CoreType::Tuple(tuple_ty) => {
                tuple_ty.is_type_bounded_by_interfaces(interface_bounds, namespace)
            }
            CoreType::Lambda(_) | CoreType::Atomic(_) | CoreType::Enum(_) => false,
            CoreType::Unknown | CoreType::Unset | CoreType::Void => false,
        }
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&TurbofishTypes>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        match self.0.as_ref() {
            CoreType::Struct(struct_ty) => struct_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            ),
            CoreType::Enum(enum_ty) => enum_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            ),
            CoreType::Lambda(lambda_ty) => lambda_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            ),
            CoreType::Array(array_ty) => array_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            ),
            CoreType::Tuple(tuple_ty) => tuple_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            ),
            CoreType::HashMap(hashmap_ty) => hashmap_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            ),
            CoreType::Generic(generic_ty) => generic_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                namespace,
            ),
            CoreType::Atomic(_) | CoreType::Unknown | CoreType::Void | CoreType::Unset => {
                if !self.is_eq(received_ty, namespace) {
                    return Err(());
                }
                Ok(())
            }
        }
    }

    fn to_string(&self, interner: &Interner, namespace: &Namespace) -> String {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.to_string(interner, namespace),
            CoreType::Struct(struct_type) => struct_type.to_string(interner, namespace),
            CoreType::Enum(enum_type) => enum_type.to_string(interner, namespace),
            CoreType::Lambda(lambda_type) => lambda_type.to_string(interner, namespace),
            CoreType::Array(array_type) => array_type.to_string(interner, namespace),
            CoreType::Tuple(tuple_type) => tuple_type.to_string(interner, namespace),
            CoreType::HashMap(hashmap_type) => hashmap_type.to_string(interner, namespace),
            CoreType::Generic(generic_type) => generic_type.to_string(interner, namespace),
            CoreType::Unknown => UNKNOWN.to_string(),
            CoreType::Void => "()".to_string(),
            CoreType::Unset => UNSET.to_string(),
        }
    }
}
