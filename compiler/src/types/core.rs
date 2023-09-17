use super::generic::Generic;
use super::hashmap::core::HashMap;
use super::lambda::Lambda;
use super::r#struct::Struct;
use super::tuple::Tuple;
use crate::constants::common::{ANY, BOOL, UNKNOWN, UNSET};
use crate::lexer::token::BinaryOperatorKind;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::core::{ConcreteTypesRegistryKey, ConcretizationContext};
use crate::scope::core::SymbolData;
use crate::scope::function::{CallableData, CallablePrototypeData, PrototypeConcretizationResult};
use crate::scope::handler::{SemanticStateDatabase, SymbolDataRegistryTable};
use crate::scope::interfaces::{InterfaceBounds, InterfaceData};
use crate::scope::types::core::UserDefinedTypeData;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::{array::core::Array, atomic::Atomic};
use std::collections::HashMap as StdHashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

pub trait AbstractType {
    fn is_eq(
        &self,
        other_ty: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> bool;
    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
        context: &ConcretizationContext,
    ) -> bool;
    fn concretize(
        &self,
        context: &ConcretizationContext,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Type;
    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&Vec<Type>>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Result<(), ()>;
    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        semantic_state_db: &mut SemanticStateDatabase,
    ) -> bool;
    fn to_string(&self, semantic_state_db: &SemanticStateDatabase) -> String;
}

pub trait AbstractNonStructTypes {
    fn get_concrete_types(&self) -> Vec<Type>;
    fn get_builtin_methods(&self) -> Rc<StdHashMap<&'static str, CallableData>>;
    fn try_method(
        &self,
        method_name: &str,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<CallablePrototypeData> {
        let builtin_methods = self.get_builtin_methods();
        match builtin_methods.get(method_name) {
            Some(callable_data) => {
                let concrete_types = self.get_concrete_types();
                match callable_data.prototype.concretize_prototype(
                    Some(&concrete_types),
                    None,
                    registry,
                ) {
                    PrototypeConcretizationResult::UnConcretized(_) => unreachable!(),
                    PrototypeConcretizationResult::Concretized(prototype) => {
                        return Some(prototype)
                    }
                }
            }
            None => return None,
        }
    }
}

pub trait OperatorCompatiblity {
    fn check_add(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_subtract(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_multiply(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_divide(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_double_equal(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_greater(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_less(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_and(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_or(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type>;
    fn check_not_equal(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        if self.check_double_equal(other, registry).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        return None;
    }
    fn check_greater_equal(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        if self.check_greater(other, registry).is_some()
            && self.check_double_equal(other, registry).is_some()
        {
            return Some(Type::new_with_atomic(BOOL));
        }
        return None;
    }
    fn check_less_equal(
        &self,
        other: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        if self.check_less(other, registry).is_some()
            && self.check_double_equal(other, registry).is_some()
        {
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
pub struct Type(pub Rc<CoreType>, bool);

impl Type {
    pub fn new_with_atomic(name: &str) -> Type {
        Type(Rc::new(CoreType::Atomic(Atomic::new(name))), false)
    }

    pub fn new_with_array(element_type: Type) -> Type {
        Type(Rc::new(CoreType::Array(Array::new(element_type))), false)
    }

    pub fn new_with_tuple(types: Vec<Type>) -> Type {
        Type(Rc::new(CoreType::Tuple(Tuple::new(types))), false)
    }

    pub fn new_with_hashmap(key_type: Type, value_type: Type) -> Type {
        Type(
            Rc::new(CoreType::HashMap(HashMap::new(key_type, value_type))),
            false,
        )
    }

    // user-defined-types
    pub fn new_with_struct(
        symbol_data: &SymbolData<UserDefinedTypeData>,
        index: Option<ConcreteTypesRegistryKey>,
    ) -> Type {
        Type(
            Rc::new(CoreType::Struct(Struct::new(symbol_data, index))),
            false,
        )
    }

    pub fn new_with_lambda_named(
        symbol_data: &SymbolData<UserDefinedTypeData>,
        index: Option<ConcreteTypesRegistryKey>,
    ) -> Type {
        Type(
            Rc::new(CoreType::Lambda(Lambda::new_with_named(symbol_data, index))),
            false,
        )
    }

    pub fn new_with_lambda_unnamed(function_prototype: CallablePrototypeData) -> Type {
        Type(
            Rc::new(CoreType::Lambda(Lambda::new_with_unnamed(
                function_prototype,
            ))),
            false,
        )
    }

    pub fn new_with_generic(symbol_data: &SymbolData<UserDefinedTypeData>) -> Type {
        Type(Rc::new(CoreType::Generic(Generic::new(symbol_data))), false)
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

    pub fn is_unset(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Unset => true,
            _ => false,
        }
    }

    pub fn set_concretization_required_flag(&mut self) {
        self.1 = true;
    }

    pub fn is_concretization_required(&self) -> bool {
        self.1
    }

    // This function returns Some if operation is possible and None otherwise
    pub fn check_operator(
        &self,
        other: &Type,
        op_kind: &BinaryOperatorKind,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Option<Type> {
        match op_kind {
            BinaryOperatorKind::Add => {
                impl_op_compatiblity!(check_add, self, other, registry)
            }
            BinaryOperatorKind::Subtract => {
                impl_op_compatiblity!(check_subtract, self, other, registry)
            }
            BinaryOperatorKind::Multiply => {
                impl_op_compatiblity!(check_multiply, self, other, registry)
            }
            BinaryOperatorKind::Divide => {
                impl_op_compatiblity!(check_divide, self, other, registry)
            }
            BinaryOperatorKind::Less => {
                impl_op_compatiblity!(check_less, self, other, registry)
            }
            BinaryOperatorKind::LessEqual => {
                impl_op_compatiblity!(check_less_equal, self, other, registry)
            }
            BinaryOperatorKind::Greater => {
                impl_op_compatiblity!(check_greater, self, other, registry)
            }
            BinaryOperatorKind::GreaterEqual => {
                impl_op_compatiblity!(check_greater_equal, self, other, registry)
            }
            BinaryOperatorKind::DoubleEqual => {
                impl_op_compatiblity!(check_double_equal, self, other, registry)
            }
            BinaryOperatorKind::NotEqual => {
                impl_op_compatiblity!(check_not_equal, self, other, registry)
            }
            BinaryOperatorKind::And => {
                impl_op_compatiblity!(check_and, self, other, registry)
            }
            BinaryOperatorKind::Or => {
                impl_op_compatiblity!(check_or, self, other, registry)
            }
        }
    }
}

impl AbstractType for Type {
    fn is_eq(
        &self,
        other_ty: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.is_eq(other_ty, registry),
            CoreType::Struct(struct_type) => struct_type.is_eq(other_ty, registry),
            CoreType::Lambda(lambda_type) => lambda_type.is_eq(other_ty, registry),
            CoreType::Array(array_type) => array_type.is_eq(other_ty, registry),
            CoreType::Tuple(tuple_type) => tuple_type.is_eq(other_ty, registry),
            CoreType::HashMap(hashmap_type) => hashmap_type.is_eq(other_ty, registry),
            CoreType::Generic(generic_type) => generic_type.is_eq(other_ty, registry),
            CoreType::Void => match other_ty.0.as_ref() {
                CoreType::Void => true,
                _ => false,
            },
            CoreType::Unknown => return false,
            CoreType::Unset => return false,
            CoreType::Any => return true,
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
        context: &ConcretizationContext,
    ) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => {
                atomic_type.is_structurally_eq(other_ty, registry, context)
            }
            CoreType::Struct(struct_type) => {
                struct_type.is_structurally_eq(other_ty, registry, context)
            }
            CoreType::Lambda(lambda_type) => {
                lambda_type.is_structurally_eq(other_ty, registry, context)
            }
            CoreType::Array(array_type) => {
                array_type.is_structurally_eq(other_ty, registry, context)
            }
            CoreType::Tuple(tuple_type) => {
                tuple_type.is_structurally_eq(other_ty, registry, context)
            }
            CoreType::HashMap(hashmap_type) => {
                hashmap_type.is_structurally_eq(other_ty, registry, context)
            }
            CoreType::Generic(generic_type) => {
                generic_type.is_structurally_eq(other_ty, registry, context)
            }
            CoreType::Void => match other_ty.0.as_ref() {
                CoreType::Void => true,
                _ => false,
            },
            CoreType::Unknown | CoreType::Unset | CoreType::Any => unreachable!(),
        }
    }

    fn concretize(
        &self,
        context: &ConcretizationContext,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Type {
        match self.0.as_ref() {
            CoreType::Struct(struct_type) => struct_type.concretize(context, registry),
            CoreType::Lambda(lambda_type) => lambda_type.concretize(context, registry),
            CoreType::Array(array_type) => array_type.concretize(context, registry),
            CoreType::Tuple(tuple_type) => tuple_type.concretize(context, registry),
            CoreType::HashMap(hashmap_type) => hashmap_type.concretize(context, registry),
            CoreType::Generic(generic_type) => generic_type.concretize(context, registry),
            CoreType::Atomic(_)
            | CoreType::Unknown
            | CoreType::Void
            | CoreType::Unset
            | CoreType::Any => self.clone(),
        }
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        semantic_state_db: &mut SemanticStateDatabase,
    ) -> bool {
        if interface_bounds.len() == 0 {
            return true;
        }
        match self.0.as_ref() {
            CoreType::Struct(struct_ty) => {
                struct_ty.is_type_bounded_by_interfaces(interface_bounds, semantic_state_db)
            }
            CoreType::Generic(generic_ty) => {
                generic_ty.is_type_bounded_by_interfaces(interface_bounds, semantic_state_db)
            }
            CoreType::Array(array_ty) => {
                array_ty.is_type_bounded_by_interfaces(interface_bounds, semantic_state_db)
            }
            CoreType::HashMap(hashmap_ty) => {
                hashmap_ty.is_type_bounded_by_interfaces(interface_bounds, semantic_state_db)
            }
            CoreType::Tuple(tuple_ty) => {
                tuple_ty.is_type_bounded_by_interfaces(interface_bounds, semantic_state_db)
            }
            CoreType::Lambda(_) | CoreType::Atomic(_) | CoreType::Any => false,
            CoreType::Unknown | CoreType::Unset | CoreType::Void => false,
        }
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&Vec<Type>>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> Result<(), ()> {
        match self.0.as_ref() {
            CoreType::Struct(struct_ty) => struct_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                registry,
            ),
            CoreType::Lambda(lambda_ty) => lambda_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                registry,
            ),
            CoreType::Array(array_ty) => array_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                registry,
            ),
            CoreType::Tuple(tuple_ty) => tuple_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                registry,
            ),
            CoreType::HashMap(hashmap_ty) => hashmap_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                registry,
            ),
            CoreType::Generic(generic_ty) => generic_ty.try_infer_type_or_check_equivalence(
                received_ty,
                inferred_concrete_types,
                global_concrete_types,
                num_inferred_types,
                inference_category,
                registry,
            ),
            CoreType::Atomic(_)
            | CoreType::Unknown
            | CoreType::Void
            | CoreType::Unset
            | CoreType::Any => {
                if !self.is_eq(received_ty, registry) {
                    return Err(());
                }
                Ok(())
            }
        }
    }

    fn to_string(&self, semantic_state_db: &SemanticStateDatabase) -> String {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.to_string(semantic_state_db),
            CoreType::Struct(struct_type) => struct_type.to_string(semantic_state_db),
            CoreType::Lambda(lambda_type) => lambda_type.to_string(semantic_state_db),
            CoreType::Array(array_type) => array_type.to_string(semantic_state_db),
            CoreType::Tuple(tuple_type) => tuple_type.to_string(semantic_state_db),
            CoreType::HashMap(hashmap_type) => hashmap_type.to_string(semantic_state_db),
            CoreType::Generic(generic_type) => generic_type.to_string(semantic_state_db),
            CoreType::Unknown => UNKNOWN.to_string(),
            CoreType::Void => "()".to_string(),
            CoreType::Unset => UNSET.to_string(),
            CoreType::Any => ANY.to_string(),
        }
    }
}
