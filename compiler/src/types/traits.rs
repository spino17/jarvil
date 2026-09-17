//! Traits every type implements.
//!
//! [`TypeLike`] is the core of it: equality, generic instantiation, inference,
//! and interface-bound checking. [`OperatorCompatiblity`] decides what `+` and
//! friends mean for a given pair of types, and is where operator overloading
//! would attach once interfaces can express it.

use super::core::{Type, TypeStringifyContext};
use crate::constants::common::BOOL;
use crate::core::string_interner::IdentName;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::{TurbofishTypes, TypeGenericsInstantiationContext};
use crate::scope::namespace::Namespace;
use crate::scope::symbol::core::SymbolIndex;
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::core::UserDefinedTypeData;
use crate::scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory;
use crate::scope::traits::InstantiationContext;

/// What every type must be able to answer.
///
/// Implemented once per variant of [`CoreType`], with [`Type`] dispatching to
/// whichever it holds.
///
/// [`CoreType`]: super::core::CoreType
pub trait TypeLike {
    /// Whether two types are the same for assignment purposes.
    ///
    /// `Unknown` compares equal to everything, so that one type error does not
    /// cascade into a second report at every later use of the value.
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool;

    /// Equality *ignoring* names, used for lambda types where two differently
    /// named function types match if their parameters and returns do.
    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool;

    /// Substitutes concrete type arguments for generic parameters.
    ///
    /// A generic declaration is stored once; this is what produces the type as
    /// seen at a particular use site.
    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type;

    /// Unifies this type against `received_ty`, recording any inferences.
    ///
    /// This is the heart of generic inference: matching a declared parameter
    /// type against an argument's actual type, filling in
    /// `inferred_concrete_types` as generic parameters are pinned down.
    ///
    /// # Errors
    ///
    /// `Err(())` when the types cannot be unified. The caller turns that into
    /// a diagnostic, which is why no detail is carried here.
    fn try_infer_ty_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&TurbofishTypes>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()>;

    /// Whether this type satisfies every interface in `interface_bounds`.
    fn is_ty_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        namespace: &Namespace,
    ) -> bool;

    /// Renders the type as a user would write it, for diagnostics and hover.
    ///
    /// Needs a context because names are interned and symbols live in the
    /// namespace, so neither is reachable from the type alone.
    fn to_string(&self, context: TypeStringifyContext) -> String;
}

/// Shared surface of the types a user can declare: structs, enums and lambdas.
pub trait UserDefinedType {
    fn symbol_index(&self) -> SymbolIndex<UserDefinedTypeData>;
    fn concrete_types(&self) -> Option<&TurbofishTypes>;
    fn name(&self) -> IdentName;
}

/// A type parameterised by element types -- `[T]` and `{K: V}`.
///
/// Used to concretize built-in method prototypes against the receiver's element
/// types; see [`crate::types::non_struct`].
pub trait CollectionType {
    fn concrete_types(&self) -> TurbofishTypes;
}

/// Which binary operators a type supports, and what they yield.
///
/// Each method returns the result type when the operation is legal and `None`
/// when it is not, which the type checker turns into a diagnostic naming both
/// operands.
///
/// `check_not_equal`, `check_greater_equal` and `check_less_equal` are derived
/// from their counterparts by default, so implementors define only the
/// primitives.
pub trait OperatorCompatiblity {
    fn check_add(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_subtract(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_multiply(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_divide(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_double_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_greater(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_less(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_and(&self, other: &Type, namespace: &Namespace) -> Option<Type>;
    fn check_or(&self, other: &Type, namespace: &Namespace) -> Option<Type>;

    fn check_not_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        if self.check_double_equal(other, namespace).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }

    fn check_greater_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        if self.check_greater(other, namespace).is_some()
            && self.check_double_equal(other, namespace).is_some()
        {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }

    fn check_less_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        if self.check_less(other, namespace).is_some()
            && self.check_double_equal(other, namespace).is_some()
        {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }
}
