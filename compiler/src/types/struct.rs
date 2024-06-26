use super::core::{CoreType, Type, TypeStringifyContext};
use super::helper::{try_infer_types_from_tuple, user_defined_ty_compare_fn};
use super::traits::{OperatorCompatiblity, TypeLike, UserDefinedType};
use crate::core::string_interner::IdentName;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::{TurbofishTypes, TypeGenericsInstantiationContext};
use crate::scope::namespace::Namespace;
use crate::scope::symbol::core::SymbolIndex;
use crate::scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory;
use crate::scope::traits::InstantiationContext;
use crate::scope::{symbol::interfaces::InterfaceBounds, symbol::types::core::UserDefinedTypeData};

#[derive(Debug)]
pub struct Struct {
    symbol_index: SymbolIndex<UserDefinedTypeData>,
    concrete_types: Option<TurbofishTypes>,
}

impl Struct {
    pub fn new(
        symbol_index: SymbolIndex<UserDefinedTypeData>,
        concrete_types: Option<TurbofishTypes>,
    ) -> Struct {
        Struct {
            symbol_index,
            concrete_types,
        }
    }
}

impl UserDefinedType for Struct {
    fn symbol_index(&self) -> SymbolIndex<UserDefinedTypeData> {
        self.symbol_index
    }

    fn concrete_types(&self) -> Option<&TurbofishTypes> {
        self.concrete_types.as_ref()
    }

    fn name(&self) -> IdentName {
        self.symbol_index.identifier_name()
    }
}

impl TypeLike for Struct {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        let CoreType::Struct(struct_data) = other_ty.core_ty() else {
            return false;
        };
        let ty_cmp_func =
            |ty1: &Type,
             ty2: &Type,
             _context: TypeGenericsInstantiationContext,
             namespace: &Namespace| { ty1.is_eq(ty2, namespace) };

        user_defined_ty_compare_fn(
            self,
            struct_data,
            ty_cmp_func,
            TypeGenericsInstantiationContext::default(),
            namespace,
        )
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        let CoreType::Struct(struct_data) = other_ty.core_ty() else {
            return false;
        };
        let ty_cmp_func =
            |ty1: &Type,
             ty2: &Type,
             context: TypeGenericsInstantiationContext,
             namespace: &Namespace| { ty1.is_structurally_eq(ty2, context, namespace) };

        user_defined_ty_compare_fn(self, struct_data, ty_cmp_func, context, namespace)
    }

    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type {
        let Some(concrete_types) = &self.concrete_types else {
            return Type::new_with_struct(self.symbol_index, None);
        };
        let mut concretized_concrete_types = vec![];

        for ty in concrete_types.iter() {
            concretized_concrete_types.push(ty.concretize(context, namespace));
        }

        Type::new_with_struct(
            self.symbol_index,
            Some(TurbofishTypes::new(concretized_concrete_types)),
        )
    }

    fn is_ty_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        namespace: &Namespace,
    ) -> bool {
        let ty_data = namespace
            .types_ref()
            .symbol_ref(self.symbol_index)
            .data_ref();

        match ty_data.struct_data_ref().implementing_interfaces() {
            Some(ty_interface_bounds) => interface_bounds.is_subset(ty_interface_bounds, namespace),
            None => false,
        }
    }

    fn try_infer_ty_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&TurbofishTypes>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let CoreType::Struct(struct_ty) = received_ty.core_ty() else {
            return Err(());
        };

        if self.name() != struct_ty.name() {
            return Err(());
        }

        let Some(generics_containing_types_tuple) = &self.concrete_types else {
            return Ok(());
        };
        let Some(base_types_tuple) = &struct_ty.concrete_types else {
            unreachable!()
        };

        try_infer_types_from_tuple(
            base_types_tuple.core_ref(),
            generics_containing_types_tuple.core_ref(),
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )
    }

    fn to_string(&self, context: TypeStringifyContext) -> String {
        let mut s = context.interner().lookup(self.name());
        let Some(concrete_types) = &self.concrete_types else {
            return s;
        };

        s.push('<');
        s.push_str(&concrete_types.to_string(context));
        s.push('>');
        s
    }
}

// TODO: operator compatiblity for struct types can be defined using interfaces
// This is called `operator-overloading`
impl OperatorCompatiblity for Struct {
    fn check_add(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `Add` interface
        None
    }

    fn check_subtract(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `Subtract` interface
        None
    }

    fn check_multiply(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `Multiply` interface
        None
    }

    fn check_divide(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `Divide` interface
        None
    }

    fn check_double_equal(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `DoubleEqual` interface
        None
    }

    fn check_greater(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `Greater` interface
        None
    }

    fn check_less(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `Less` interface
        None
    }

    fn check_and(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `And` interface
        None
    }

    fn check_or(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.core_ty() else {
            return None;
        };

        if self.name() != other_struct.name() {
            return None;
        }

        // TODO - This will be replaced with checking whether struct implements `Or` interface
        None
    }
}
