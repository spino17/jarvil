use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use super::helper::{struct_enum_compare_fn, try_infer_types_from_tuple, StructEnumType};
use crate::core::string_interner::{Interner, StrId};
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::ConcreteTypesTuple;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::core::SymbolIndex;
use crate::scope::symbol::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::scope::{
    concrete::ConcretizationContext, symbol::interfaces::InterfaceBounds,
    symbol::types::core::UserDefinedTypeData,
};

#[derive(Debug)]
pub struct Struct {
    pub symbol_data: SymbolIndex<UserDefinedTypeData>,
    pub concrete_types: Option<ConcreteTypesTuple>,
}

impl Struct {
    pub fn new(
        symbol_data: SymbolIndex<UserDefinedTypeData>,
        concrete_types: Option<ConcreteTypesTuple>,
    ) -> Struct {
        Struct {
            symbol_data,
            concrete_types,
        }
    }
}

impl StructEnumType for Struct {
    fn get_concrete_types(&self) -> Option<&ConcreteTypesTuple> {
        self.concrete_types.as_ref()
    }

    fn get_name(&self) -> StrId {
        self.symbol_data.identifier_name()
    }
}

impl AbstractType for Struct {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Struct(struct_data) => {
                let ty_cmp_func =
                    |ty1: &Type,
                     ty2: &Type,
                     _context: &ConcretizationContext,
                     namespace: &Namespace| { ty1.is_eq(ty2, namespace) };
                struct_enum_compare_fn(
                    self,
                    struct_data,
                    ty_cmp_func,
                    &ConcretizationContext::default(),
                    namespace,
                )
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: &ConcretizationContext,
        namespace: &Namespace,
    ) -> bool {
        let CoreType::Struct(struct_data) = other_ty.0.as_ref() else {
            return false;
        };
        let ty_cmp_func =
            |ty1: &Type, ty2: &Type, context: &ConcretizationContext, namespace: &Namespace| {
                ty1.is_structurally_eq(ty2, context, namespace)
            };
        struct_enum_compare_fn(self, struct_data, ty_cmp_func, context, namespace)
    }

    fn concretize(&self, context: &ConcretizationContext, namespace: &Namespace) -> Type {
        let Some(concrete_types) = &self.concrete_types else {
            return Type::new_with_struct(self.symbol_data, None);
        };
        let mut concretized_concrete_types = vec![];
        for ty in concrete_types.iter() {
            concretized_concrete_types.push(ty.concretize(context, namespace));
        }
        Type::new_with_struct(
            self.symbol_data,
            Some(ConcreteTypesTuple::new(concretized_concrete_types)),
        )
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        namespace: &Namespace,
    ) -> bool {
        let symbol_data = &namespace.types.get_symbol_data_ref(self.symbol_data).data;
        match &symbol_data.get_struct_data_ref().implementing_interfaces {
            Some(ty_interface_bounds) => interface_bounds.is_subset(ty_interface_bounds, namespace),
            None => false,
        }
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let CoreType::Struct(struct_ty) = received_ty.0.as_ref() else {
            return Err(());
        };
        if self.get_name() != struct_ty.get_name() {
            return Err(());
        }
        let Some(generics_containing_types_tuple) = &self.concrete_types else {
            return Ok(());
        };
        let Some(base_types_tuple) = &struct_ty.concrete_types else {
            unreachable!()
        };
        try_infer_types_from_tuple(
            base_types_tuple.get_core_ref(),
            generics_containing_types_tuple.get_core_ref(),
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )
    }

    fn to_string(&self, interner: &Interner, namespace: &Namespace) -> String {
        let mut s = interner.lookup(self.get_name());
        let Some(concrete_types) = &self.concrete_types else {
            return s;
        };
        s.push('<');
        s.push_str(&concrete_types.to_string(interner, namespace));
        s.push('>');
        s
    }
}

// TODO: operator compatiblity for struct types can be defined using interfaces
// This is called `operator-overloading`
impl OperatorCompatiblity for Struct {
    fn check_add(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `Add` interface
        None
    }

    fn check_subtract(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `Subtract` interface
        None
    }

    fn check_multiply(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `Multiply` interface
        None
    }

    fn check_divide(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `Divide` interface
        None
    }

    fn check_double_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `DoubleEqual` interface
        None
    }

    fn check_greater(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `Greater` interface
        None
    }

    fn check_less(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `Less` interface
        None
    }

    fn check_and(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `And` interface
        None
    }

    fn check_or(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        let CoreType::Struct(other_struct) = other.0.as_ref() else {
            return None;
        };
        if self.get_name() != other_struct.get_name() {
            return None;
        }
        // TODO - This will be replaced with checking whether struct implements `Or` interface
        None
    }
}
