use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use super::helper::{struct_enum_compare_fn, try_infer_types_from_tuple, StructEnumType};
use crate::core::string_interner::{Interner, StrId};
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::ConcreteTypesTuple;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::scope::{
    concrete::ConcretizationContext, core::SymbolData, interfaces::InterfaceBounds,
    types::core::UserDefinedTypeData,
};

#[derive(Debug, Clone)]
pub struct Struct {
    pub symbol_data: SymbolData<UserDefinedTypeData>,
    pub concrete_types: Option<ConcreteTypesTuple>,
}

impl Struct {
    pub fn new(
        symbol_data: &SymbolData<UserDefinedTypeData>,
        concrete_types: Option<ConcreteTypesTuple>,
    ) -> Struct {
        Struct {
            symbol_data: symbol_data.clone(),
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
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Struct(struct_data) => {
                let ty_cmp_func =
                    |ty1: &Type, ty2: &Type, _context: &ConcretizationContext| ty1.is_eq(ty2);
                struct_enum_compare_fn(
                    self,
                    struct_data,
                    ty_cmp_func,
                    &ConcretizationContext::default(),
                )
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(&self, other_ty: &Type, context: &ConcretizationContext) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Struct(struct_data) => {
                let ty_cmp_func = |ty1: &Type, ty2: &Type, context: &ConcretizationContext| {
                    ty1.is_structurally_eq(ty2, context)
                };
                struct_enum_compare_fn(self, struct_data, ty_cmp_func, context)
            }
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        match &self.concrete_types {
            Some(concrete_types) => {
                let mut concretized_concrete_types = vec![];
                for ty in concrete_types.iter() {
                    concretized_concrete_types.push(ty.concretize(context));
                }
                Type::new_with_struct(
                    &self.symbol_data,
                    Some(ConcreteTypesTuple::new(concretized_concrete_types)),
                )
            }
            None => Type::new_with_struct(&self.symbol_data, None),
        }
    }

    fn is_type_bounded_by_interfaces(&self, interface_bounds: &InterfaceBounds) -> bool {
        let symbol_data = self.symbol_data.get_core_ref();
        match &symbol_data.get_struct_data_ref().implementing_interfaces {
            Some(ty_interface_bounds) => interface_bounds.is_subset(ty_interface_bounds),
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
    ) -> Result<(), ()> {
        match received_ty.0.as_ref() {
            CoreType::Struct(struct_ty) => {
                if self.get_name() == struct_ty.get_name() {
                    match &self.concrete_types {
                        Some(generics_containing_types_tuple) => {
                            let base_types_tuple = match &struct_ty.concrete_types {
                                Some(concrete_types) => concrete_types,
                                None => unreachable!(),
                            };
                            try_infer_types_from_tuple(
                                base_types_tuple.get_core_ref(),
                                generics_containing_types_tuple.get_core_ref(),
                                inferred_concrete_types,
                                global_concrete_types,
                                num_inferred_types,
                                inference_category,
                            )
                        }
                        None => Ok(()),
                    }
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        }
    }

    fn to_string(&self, interner: &Interner) -> String {
        let mut s = interner.lookup(self.get_name()).to_string();
        match &self.concrete_types {
            Some(concrete_types) => {
                s.push('<');
                s.push_str(&concrete_types.to_string(interner));
                s.push('>');
                s
            }
            None => s,
        }
    }
}

// TODO: operator compatiblity for struct types can be defined using interfaces
// This is called `operator-overloading`
impl OperatorCompatiblity for Struct {
    fn check_add(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `Add` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_subtract(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `Subtract` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_multiply(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `Multiply` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_divide(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `Divide` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_double_equal(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `Equal` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_greater(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `Greater` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_less(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `Less` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_and(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `And` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_or(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.get_name() == other_struct.get_name() {
                    // This will be replaced with checking whether struct implements `Or` interface
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
