use crate::scope::namespace::Namespace;
use crate::scope::symbol::types::generic_type::GenericTypeParams;
use crate::scope::traits::IsInitialized;
use crate::types::core::Type;
use crate::{
    core::string_interner::StrId,
    scope::concrete::{ConcreteTypesTuple, ConcretizationContext},
    types::traits::TypeLike,
};
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct EnumTypeData {
    variants: Vec<(StrId, Option<Type>, TextRange)>,
    generics: Option<GenericTypeParams>,
    is_init: bool,
}

impl EnumTypeData {
    pub fn set_meta_data(&mut self, variants: Vec<(StrId, Option<Type>, TextRange)>) {
        self.variants = variants;
    }

    pub fn set_generics(&mut self, generics_spec: Option<GenericTypeParams>) {
        self.generics = generics_spec;
        self.is_init = true;
    }

    pub fn variants(&self) -> &Vec<(StrId, Option<Type>, TextRange)> {
        &self.variants
    }

    pub fn is_init(&self) -> bool {
        self.is_init
    }

    pub fn generics(&self) -> Option<&GenericTypeParams> {
        self.generics.as_ref()
    }

    pub fn try_type_for_variant(
        &self,
        variant_name: StrId,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        namespace: &Namespace,
    ) -> Option<Option<Type>> {
        for (curr_variant_name, ty, _) in &self.variants {
            if *curr_variant_name == variant_name {
                let Some(ty) = ty else { return Some(None) };
                if ty.is_concretization_required() {
                    return Some(Some(ty.concretize(
                        &ConcretizationContext::new(global_concrete_types, None),
                        namespace,
                    )));
                }
                return Some(Some(ty.clone()));
            }
        }
        None
    }

    pub fn try_index_for_variant(&self, variant_name: StrId) -> Option<usize> {
        for (index, (curr_variant_name, _, _)) in self.variants.iter().enumerate() {
            if *curr_variant_name == variant_name {
                return Some(index);
            }
        }
        None
    }
}

impl IsInitialized for EnumTypeData {
    fn is_initialized(&self) -> bool {
        self.is_init
    }
}
