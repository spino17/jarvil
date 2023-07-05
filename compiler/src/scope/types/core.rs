use crate::{
    scope::{
        concrete::core::ConcreteTypesRegistryKey,
        core::{AbstractConcreteTypesHandler, GenericContainingConstructs},
        types::generic_type::GenericTypeData,
        types::lambda_type::LambdaTypeData,
        types::struct_type::StructTypeData,
    },
    types::core::Type,
};

#[derive(Debug)]
pub enum UserDefinedTypeData {
    Struct(StructTypeData),
    Lambda(LambdaTypeData),
    Generic(GenericTypeData),
}

impl UserDefinedTypeData {
    pub fn default_with_struct() -> Self {
        UserDefinedTypeData::Struct(StructTypeData::default())
    }

    // Below methods should only be called if getting the desired variant is guarenteed
    // that's why interally it uses `unreachable!()`
    pub fn get_struct_data_ref(&self) -> &StructTypeData {
        match self {
            UserDefinedTypeData::Struct(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn get_struct_data_mut_ref(&mut self) -> &mut StructTypeData {
        match self {
            UserDefinedTypeData::Struct(data) => data,
            _ => unreachable!(),
        }
    }
}

impl AbstractConcreteTypesHandler for UserDefinedTypeData {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        match self {
            UserDefinedTypeData::Struct(struct_type_data) => struct_type_data
                .register_concrete_types(concrete_types, generics_containing_indexes),
            UserDefinedTypeData::Lambda(lambda_type_data) => lambda_type_data
                .register_concrete_types(concrete_types, generics_containing_indexes),
            UserDefinedTypeData::Generic(_) => unreachable!(),
        }
    }

    fn register_method_concrete_types(
        &mut self,
        key: Option<ConcreteTypesRegistryKey>,
        method_name: String,
        method_concrete_types: Vec<Type>,
        method_generics_containing_indexes: Vec<usize>,
    ) {
        match self {
            UserDefinedTypeData::Struct(struct_type_data) => struct_type_data
                .register_method_concrete_types(
                    key,
                    method_name,
                    method_concrete_types,
                    method_generics_containing_indexes,
                ),
            UserDefinedTypeData::Lambda(_) => unreachable!(),
            UserDefinedTypeData::Generic(_) => unreachable!(),
        }
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        match self {
            UserDefinedTypeData::Struct(struct_type_data) => {
                struct_type_data.get_concrete_types_at_key(key)
            }
            UserDefinedTypeData::Lambda(lambda_type_data) => {
                lambda_type_data.get_concrete_types_at_key(key)
            }
            UserDefinedTypeData::Generic(_) => unreachable!(),
        }
    }
}

impl GenericContainingConstructs for UserDefinedTypeData {
    fn has_generics(&self) -> bool {
        match self {
            UserDefinedTypeData::Struct(struct_type_data) => struct_type_data.has_generics(),
            UserDefinedTypeData::Lambda(lambda_type_data) => lambda_type_data.has_generics(),
            UserDefinedTypeData::Generic(_) => false,
        }
    }
}
