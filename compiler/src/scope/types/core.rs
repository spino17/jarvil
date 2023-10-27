use crate::scope::{
    core::AbstractConcreteTypesHandler, types::generic_type::GenericTypeData,
    types::lambda_type::LambdaTypeData, types::struct_type::StructTypeData,
};

use super::enum_type::EnumTypeData;

#[derive(Debug)]
pub enum UserDefinedTypeData {
    Struct(StructTypeData),
    Lambda(LambdaTypeData),
    Enum(EnumTypeData),
    Generic(GenericTypeData),
}

#[derive(Debug)]
pub enum UserDefineTypeKind {
    Struct,
    Lambda,
    Enum,
    Generic,
}

impl UserDefinedTypeData {
    pub fn default_with_struct() -> Self {
        UserDefinedTypeData::Struct(StructTypeData::default())
    }

    pub fn default_with_enum() -> Self {
        UserDefinedTypeData::Enum(EnumTypeData::default())
    }

    pub fn get_kind(&self) -> UserDefineTypeKind {
        match self {
            UserDefinedTypeData::Struct(_) => UserDefineTypeKind::Struct,
            UserDefinedTypeData::Lambda(_) => UserDefineTypeKind::Lambda,
            UserDefinedTypeData::Generic(_) => UserDefineTypeKind::Generic,
            UserDefinedTypeData::Enum(_) => UserDefineTypeKind::Enum,
        }
    }

    pub fn is_initialized(&self) -> bool {
        match self {
            UserDefinedTypeData::Struct(struct_data) => struct_data.is_init,
            UserDefinedTypeData::Enum(enum_data) => enum_data.is_init,
            UserDefinedTypeData::Lambda(_) | UserDefinedTypeData::Generic(_) => true,
        }
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

    pub fn get_enum_data_ref(&self) -> &EnumTypeData {
        match self {
            UserDefinedTypeData::Enum(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn get_enum_data_mut_ref(&mut self) -> &mut EnumTypeData {
        match self {
            UserDefinedTypeData::Enum(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn get_lambda_data_ref(&self) -> &LambdaTypeData {
        match self {
            UserDefinedTypeData::Lambda(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn get_lambda_data_mut_ref(&mut self) -> &mut LambdaTypeData {
        match self {
            UserDefinedTypeData::Lambda(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn get_generic_data_ref(&self) -> &GenericTypeData {
        match self {
            UserDefinedTypeData::Generic(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn get_generic_data_mut_ref(&mut self) -> &mut GenericTypeData {
        match self {
            UserDefinedTypeData::Generic(data) => data,
            _ => unreachable!(),
        }
    }
}

impl AbstractConcreteTypesHandler for UserDefinedTypeData {
    fn is_initialized(&self) -> bool {
        match self {
            UserDefinedTypeData::Struct(struct_type_data) => struct_type_data.is_initialized(),
            UserDefinedTypeData::Enum(enum_type_data) => enum_type_data.is_initialized(),
            UserDefinedTypeData::Lambda(_) | UserDefinedTypeData::Generic(_) => true,
        }
    }
}
