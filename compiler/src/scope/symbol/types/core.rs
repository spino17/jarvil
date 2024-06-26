use super::enum_ty::EnumTypeData;
use crate::scope::concrete::TurbofishTypes;
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::helper::check_concrete_types_bounded_by_interfaces;
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::core::{SymbolDataEntry, SymbolIndex};
use crate::scope::traits::{AbstractSymbol, IsInitialized};
use crate::scope::{
    symbol::types::generic_ty::GenericTypeData, symbol::types::lambda_ty::LambdaTypeData,
    symbol::types::struct_ty::StructTypeData,
};
use crate::types::core::TypeStringifyContext;
use text_size::TextRange;

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

    pub fn kind(&self) -> UserDefineTypeKind {
        match self {
            UserDefinedTypeData::Struct(_) => UserDefineTypeKind::Struct,
            UserDefinedTypeData::Lambda(_) => UserDefineTypeKind::Lambda,
            UserDefinedTypeData::Generic(_) => UserDefineTypeKind::Generic,
            UserDefinedTypeData::Enum(_) => UserDefineTypeKind::Enum,
        }
    }

    pub fn is_initialized(&self) -> bool {
        match self {
            UserDefinedTypeData::Struct(struct_data) => struct_data.is_init(),
            UserDefinedTypeData::Enum(enum_data) => enum_data.is_init(),
            UserDefinedTypeData::Lambda(_) | UserDefinedTypeData::Generic(_) => true,
        }
    }

    // Below methods should only be called if getting the desired variant is guarenteed
    // that's why interally it uses `unreachable!()`
    pub fn struct_data_ref(&self) -> &StructTypeData {
        match self {
            UserDefinedTypeData::Struct(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn struct_data_mut_ref(&mut self) -> &mut StructTypeData {
        match self {
            UserDefinedTypeData::Struct(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn enum_data_ref(&self) -> &EnumTypeData {
        match self {
            UserDefinedTypeData::Enum(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn enum_data_mut_ref(&mut self) -> &mut EnumTypeData {
        match self {
            UserDefinedTypeData::Enum(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn lambda_data_ref(&self) -> &LambdaTypeData {
        match self {
            UserDefinedTypeData::Lambda(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn lambda_data_mut_ref(&mut self) -> &mut LambdaTypeData {
        match self {
            UserDefinedTypeData::Lambda(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn generic_data_ref(&self) -> &GenericTypeData {
        match self {
            UserDefinedTypeData::Generic(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn generic_data_mut_ref(&mut self) -> &mut GenericTypeData {
        match self {
            UserDefinedTypeData::Generic(data) => data,
            _ => unreachable!(),
        }
    }
}

impl IsInitialized for UserDefinedTypeData {
    fn is_initialized(&self) -> bool {
        match self {
            UserDefinedTypeData::Struct(struct_ty_data) => struct_ty_data.is_initialized(),
            UserDefinedTypeData::Enum(enum_ty_data) => enum_ty_data.is_initialized(),
            UserDefinedTypeData::Lambda(_) | UserDefinedTypeData::Generic(_) => true,
        }
    }
}

#[derive(Debug)]
pub struct UserDefinedTypeSymbolData(pub SymbolIndex<UserDefinedTypeData>);

impl AbstractSymbol for UserDefinedTypeSymbolData {
    type SymbolTy = UserDefinedTypeData;

    fn symbol_index(&self) -> SymbolIndex<Self::SymbolTy> {
        self.0
    }

    fn entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Type(self.0)
    }

    fn check_generic_ty_args(
        &self,
        concrete_types: Option<&TurbofishTypes>,
        ty_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        context: TypeStringifyContext,
    ) -> Result<(), GenericTypeArgsCheckError> {
        match context
            .namespace()
            .types_ref()
            .symbol_ref(self.0)
            .data_ref()
        {
            UserDefinedTypeData::Struct(struct_data) => {
                let generic_ty_decls = struct_data.generics();

                check_concrete_types_bounded_by_interfaces(
                    generic_ty_decls,
                    concrete_types,
                    ty_ranges,
                    is_concrete_types_none_allowed,
                    context,
                )
            }
            UserDefinedTypeData::Lambda(lambda_data) => {
                let generic_ty_decls = lambda_data.generics();

                check_concrete_types_bounded_by_interfaces(
                    generic_ty_decls,
                    concrete_types,
                    ty_ranges,
                    false,
                    context,
                )
            }
            UserDefinedTypeData::Enum(enum_data) => {
                let generic_ty_decls = enum_data.generics();

                check_concrete_types_bounded_by_interfaces(
                    generic_ty_decls,
                    concrete_types,
                    ty_ranges,
                    false,
                    context,
                )
            }
            UserDefinedTypeData::Generic(_) => {
                if concrete_types.is_some() {
                    return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected);
                }

                Ok(())
            }
        }
    }

    fn mangled_name(&self, namespace: &Namespace) -> MangledIdentifierName<Self::SymbolTy> {
        self.0.mangled_name(namespace.types_ref())
    }
}

impl From<SymbolIndex<UserDefinedTypeData>> for UserDefinedTypeSymbolData {
    fn from(value: SymbolIndex<UserDefinedTypeData>) -> Self {
        UserDefinedTypeSymbolData(value)
    }
}
