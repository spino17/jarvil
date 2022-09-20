pub const SCOPE_NOT_SET_TO_BLOCK_MSG: &'static str =
    "scope should be set to the `BlockNode` in the first phase of resolver";
pub const STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG: &'static str = "
    struct name should be binded with `StructData` variant of `SymbolData<UserDefinedTypeData>`
";
pub const LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG: &'static str = "
    lambda type name should be binded with `LambdaTypeData` variant of `SymbolData<UserDefinedTypeData>`
";

pub const CASTING_DATA_ERROR_MSG: &'static str = "`Data` type value not castable";
pub const CASTING_OBJECT_ERROR_MSG: &'static str = "`Object` type value not castable";
pub const TYPE_CHECK_BUG_ERROR_MSG: &'static str = "type-checking phase should not allow this type";
