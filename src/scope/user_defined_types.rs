use super::function::FunctionData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug)]
pub enum UserDefinedTypeData {
    STRUCT(StructData),
    LAMBDA(LambdaTypeData),
}

impl UserDefinedTypeData {
    pub fn default_with_struct() -> Self {
        UserDefinedTypeData::STRUCT(StructData::default())
    }

    pub fn default_with_lambda() -> Self {
        UserDefinedTypeData::LAMBDA(LambdaTypeData::default())
    }

    // below methods should only be called if getting the desired variant is guarenteed
    // that's why interally it uses `unreachable!()`
    pub fn get_struct_data_mut_ref(&mut self) -> &mut StructData {
        match self {
            UserDefinedTypeData::STRUCT(data) => data,
            _ => unreachable!(),
        }
    }

    pub fn get_lambda_data_ref(&self) -> &LambdaTypeData {
        match self {
            UserDefinedTypeData::LAMBDA(data) => data,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Default)]
pub struct StructData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub constructor: FunctionData,
    pub methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub class_methods: FxHashMap<String, (FunctionData, TextRange)>,
}

impl StructData {
    pub fn set_fields(&mut self, fields: FxHashMap<String, (Type, TextRange)>) {
        self.fields = fields;
    }

    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        constructor: Option<(FunctionData, TextRange)>,
        methods: FxHashMap<String, (FunctionData, TextRange)>,
        class_methods: FxHashMap<String, (FunctionData, TextRange)>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.class_methods = class_methods;
        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
    }

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &str) -> Option<(FunctionData, TextRange)> {
        match self.methods.get(method_name) {
            Some(func_data) => Some(func_data.clone()),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct LambdaTypeData {
    pub meta_data: FunctionData,
}

impl LambdaTypeData {
    pub fn new(param_types: Vec<Type>, return_type: Type) -> Self {
        LambdaTypeData {
            meta_data: FunctionData {
                params: Rc::new(param_types),
                return_type,
            },
        }
    }
}
