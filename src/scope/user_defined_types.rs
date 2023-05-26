use super::function::FunctionData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug, Clone)]
pub enum UserDefinedTypeData {
    STRUCT(StructData),
    LAMBDA(LambdaTypeData),
    // GENERIC(GenericType),
}

impl UserDefinedTypeData {
    pub fn default_with_struct() -> Self {
        UserDefinedTypeData::STRUCT(StructData::default())
    }

    pub fn default_with_lambda() -> Self {
        UserDefinedTypeData::LAMBDA(LambdaTypeData::default())
    }

    pub fn lambda_data(&self, panic_message: &'static str) -> &LambdaTypeData {
        match self {
            UserDefinedTypeData::LAMBDA(data) => data,
            _ => panic!("{}", panic_message),
        }
    }

    pub fn struct_data(&self, panic_message: &'static str) -> &StructData {
        match self {
            UserDefinedTypeData::STRUCT(data) => data,
            _ => panic!("{}", panic_message),
        }
    }

    pub fn lambda_data_mut(&mut self, panic_message: &'static str) -> &mut LambdaTypeData {
        match self {
            UserDefinedTypeData::LAMBDA(data) => data,
            _ => panic!("{}", panic_message),
        }
    }

    pub fn struct_data_mut(&mut self, panic_message: &'static str) -> &mut StructData {
        match self {
            UserDefinedTypeData::STRUCT(data) => data,
            _ => panic!("{}", panic_message),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct StructData {
    pub fields: Rc<FxHashMap<String, (Type, TextRange)>>,
    pub constructor: FunctionData,
    pub methods: Rc<FxHashMap<String, (FunctionData, TextRange)>>,
    pub class_methods: Rc<FxHashMap<String, (FunctionData, TextRange)>>,
}

impl StructData {
    pub fn set_fields(&mut self, fields: FxHashMap<String, (Type, TextRange)>) {
        self.fields = Rc::new(fields);
    }

    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        constructor: Option<(FunctionData, TextRange)>,
        methods: FxHashMap<String, (FunctionData, TextRange)>,
        class_methods: FxHashMap<String, (FunctionData, TextRange)>,
    ) {
        self.fields = Rc::new(fields);
        self.methods = Rc::new(methods);
        self.class_methods = Rc::new(class_methods);
        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
    }

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        match self.fields.as_ref().get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &str) -> Option<(FunctionData, TextRange)> {
        match self.methods.as_ref().get(method_name) {
            Some(func_data) => Some(func_data.clone()),
            None => None,
        }
    }

    pub fn try_class_method(
        &self,
        class_method_name: &str,
    ) -> Option<(FunctionData, TextRange)> {
        match self.class_methods.as_ref().get(class_method_name) {
            Some(func_data) => Some(func_data.clone()),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct LambdaTypeData {
    pub meta_data: FunctionData
}

impl LambdaTypeData {
    pub fn new(param_types: Vec<Type>, return_type: Type) -> Self {
        LambdaTypeData {
            meta_data: FunctionData {
                params: Rc::new(param_types),
                return_type,
            }
        }
    }

    pub fn set_params_and_return_type(&mut self, param_types: Vec<Type>, return_type: Type) {
        self.meta_data.params = Rc::new(param_types);
        self.meta_data.return_type = return_type
    }
}
