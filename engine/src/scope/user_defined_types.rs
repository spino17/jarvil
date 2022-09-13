use super::function::FunctionData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
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
            _ => panic!(panic_message),
        }
    }

    pub fn struct_data(&self, panic_message: &'static str) -> &StructData {
        match self {
            UserDefinedTypeData::STRUCT(data) => data,
            _ => panic!(panic_message),
        }
    }

    pub fn lambda_data_mut(&mut self, panic_message: &'static str) -> &mut LambdaTypeData {
        match self {
            UserDefinedTypeData::LAMBDA(data) => data,
            _ => panic!(panic_message),
        }
    }

    pub fn struct_data_mut(&mut self, panic_message: &'static str) -> &mut StructData {
        match self {
            UserDefinedTypeData::STRUCT(data) => data,
            _ => panic!(panic_message),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct StructData {
    pub fields: Rc<FxHashMap<String, (Type, TextRange)>>,
    pub constructor: FunctionData,
    pub methods: Rc<RefCell<FxHashMap<String, FunctionData>>>,
    pub class_methods: Rc<RefCell<FxHashMap<String, FunctionData>>>,
}

impl StructData {
    pub fn set_fields(&mut self, fields: FxHashMap<String, (Type, TextRange)>) {
        self.fields = Rc::new(fields);
    }

    pub fn try_field(&self, field_name: &String) -> Option<(Type, TextRange)> {
        match self.fields.as_ref().get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &String) -> Option<FunctionData> {
        match self.methods.as_ref().borrow().get(method_name) {
            Some(func_data) => Some(func_data.clone()),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct LambdaTypeData {
    pub func_data: FunctionData,
}

impl LambdaTypeData {
    pub fn new(params: Vec<(Rc<String>, Type)>, return_type: Type) -> Self {
        LambdaTypeData {
            func_data: FunctionData {
                params: Rc::new(params),
                return_type,
            },
        }
    }

    pub fn set_params_and_return_type(
        &mut self,
        params: Vec<(Rc<String>, Type)>,
        return_type: Type,
    ) {
        self.func_data.set_data(params, return_type);
    }
}
