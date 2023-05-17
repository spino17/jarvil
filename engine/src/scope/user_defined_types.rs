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
    pub fields: Rc<FxHashMap<Rc<String>, (Type, TextRange)>>,
    pub constructor: (FunctionData, bool), // bool field is `is_constructor_set` to prevent redeclaration of constructor
    pub methods: Rc<RefCell<FxHashMap<Rc<String>, (FunctionData, TextRange)>>>,
    pub class_methods: Rc<RefCell<FxHashMap<Rc<String>, (FunctionData, TextRange)>>>,
}

impl StructData {
    pub fn set_fields(&mut self, fields: FxHashMap<Rc<String>, (Type, TextRange)>) {
        self.fields = Rc::new(fields);
    }

    pub fn try_constructor(&self) -> Option<FunctionData> {
        if self.constructor.1 {
            return Some(self.constructor.0.clone());
        } else {
            return None;
        }
    }

    pub fn try_field(&self, field_name: &Rc<String>) -> Option<(Type, TextRange)> {
        match self.fields.as_ref().get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &Rc<String>) -> Option<(FunctionData, TextRange)> {
        match self.methods.as_ref().borrow().get(method_name) {
            Some(func_data) => Some(func_data.clone()),
            None => None,
        }
    }

    pub fn try_class_method(
        &self,
        class_method_name: &Rc<String>,
    ) -> Option<(FunctionData, TextRange)> {
        match self.class_methods.as_ref().borrow().get(class_method_name) {
            Some(func_data) => Some(func_data.clone()),
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LambdaTypeData {
    pub param_types: Rc<Vec<Type>>,
    pub return_type: Type,
}

impl Default for LambdaTypeData {
    fn default() -> Self {
        LambdaTypeData {
            param_types: Rc::new(vec![]),
            return_type: Type::new_with_unknown(),
        }
    }
}

impl LambdaTypeData {
    pub fn new(param_types: Vec<Type>, return_type: Type) -> Self {
        LambdaTypeData {
            param_types: Rc::new(param_types),
            return_type: return_type,
        }
    }

    pub fn set_params_and_return_type(&mut self, param_types: Vec<Type>, return_type: Type) {
        self.param_types = Rc::new(param_types);
        self.return_type = return_type;
    }
}
