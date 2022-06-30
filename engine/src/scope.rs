// Symbol Table - HashMap for (token_name, meta data like datatypes etc.). Each scope bounded by {...} is represented by
// individual symbol table. Each symbol table points to the symbol table belonging to it's enclosing scope.
// Point of interactions:
// 1. Before lexical analysis phase to initialize the map with keywords.
// 2. During lexical analysis phase to distringuish between pattern matched for identifiers and keywords (if we found lexeme in
// symbol_table then it's a keyword so form token accordingly).
// 3. During parsing phase, we add entries in the table for identifiers when we find declartion statements in the current scope.
// 4. During parsing phase, when we encounter any statement involving identifiers then we access it for checking declartions, 
// datatypes etc.

use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::types::{Type, CoreType, Struct};

#[derive(Debug)]
pub struct FunctionData {
    pub params: Rc<Vec<(Rc<String>, Type)>>,  // change second column to type
    pub return_type: Rc<Option<Type>>,  // change this to type
    // generic_symbols: GenericSymbolVSBounds,
}

#[derive(Debug)]
pub enum StructFunction {
    METHOD(FunctionData),
    CLASS_METHOD(FunctionData),
}

#[derive(Debug)]
struct IdentifierData {
    data_type: Type,  // change this to type
    is_init: bool,
}

#[derive(Debug)]
pub struct StructType {
    pub name: Rc<String>,
    fields: Rc<FxHashMap<Rc<String>, Type>>,  // change this to type
    constructor: FunctionData,
    methods: Rc<RefCell<FxHashMap<Rc<String>, FunctionData>>>,
    class_methods: Rc<RefCell<FxHashMap<Rc<String>, FunctionData>>>,
}

#[derive(Debug)]
pub struct LambdaType {
    pub name: Rc<String>,
    function_data: FunctionData,
}

#[derive(Debug)]
pub enum UserDefinedTypeData {
    STRUCT(StructType),
    LAMBDA(LambdaType),
    // GENERIC(GenericType),
}

#[derive(Debug)]
enum MetaData {
    IDENTIFIER(IdentifierData),
    USER_DEFINED_TYPE(UserDefinedTypeData),
    FUNCTION(FunctionData),
}

#[derive(Debug)]
pub struct SymbolData(Rc<RefCell<MetaData>>);

impl SymbolData {

    // identifier specific methods
    
    pub fn get_type(&self) -> Type {
        match &*self.0.borrow() {
            MetaData::IDENTIFIER(data) => Type(data.data_type.0.clone()),
            _ => {
                Type(Rc::new(CoreType::NONE))
            }
        }
    }

    pub fn get_id_data(&self) -> Option<(Type, bool)> {
        match &*self.0.borrow() {
            MetaData::IDENTIFIER(data) => {
                Some((Type(data.data_type.0.clone()), data.is_init))
            },
            _ => {
                None
            }
        }
    }

    // type specific methods
    pub fn set_method_to_struct(&self, method_name: &Rc<String>, method_data: StructFunction) {
        match &*self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(data) => {
                match data {
                    UserDefinedTypeData::STRUCT(struct_data) => {
                        match method_data {
                            StructFunction::METHOD(function_data) => {
                                struct_data.methods.borrow_mut().insert(method_name.clone(), function_data);
                            },
                            StructFunction::CLASS_METHOD(function_data) => {
                                struct_data.class_methods.borrow_mut().insert(method_name.clone(), function_data);
                            }
                        }
                    },
                    _ => {}
                }
            },
            _ => {}
        }
    }

    pub fn get_user_defined_type_data(&self) -> Option<UserDefinedTypeData> {
        match &*self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(data) => {
                match data {
                    UserDefinedTypeData::STRUCT(struct_data) => {
                        Some(UserDefinedTypeData::STRUCT(StructType{
                            name: struct_data.name.clone(),
                            fields: struct_data.fields.clone(),
                            constructor: FunctionData{
                                params: struct_data.constructor.params.clone(),
                                return_type: struct_data.constructor.return_type.clone(),
                            },
                            methods: struct_data.methods.clone(),
                            class_methods: struct_data.class_methods.clone(),
                        }))
                    },
                    UserDefinedTypeData::LAMBDA(lambda_data) => {
                        Some(UserDefinedTypeData::LAMBDA(LambdaType{
                            name: lambda_data.name.clone(),
                            function_data: FunctionData{
                                params: lambda_data.function_data.params.clone(),
                                return_type: lambda_data.function_data.return_type.clone(),
                            },
                        }))
                    }
                }
            },
            _ => {
                None
            }
        }
    }

    pub fn get_user_defined_struct_type_data(&self) -> Option<StructType> {
        match &*self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(data) => {
                match data {
                    UserDefinedTypeData::STRUCT(struct_data) => {
                        Some(StructType{
                            name: struct_data.name.clone(),
                            fields: struct_data.fields.clone(),
                            constructor: FunctionData{
                                params: struct_data.constructor.params.clone(),
                                return_type: struct_data.constructor.return_type.clone(),
                            },
                            methods: struct_data.methods.clone(),
                            class_methods: struct_data.class_methods.clone(),
                        })
                    },
                    _ => None
                }
            },
            _ => {
                None
            }
        }
    }

    pub fn get_struct_constructor_data(&self) -> Option<FunctionData> {
        match self.get_user_defined_type_data() {
            Some(response) => {
                match response {
                    UserDefinedTypeData::STRUCT(struct_data) => {
                        /*
                        if let Some(function_data) = struct_data.methods.get(&struct_data.name) {
                            Some(FunctionData{
                                params: function_data.params.clone(),
                                return_type: function_data.return_type.clone(),
                            })
                        } else {
                            unreachable!("struct type always have constructor with same name")
                        }
                         */
                        Some(FunctionData{
                            params: struct_data.constructor.params.clone(),
                            return_type: struct_data.constructor.return_type.clone(),
                        })
                    },
                    _ => None
                }
            },
            None => None
        }
    }

    pub fn has_field_with_name(&self, field_name: &Rc<String>) -> Option<Type> {
        match &*self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(data) => {
                match data {
                    UserDefinedTypeData::STRUCT(data) => {
                        match data.fields.get(field_name) {
                            Some(val) => Some(Type(val.0.clone())),
                            None => None,
                        }
                    },
                    _ => {
                        return None
                    }
                }
            },
            _ => {
                None
            }
        }
    }

    pub fn has_method_with_name(&self, method_name: &Rc<String>) -> Option<FunctionData> {
        match &*self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(data) => {
                match data {
                    UserDefinedTypeData::STRUCT(data) => {
                        match data.methods.borrow().get(method_name) {
                            Some(val) => {
                                Some(FunctionData{
                                    params: val.params.clone(),
                                    return_type: val.return_type.clone(),
                                })
                            },
                            None => None,
                        }
                    },
                    _ => {
                        return None
                    }
                }
            },
            _ => {
                None
            }
        }
    }

    pub fn has_class_method_with_name(&self, class_method_name: &Rc<String>) -> Option<FunctionData> {
        match &*self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(data) => {
                match data {
                    UserDefinedTypeData::STRUCT(data) => {
                        match data.class_methods.borrow().get(class_method_name) {
                            Some(val) => {
                                Some(FunctionData{
                                    params: val.params.clone(),
                                    return_type: val.return_type.clone(),
                                })
                            },
                            None => None,
                        }
                    },
                    _ => {
                        return None
                    }
                }
            },
            _ => {
                None
            }
        }
    }
    
    pub fn get_lambda_data(&self) -> Option<FunctionData> {
        match &*self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(data) => {
                match data {
                    UserDefinedTypeData::LAMBDA(data) => {
                        return Some(FunctionData{
                            params: data.function_data.params.clone(),
                            return_type: data.function_data.return_type.clone(),
                        })
                    },
                    _ => {
                        return None
                    }
                }
            },
            _ => {
                None
            }
        }
    }

    // function specific methods
    pub fn get_function_data(&self) -> Option<FunctionData> {
        match &*self.0.borrow() {
            MetaData::FUNCTION(data) => {
                Some(FunctionData{
                    params: data.params.clone(),
                    return_type: data.return_type.clone(),
                })
            },
            _ => {
                None
            }
        }
    }

    // general methods
    pub fn get_type_of_identifier(&self) -> &str {
        match &*self.0.borrow() {
            MetaData::IDENTIFIER(_) => "identifier",
            MetaData::USER_DEFINED_TYPE(user_defined_type) => {
                match user_defined_type {
                    UserDefinedTypeData::STRUCT(_) => "struct",
                    UserDefinedTypeData::LAMBDA(_) => "lambda",
                }
            },
            MetaData::FUNCTION(_) => "function",
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    symbol_table: FxHashMap<Rc<String>, SymbolData>,
    pub parent_env: Option<Env>,
    return_type: Option<Rc<String>>,  // for functional scope - match return type in nested sub blocks checking this global field
}

impl Scope {
    fn set(&mut self, name: Rc<String>, meta_data: MetaData) {
        self.symbol_table.insert(name, SymbolData(Rc::new(RefCell::new(meta_data))));
    }

    fn get(&self, name: &Rc<String>) -> Option<&SymbolData> {
        self.symbol_table.get(name)
    }
}

#[derive(Debug)]
pub struct Env(pub Rc<RefCell<Scope>>);

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: FxHashMap::default(),
            parent_env: None,
            return_type: None,
        })))
    }

    pub fn new_with_parent_env(parent_env: &Env) -> Self {
        let env = parent_env.0.clone();
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: FxHashMap::default(),
            parent_env: Some(Env(env)),
            return_type: None,
        })))
    }

    pub fn set_identifier(&self, identifier_name: &Rc<String>, data_type: &Type, is_init: bool) {
        let meta_data = MetaData::IDENTIFIER(IdentifierData{
            data_type: Type(data_type.0.clone()),
            is_init,
        });
        self.0.borrow_mut().set(identifier_name.clone(), meta_data);
    }

    pub fn set_user_defined_struct_type(&self, identifier_name: &Rc<String>, fields: &Rc<Vec<(Rc<String>, Type)>>) {
        let mut constructor_data: Vec<(Rc<String>, Type)> = vec![];
        let mut fields_map: FxHashMap<Rc<String>, Type> = FxHashMap::default();
        for (field_name, data_type) in fields.as_ref() {
            constructor_data.push((field_name.clone(), Type(data_type.0.clone())));
            fields_map.insert(field_name.clone(), Type(data_type.0.clone()));
        }
        let methods: FxHashMap<Rc<String>, FunctionData> = FxHashMap::default();
        let class_methods: FxHashMap<Rc<String>, FunctionData> = FxHashMap::default();
        /*
        methods.insert(identifier_name.clone(), FunctionData {
            params: Rc::new(constructor_data), 
            return_type: Rc::new(Some(identifier_name.clone())),
        });
         */
        let meta_data = MetaData::USER_DEFINED_TYPE(UserDefinedTypeData::STRUCT(StructType{
            name: identifier_name.clone(),
            fields: Rc::new(fields_map),
            constructor: FunctionData{
                params: Rc::new(constructor_data),
                return_type: Rc::new(Some(Type(Rc::new(CoreType::STRUCT(Struct{
                    name: identifier_name.clone(),
                }))))),
            },
            methods: Rc::new(RefCell::new(methods)),
            class_methods: Rc::new(RefCell::new(class_methods)),
        }));
        self.0.borrow_mut().set(identifier_name.clone(), meta_data);
    }

    pub fn set_user_defined_lambda_type(&self, identifier_name: &Rc<String>, 
        params: &Rc<Vec<(Rc<String>, Type)>>, return_type: &Rc<Option<Type>>) {
        let meta_data = MetaData::USER_DEFINED_TYPE(UserDefinedTypeData::LAMBDA(LambdaType{
            name: identifier_name.clone(),
            function_data: FunctionData{
                params: params.clone(),
                return_type: return_type.clone(),
            }
        }));
        self.0.borrow_mut().set(identifier_name.clone(), meta_data);
    }

    pub fn set_function(&self, identifier_name: &Rc<String>, 
        params: &Rc<Vec<(Rc<String>, Type)>>, return_type: &Rc<Option<Type>>) {
        let meta_data = MetaData::FUNCTION(FunctionData{
            params: params.clone(),
            return_type: return_type.clone(),
        });
        self.0.borrow_mut().set(identifier_name.clone(), meta_data);
    }

    pub fn set_method_to_struct(&self, struct_name: &Rc<String>, method_name: &Rc<String>, method_data: StructFunction) {
        // self.0.borrow_mut().set_method_to_struct(struct_name, method_name, method_data);
        if let Some(symbol_data) = self.get(struct_name) {
            symbol_data.set_method_to_struct(method_name, method_data)
        }
    }

    pub fn get(&self, key: &Rc<String>) -> Option<SymbolData> {
        let scope_ref = self.0.borrow();

        // check the identifier name in current scope
        match scope_ref.get(key) {
            Some(value) => {
                Some(SymbolData(value.0.clone()))
            },
            None => {

                // if not found in current scope, check recursively in parent scopes
                if let Some(parent_env) = &scope_ref.parent_env {

                    // return from the nearest scope which found the identifier name
                    parent_env.get(key)
                } else {
                    None
                }
            }
        }
    }
}