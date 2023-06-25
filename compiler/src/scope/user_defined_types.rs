use super::{
    concrete::{
        CallableConcreteTypesRegistry, ConcreteSymbolData, ConcreteTypesRegistryKey,
        GenericsSpecAndConcreteTypesRegistry, StructConcreteTypesRegistry,
    },
    core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
    function::FunctionData,
    interfaces::InterfaceData,
};
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

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
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        match self {
            UserDefinedTypeData::Struct(struct_type_data) => {
                struct_type_data.register_concrete_types(concrete_types)
            }
            UserDefinedTypeData::Lambda(lambda_type_data) => {
                lambda_type_data.register_concrete_types(concrete_types)
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

#[derive(Debug, Default)]
pub struct StructTypeData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub constructor: FunctionData,
    pub methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub class_methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry<StructConcreteTypesRegistry>>,
}

impl StructTypeData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        constructor: Option<(FunctionData, TextRange)>,
        methods: FxHashMap<String, (FunctionData, TextRange)>,
        class_methods: FxHashMap<String, (FunctionData, TextRange)>,
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.class_methods = class_methods;
        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
        self.generics = match generics_spec {
            Some(generics_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                generics_spec,
                concrete_types_registry: StructConcreteTypesRegistry::default(),
            }),
            None => None,
        }
    }

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &str) -> Option<(&FunctionData, TextRange)> {
        match self.methods.get(method_name) {
            Some(func_data) => Some((&func_data.0, func_data.1)),
            None => None,
        }
    }

    pub fn register_method_concrete_types_for_key(
        &mut self,
        key: &ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: &Vec<Type>,
    ) {
        //self.concrete_types_registry
        //    .register_method_concrete_types_for_key(key, method_name, method_concrete_types)
        todo!()
    }
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        match &mut self.generics {
            Some(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            None => unreachable!(),
        }
    }
}

impl GenericContainingConstructs for StructTypeData {
    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}

#[derive(Debug, Default)]
pub struct LambdaTypeData {
    pub meta_data: FunctionData,
}

impl LambdaTypeData {
    pub fn new(
        param_types: Vec<Type>,
        return_type: Type,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        LambdaTypeData {
            meta_data: FunctionData {
                params: param_types,
                return_type,
                generics: match generics_spec {
                    Some(generic_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                        generics_spec: generic_spec,
                        concrete_types_registry: CallableConcreteTypesRegistry::default(),
                    }),
                    None => None,
                },
            },
        }
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        self.meta_data.register_concrete_types(concrete_types)
    }
}

impl GenericContainingConstructs for LambdaTypeData {
    fn has_generics(&self) -> bool {
        self.meta_data.has_generics()
    }
}

#[derive(Debug)]
pub struct GenericTypeData {
    index: usize, // index in the sequence of all generic type params in declaration
    category: GenericTypeCategory,
    interface_bounds: Vec<ConcreteSymbolData<InterfaceData>>,
}

#[derive(Debug)]
pub enum GenericTypeCategory {
    Struct,
    Callable,
}
