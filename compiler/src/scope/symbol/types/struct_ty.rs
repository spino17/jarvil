use crate::core::string_interner::IdentName;
use crate::scope::concrete::TypeGenericsInstantiationContext;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::common::{FieldsMap, MethodsMap};
use crate::scope::symbol::function::{CallableData, CallableKind, PartialConcreteCallableDataRef};
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::generic_ty::GenericTypeParams;
use crate::scope::traits::IsInitialized;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug)]
pub struct StructTypeData {
    fields: FieldsMap,
    constructor: CallableData,
    methods: MethodsMap,
    class_methods: MethodsMap,
    generics: Option<GenericTypeParams>,
    implementing_interfaces: Option<InterfaceBounds>,
    is_init: bool,
}

impl StructTypeData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<IdentName, (Type, TextRange)>,
        constructor: Option<(CallableData, TextRange)>,
        methods: FxHashMap<IdentName, (CallableData, TextRange)>,
        class_methods: FxHashMap<IdentName, (CallableData, TextRange)>,
    ) {
        self.fields = FieldsMap::new(fields);
        self.methods = MethodsMap::new(methods);
        self.class_methods = MethodsMap::new(class_methods);

        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
    }

    pub fn set_generics_and_interfaces(
        &mut self,
        generics_spec: Option<GenericTypeParams>,
        implementing_interfaces: Option<InterfaceBounds>,
    ) {
        self.generics = generics_spec;
        self.implementing_interfaces = implementing_interfaces;
        self.is_init = true;
    }

    pub fn is_init(&self) -> bool {
        self.is_init
    }

    pub fn constructor(&self) -> &CallableData {
        &self.constructor
    }

    pub fn implementing_interfaces(&self) -> Option<&InterfaceBounds> {
        self.implementing_interfaces.as_ref()
    }

    pub fn generics(&self) -> Option<&GenericTypeParams> {
        self.generics.as_ref()
    }

    pub fn try_field<'a>(
        &'a self,
        field_name: &IdentName,
        namespace: &Namespace,
        context: TypeGenericsInstantiationContext,
    ) -> Option<(Type, TextRange)> {
        self.fields.try_field(field_name, namespace, context)
    }

    pub fn try_method<'a>(
        &'a self,
        method_name: &IdentName,
        context: TypeGenericsInstantiationContext<'a>,
    ) -> Option<(PartialConcreteCallableDataRef, TextRange)> {
        self.methods.try_method(method_name, context)
    }

    pub fn try_class_method<'a>(
        &'a self,
        class_method_name: &IdentName,
        context: TypeGenericsInstantiationContext<'a>,
    ) -> Option<(PartialConcreteCallableDataRef, TextRange)> {
        self.class_methods.try_method(class_method_name, context)
    }

    pub fn methods_ref(&self) -> &MethodsMap {
        &self.methods
    }

    pub fn fields_ref(&self) -> &FieldsMap {
        &self.fields
    }
}

impl IsInitialized for StructTypeData {
    fn is_initialized(&self) -> bool {
        self.is_init
    }
}

impl Default for StructTypeData {
    fn default() -> Self {
        StructTypeData {
            fields: FieldsMap::default(),
            constructor: CallableData::default_for_kind(CallableKind::Method),
            methods: MethodsMap::default(),
            class_methods: MethodsMap::default(),
            generics: Option::default(),
            implementing_interfaces: Option::default(),
            is_init: false,
        }
    }
}
