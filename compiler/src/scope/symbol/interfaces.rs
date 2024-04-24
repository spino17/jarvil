use super::core::SymbolDataEntry;
use super::core::SymbolIndex;
use super::function::CallableData;
use super::function::PartialConcreteCallableDataRef;
use crate::core::string_interner::StrId;
use crate::scope::concrete::ConcreteSymbolIndex;
use crate::scope::concrete::TurbofishTypes;
use crate::scope::concrete::TypeGenericsInstantiationContext;
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::helper::check_concrete_types_bounded_by_interfaces;
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::common::FieldsMap;
use crate::scope::symbol::common::MethodsMap;
use crate::scope::symbol::types::generic_ty::GenericTypeParams;
use crate::scope::traits::AbstractSymbol;
use crate::scope::traits::IsInitialized;
use crate::types::core::Type;
use crate::types::core::TypeStringifyContext;
use crate::types::traits::TypeLike;
use rustc_hash::FxHashMap;
use std::slice::Iter;
use std::vec;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct InterfaceData {
    fields: FieldsMap,
    methods: MethodsMap,
    generics: Option<GenericTypeParams>,
    is_init: bool,
}

impl InterfaceData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<StrId, (Type, TextRange)>,
        methods: FxHashMap<StrId, (CallableData, TextRange)>,
    ) {
        self.fields = FieldsMap::new(fields);
        self.methods = MethodsMap::new(methods);
    }

    pub fn set_generics(&mut self, generics_spec: Option<GenericTypeParams>) {
        self.generics = generics_spec;
        self.is_init = true;
    }

    pub fn generics(&self) -> Option<&GenericTypeParams> {
        self.generics.as_ref()
    }

    pub fn try_field<'a>(
        &'a self,
        field_name: &StrId,
        namespace: &Namespace,
        context: TypeGenericsInstantiationContext<'a>,
    ) -> Option<(Type, TextRange)> {
        self.fields.try_field(field_name, namespace, context)
    }

    pub fn try_method<'a>(
        &'a self,
        method_name: &StrId,
        context: TypeGenericsInstantiationContext<'a>,
    ) -> Option<(PartialConcreteCallableDataRef, TextRange)> {
        self.methods.try_method(method_name, context)
    }

    pub fn has_method(&self, method_name: &StrId) -> bool {
        self.methods.has_method(method_name)
    }

    pub fn partially_concrete_interface_bounded_objects<'a>(
        &'a self,
        context: TypeGenericsInstantiationContext<'a>,
    ) -> PartialConcreteInterfaceBoundedObjects {
        PartialConcreteInterfaceBoundedObjects::new(&self.fields, &self.methods, context)
    }
}

impl IsInitialized for InterfaceData {
    fn is_initialized(&self) -> bool {
        self.is_init
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceObject(ConcreteSymbolIndex<InterfaceData>);

impl InterfaceObject {
    pub fn new(
        symbol_index: SymbolIndex<InterfaceData>,
        concrete_types: Option<TurbofishTypes>,
    ) -> Self {
        InterfaceObject(ConcreteSymbolIndex::new(symbol_index, concrete_types))
    }

    pub fn name(&self) -> StrId {
        self.0.symbol_index().ident_name()
    }

    pub fn concrete_types(&self) -> Option<&TurbofishTypes> {
        self.0.concrete_types()
    }

    pub fn core_symbol(&self) -> &ConcreteSymbolIndex<InterfaceData> {
        &self.0
    }

    pub fn is_eq(&self, other: &InterfaceObject, namespace: &Namespace) -> bool {
        if self.name() != other.name() {
            return false;
        }
        let Some(self_concrete_types) = self.concrete_types() else {
            return true;
        };
        let Some(other_concrete_types) = other.concrete_types() else {
            unreachable!()
        };
        let self_len = self_concrete_types.len();
        let other_len = other_concrete_types.len();
        debug_assert!(self_len == other_len);
        for i in 0..self_len {
            if !self_concrete_types[i].is_eq(&other_concrete_types[i], namespace) {
                return false;
            }
        }
        true
    }

    pub fn to_string(&self, context: TypeStringifyContext) -> String {
        let mut s = context.interner().lookup(self.name());
        match self.concrete_types() {
            Some(concrete_types) => {
                s.push('<');
                s.push_str(&concrete_types.to_string(context));
                s.push('>');
                s
            }
            None => s,
        }
    }
}

// NOTE - More efficient way to implement interface bounds is to use `HashSet<InterfaceObject>`
// which would require us to make `InterfaceObject` hashable. We already have the notion
// of equality between `InterfaceObject` but to construct a hash function which adheres
// to the following constraint: k1 == k1 => Hash(k1) == Hash(k2) is not at all obvious.
// One approach can be to construct `Vec<String>` (which is hashable) from names of interfaces and types but that
// can contain a lot of collision for example:
// [ExampleInterface<genericType>, AnotherInterface] => ["ExampleInterface", "genericType", "AnotherInterface"]
// [ExampleInterface, genericType, AnotherInterface] => ["ExampleInterface", "genericType", "AnotherInterface"]
// Furthermore the equality of lambda types is structural so that complicates the process.
// To actually realize a hash implementation of `InterfaceObject` requires more advanced name-mangling approach which for now
// we choose to skip.
// Given all the above factors we used the brute force approach (which is fine for small number of interfaces which often times
// is the case).
#[derive(Debug, Default, Clone)]
pub struct InterfaceBounds {
    interfaces: Vec<(InterfaceObject, TextRange)>,
}

impl InterfaceBounds {
    pub fn new(interfaces: Vec<(InterfaceObject, TextRange)>) -> Self {
        InterfaceBounds { interfaces }
    }

    pub fn len(&self) -> usize {
        self.interfaces.len()
    }

    pub fn iter(&self) -> Iter<'_, (InterfaceObject, TextRange)> {
        self.interfaces.iter()
    }

    pub fn contains(&self, obj: &InterfaceObject, namespace: &Namespace) -> Option<TextRange> {
        for (entry, decl_range) in &self.interfaces {
            if entry.is_eq(obj, namespace) {
                return Some(*decl_range);
            }
        }
        None
    }

    pub fn interface_obj_at_index(&self, index: usize) -> &InterfaceObject {
        &self.interfaces[index].0
    }

    pub fn insert(
        &mut self,
        obj: InterfaceObject,
        decl_range: TextRange,
        namespace: &Namespace,
    ) -> Option<TextRange> {
        match self.contains(&obj, namespace) {
            Some(previous_decl_range) => Some(previous_decl_range),
            None => {
                self.interfaces.push((obj, decl_range));
                None
            }
        }
    }

    pub fn is_subset(&self, other: &InterfaceBounds, namespace: &Namespace) -> bool {
        for (self_entry, _) in &self.interfaces {
            if other.contains(self_entry, namespace).is_none() {
                return false;
            }
        }
        true
    }

    pub fn is_eq(&self, other: &InterfaceBounds, namespace: &Namespace) -> bool {
        self.is_subset(other, namespace) && other.is_subset(self, namespace)
    }

    pub fn to_string(&self, context: TypeStringifyContext) -> String {
        let mut s = "{".to_string();
        let len = self.interfaces.len();
        if len > 0 {
            s.push_str(&self.interfaces[0].0.to_string(context));
        }
        for i in 1..len {
            s.push_str(" + ");
            s.push_str(&self.interfaces[i].0.to_string(context));
        }
        s.push('}');
        s
    }
}

#[derive(Debug)]
pub enum PartialConcreteInterfaceSpecsCheckError {
    GenericTypesDeclarationExpected(TextRange),
    GenericTypesDeclarationNotExpected(TextRange),
    PrototypeEquivalenceCheckFailed(TextRange),
    GenericTypesDeclarationCheckFailed(TextRange),
    FieldTypeEquivalenceCheckFailed(Type, Type, TextRange),
    MissingField,
    MissingMethod,
}

#[derive(Debug)]
pub struct PartialConcreteInterfaceBoundedObjects<'a> {
    fields: &'a FieldsMap,
    methods: &'a MethodsMap,
    context: TypeGenericsInstantiationContext<'a>,
}

impl<'a> PartialConcreteInterfaceBoundedObjects<'a> {
    pub fn new(
        fields: &'a FieldsMap,
        methods: &'a MethodsMap,
        context: TypeGenericsInstantiationContext<'a>,
    ) -> Self {
        PartialConcreteInterfaceBoundedObjects {
            fields,
            methods,
            context,
        }
    }

    fn is_struct_implements_interface_fields(
        &self,
        struct_fields: &FieldsMap,
        namespace: &Namespace,
    ) -> Result<(), Vec<(&StrId, PartialConcreteInterfaceSpecsCheckError)>> {
        let struct_fields_map_ref = struct_fields.core_ref();
        let mut errors: Vec<(&StrId, PartialConcreteInterfaceSpecsCheckError)> = vec![];
        for (interface_field_name, (interface_field_ty, _)) in self.fields.core_ref() {
            match struct_fields_map_ref.get(interface_field_name) {
                Some((struct_field_ty, range)) => {
                    let concrete_interface_field_ty =
                        interface_field_ty.concretize(self.context, namespace);
                    if !struct_field_ty.is_eq(&concrete_interface_field_ty, namespace) {
                        errors.push((
                            interface_field_name,
                            PartialConcreteInterfaceSpecsCheckError::FieldTypeEquivalenceCheckFailed(
                                concrete_interface_field_ty.clone(),
                                struct_field_ty.clone(),
                                *range,
                            ),
                        ));
                    }
                }
                None => errors.push((
                    interface_field_name,
                    PartialConcreteInterfaceSpecsCheckError::MissingField,
                )),
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(())
    }

    fn compare_interface_method_with_struct_method(
        &self,
        interface_method_callable_data: &CallableData,
        struct_method_callable_data: &CallableData,
        range: TextRange,
        namespace: &Namespace,
    ) -> Result<(), PartialConcreteInterfaceSpecsCheckError> {
        let interface_method_generic_ty_decls = &interface_method_callable_data.generics();
        let struct_method_generic_ty_decls = &struct_method_callable_data.generics();
        match interface_method_generic_ty_decls {
            Some(interface_method_generic_ty_decls) => {
                match struct_method_generic_ty_decls {
                    Some(struct_method_generic_ty_decls) => {
                        // check if number of generic type declarations match
                        if interface_method_generic_ty_decls.len()
                            != struct_method_generic_ty_decls.len()
                        {
                            return Err(PartialConcreteInterfaceSpecsCheckError::GenericTypesDeclarationCheckFailed(range));
                        }
                        // check if the interface bounds each generic type in the declaration match
                        let generic_ty_decls_len = interface_method_generic_ty_decls.len();
                        for index in 0..generic_ty_decls_len {
                            let interface_generic_bound =
                                interface_method_generic_ty_decls.interface_bounds(index);
                            let struct_generic_bound =
                                struct_method_generic_ty_decls.interface_bounds(index);
                            if !interface_generic_bound.is_eq(struct_generic_bound, namespace) {
                                return Err(PartialConcreteInterfaceSpecsCheckError::GenericTypesDeclarationCheckFailed(range));
                            }
                        }
                        // check if prototypes match
                        if !interface_method_callable_data
                            .structural_prototype()
                            .is_structurally_eq(
                                &struct_method_callable_data.structural_prototype(),
                                self.context,
                                namespace,
                            )
                        {
                            return Err(PartialConcreteInterfaceSpecsCheckError::PrototypeEquivalenceCheckFailed(range));
                        }
                        Ok(())
                    }
                    None => Err(
                        PartialConcreteInterfaceSpecsCheckError::GenericTypesDeclarationExpected(
                            range,
                        ),
                    ),
                }
            }
            None => match struct_method_generic_ty_decls {
                Some(_) => Err(
                    PartialConcreteInterfaceSpecsCheckError::GenericTypesDeclarationNotExpected(
                        range,
                    ),
                ),
                None => {
                    if !interface_method_callable_data
                        .structural_prototype()
                        .is_structurally_eq(
                            &struct_method_callable_data.structural_prototype(),
                            self.context,
                            namespace,
                        )
                    {
                        return Err(PartialConcreteInterfaceSpecsCheckError::PrototypeEquivalenceCheckFailed(range));
                    }
                    Ok(())
                }
            },
        }
    }

    fn is_struct_implements_interface_methods(
        &self,
        struct_methods: &MethodsMap,
        namespace: &Namespace,
    ) -> Result<(), Vec<(&StrId, PartialConcreteInterfaceSpecsCheckError)>> {
        let struct_methods_map_ref = struct_methods.core_ref();
        let mut errors: Vec<(&StrId, PartialConcreteInterfaceSpecsCheckError)> = vec![];
        for (interface_method_name, (interface_method_callable_data, _)) in self.methods.core_ref()
        {
            match struct_methods_map_ref.get(interface_method_name) {
                Some((struct_method_callable_data, range)) => {
                    if let Err(err) = self.compare_interface_method_with_struct_method(
                        interface_method_callable_data,
                        struct_method_callable_data,
                        *range,
                        namespace,
                    ) {
                        errors.push((interface_method_name, err));
                    }
                }
                None => errors.push((
                    interface_method_name,
                    PartialConcreteInterfaceSpecsCheckError::MissingMethod,
                )),
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(())
    }

    pub fn is_struct_implements_interface_specs(
        &self,
        struct_fields: &FieldsMap,
        struct_methods: &MethodsMap,
        namespace: &Namespace,
    ) -> Result<(), Vec<(&StrId, PartialConcreteInterfaceSpecsCheckError)>> {
        let _ = self.is_struct_implements_interface_fields(struct_fields, namespace)?;
        let _ = self.is_struct_implements_interface_methods(struct_methods, namespace)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct InterfaceSymbolData(SymbolIndex<InterfaceData>);

impl AbstractSymbol for InterfaceSymbolData {
    type SymbolTy = InterfaceData;

    fn symbol_index(&self) -> SymbolIndex<Self::SymbolTy>
    where
        <Self as AbstractSymbol>::SymbolTy: IsInitialized,
    {
        self.0
    }

    fn entry(&self) -> SymbolDataEntry {
        SymbolDataEntry::Interface(self.0)
    }

    fn check_generic_ty_args(
        &self,
        concrete_types: Option<&TurbofishTypes>,
        ty_ranges: Option<&Vec<TextRange>>,
        is_concrete_types_none_allowed: bool,
        context: TypeStringifyContext,
    ) -> Result<(), GenericTypeArgsCheckError> {
        debug_assert!(!is_concrete_types_none_allowed);
        let interface_data = context
            .namespace()
            .interfaces_ref()
            .symbol_ref(self.0)
            .data_ref();
        let generic_ty_decls = interface_data.generics();
        check_concrete_types_bounded_by_interfaces(
            generic_ty_decls,
            concrete_types,
            ty_ranges,
            false,
            context,
        )
    }

    fn mangled_name(&self, _namespace: &Namespace) -> MangledIdentifierName<InterfaceData> {
        unreachable!()
    }
}

impl From<SymbolIndex<InterfaceData>> for InterfaceSymbolData {
    fn from(value: SymbolIndex<InterfaceData>) -> Self {
        InterfaceSymbolData(value)
    }
}
