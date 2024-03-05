#[derive(Debug)]
pub struct CallablePrototypeRef<'a>(RefOrOwned<'a, CallablePrototypeData>);

/*impl<'a> Deref for CallablePrototypeRef<'a> {
    type Target = RefOrOwned<'a, CallablePrototypeData>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}*/

impl<'a> From<CallablePrototypeRef<'a>> for RefOrOwned<'a, CallablePrototypeData> {
    fn from(value: CallablePrototypeRef<'a>) -> Self {
        value.0
    }
}

impl<'a> CallablePrototypeRef<'a> {
    //pub fn return_ty(&self) -> TypeRef<'a> {
    //    match &self.0 {
    //        RefOrOwned::Ref(prototype) => RefOrOwned::Ref(&prototype.return_type),
    //        RefOrOwned::Owned(prototype) => RefOrOwned::Owned(prototype.return_type.clone()),
    //    }
    //}

    pub fn is_received_params_valid(
        &self,
        type_checker: &TypeChecker,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<TypeRef<'a>, PrototypeEquivalenceCheckError> {
        match &self.0 {
            RefOrOwned::Ref(prototype) => {
                let expected_params = &prototype.params;
                let return_type = &prototype.return_type;
                type_checker.check_params_type_and_count(expected_params, received_params)?;
                Ok(RefOrOwned::Ref(return_type))
            }
            RefOrOwned::Owned(prototype) => {
                let expected_params = &prototype.params;
                let return_type = &prototype.return_type;
                type_checker.check_params_type_and_count(expected_params, received_params)?;
                Ok(RefOrOwned::Owned(return_type.clone()))
            }
        }
    }
}

pub type TypeRef<'a> = RefOrOwned<'a, Type>;
pub enum TypeLongShortRef<'a, 'b> {
    Long(&'a Type),
    Short(&'b Type),
}

impl<'a, 'b> Deref for TypeLongShortRef<'a, 'b> {
    type Target = Type;
    fn deref(&self) -> &Self::Target {
        match self {
            TypeLongShortRef::Short(value) => value,
            TypeLongShortRef::Long(value) => value,
        }
    }
}

impl<'a, 'b> From<TypeLongShortRef<'a, 'b>> for RefOrOwned<'a, Type> {
    fn from(value: TypeLongShortRef<'a, 'b>) -> Self {
        match value {
            TypeLongShortRef::Long(value) => RefOrOwned::Ref(value),
            TypeLongShortRef::Short(value) => RefOrOwned::Owned(value.clone()),
        }
    }
}
