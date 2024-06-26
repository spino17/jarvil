use super::symbol::core::SymbolIndex;
use super::traits::InstantiationContext;
use crate::types::core::{Type, TypeStringifyContext};
use crate::types::traits::TypeLike;
use std::ops::Index;
use std::slice::Iter;

#[derive(Debug, Clone)]
pub struct TurbofishTypes(Vec<Type>);

impl TurbofishTypes {
    pub fn new(concrete_types: Vec<Type>) -> Self {
        TurbofishTypes(concrete_types)
    }

    pub fn core_ref(&self) -> &Vec<Type> {
        &self.0
    }

    pub fn iter(&self) -> Iter<Type> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn to_string(&self, context: TypeStringifyContext) -> String {
        let mut s = "".to_string();
        let concrete_types = &self.0;
        let len = concrete_types.len();

        s.push_str(&concrete_types[0].to_string(context));

        for i in 1..len {
            s.push_str(&format!(", {}", concrete_types[i].to_string(context)));
        }

        s
    }
}

impl Index<usize> for TurbofishTypes {
    type Output = Type;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Debug)]
pub struct ConcreteSymbolIndex<T> {
    index: SymbolIndex<T>,
    concrete_types: Option<TurbofishTypes>, // This will be `None` for symbol data which does not have any generic type params
}

impl<T> Clone for ConcreteSymbolIndex<T> {
    fn clone(&self) -> Self {
        ConcreteSymbolIndex {
            index: self.index,
            concrete_types: self.concrete_types.clone(),
        }
    }
}

impl<T> ConcreteSymbolIndex<T> {
    pub fn new(symbol_index: SymbolIndex<T>, concrete_types: Option<TurbofishTypes>) -> Self {
        ConcreteSymbolIndex {
            index: symbol_index,
            concrete_types,
        }
    }

    pub fn symbol_index(&self) -> SymbolIndex<T> {
        self.index
    }

    pub fn concrete_types(&self) -> Option<&TurbofishTypes> {
        self.concrete_types.as_ref()
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct TypeGenericsInstantiationContext<'a> {
    args: Option<&'a TurbofishTypes>,
}

impl<'a> TypeGenericsInstantiationContext<'a> {
    pub fn new(args: Option<&'a TurbofishTypes>) -> Self {
        TypeGenericsInstantiationContext { args }
    }

    pub fn into_method_context(&self) -> MethodGenericsInstantiationContext<'a> {
        MethodGenericsInstantiationContext {
            bounding_ty_args: self.args,
            local_args: None,
        }
    }

    pub fn attach_local_method_context(
        &self,
        local_args: Option<&'a TurbofishTypes>,
    ) -> MethodGenericsInstantiationContext<'a> {
        MethodGenericsInstantiationContext {
            bounding_ty_args: self.args,
            local_args,
        }
    }
}

impl<'a> InstantiationContext<'a> for TypeGenericsInstantiationContext<'a> {
    fn is_empty(&self) -> bool {
        self.args.is_none()
    }

    fn ty_generics_instantiation_args(&self) -> Option<&'a TurbofishTypes> {
        self.args
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct FunctionGenericsInstantiationContext<'a> {
    args: Option<&'a TurbofishTypes>,
}

impl<'a> FunctionGenericsInstantiationContext<'a> {
    pub fn new(args: Option<&'a TurbofishTypes>) -> Self {
        FunctionGenericsInstantiationContext { args }
    }

    pub fn into_method_context(&self) -> MethodGenericsInstantiationContext<'a> {
        MethodGenericsInstantiationContext {
            bounding_ty_args: None,
            local_args: self.args,
        }
    }
}

impl<'a> InstantiationContext<'a> for FunctionGenericsInstantiationContext<'a> {
    fn is_empty(&self) -> bool {
        self.args.is_none()
    }

    fn callable_generics_instantiation_args(&self) -> Option<&'a TurbofishTypes> {
        self.args
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct MethodGenericsInstantiationContext<'a> {
    bounding_ty_args: Option<&'a TurbofishTypes>,
    local_args: Option<&'a TurbofishTypes>,
}

impl<'a> MethodGenericsInstantiationContext<'a> {
    pub fn new(
        bounding_ty_args: Option<&'a TurbofishTypes>,
        local_args: Option<&'a TurbofishTypes>,
    ) -> Self {
        MethodGenericsInstantiationContext {
            bounding_ty_args,
            local_args,
        }
    }
}

impl<'a> InstantiationContext<'a> for MethodGenericsInstantiationContext<'a> {
    fn is_empty(&self) -> bool {
        self.bounding_ty_args.is_none() && self.local_args.is_none()
    }

    fn ty_generics_instantiation_args(&self) -> Option<&'a TurbofishTypes> {
        self.bounding_ty_args
    }

    fn callable_generics_instantiation_args(&self) -> Option<&'a TurbofishTypes> {
        self.local_args
    }
}
