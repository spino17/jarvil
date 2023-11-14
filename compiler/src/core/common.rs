use std::ops::Deref;

#[derive(Debug)]
pub enum RefOrOwned<'a, T> {
    Ref(&'a T),
    Owned(T),
}

impl<'a, T> From<T> for RefOrOwned<'a, T> {
    fn from(value: T) -> Self {
        RefOrOwned::Owned(value)
    }
}

impl<'a, T> From<&'a T> for RefOrOwned<'a, T> {
    fn from(value: &'a T) -> Self {
        RefOrOwned::Ref(value)
    }
}

impl<'a, T> Deref for RefOrOwned<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        match self {
            RefOrOwned::Owned(val) => val,
            RefOrOwned::Ref(val) => val,
        }
    }
}
