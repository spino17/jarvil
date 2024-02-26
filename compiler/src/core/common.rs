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

pub enum LongShortRef<'a, 'b, T> {
    Long(&'a T),
    Short(&'b T),
}

impl<'a, 'b, T> Deref for LongShortRef<'a, 'b, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        match self {
            LongShortRef::Short(value) => *value,
            LongShortRef::Long(value) => *value,
        }
    }
}

impl<'a, 'b, T: Clone> LongShortRef<'a, 'b, T> {
    pub fn clone_short(&self) -> RefOrOwned<'a, T> {
        match self {
            LongShortRef::Long(value) => RefOrOwned::Ref(*value),
            LongShortRef::Short(value) => RefOrOwned::Owned((*value).clone()),
        }
    }
}
