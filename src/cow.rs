// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: 2025 Cedric Hammes <contact@cach30verfl0w.net>

#[cfg(feature = "alloc")]
use crate::alloc::borrow::ToOwned;

#[cfg(not(feature = "alloc"))]
pub trait ToOwned {}

#[cfg(not(feature = "alloc"))]
impl<T: ?Sized> ToOwned for T {}

pub(crate) enum Cow<'a, B: ?Sized + ToOwned + 'a> {
    Borrowed(&'a B),
    #[cfg(feature = "alloc")]
    Owned(<B as ToOwned>::Owned),
}

#[cfg(feature = "alloc")]
impl<'a, B: ?Sized + ToOwned> AsRef<B> for Cow<'a, B>
where
    B::Owned: AsRef<B>,
{
    fn as_ref(&self) -> &B {
        match self {
            Cow::Borrowed(b) => b,
            Cow::Owned(o) => o.as_ref(),
        }
    }
}

#[cfg(not(feature = "alloc"))]
impl<'a, B: ?Sized> AsRef<B> for Cow<'a, B>
{
    fn as_ref(&self) -> &B {
        match self {
            Cow::Borrowed(b) => b,
        }
    }
}
