// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: 2025 Cedric Hammes <contact@cach30verfl0w.net>

use core::fmt::Formatter;
#[cfg(feature = "alloc")]
use crate::alloc::borrow::ToOwned;
use crate::std::{fmt::Debug, fmt};

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
impl<'a, B: ?Sized + ToOwned + PartialEq> PartialEq for Cow<'a, B>
where
    B::Owned: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Cow::Borrowed(a), Cow::Borrowed(b)) => a == b,
            (Cow::Owned(a), Cow::Owned(b)) => a == b,
            _ => false
        }
    }
}

#[cfg(not(feature = "alloc"))]
impl<'a, B: ?Sized + PartialEq> PartialEq for Cow<'a, B> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Cow::Borrowed(a), Cow::Borrowed(b)) => a == b
        }
    }
}

#[cfg(feature = "alloc")]
impl<'a, B: ?Sized + ToOwned + Debug> Debug for Cow<'a, B>
where
    B::Owned: Debug
{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Cow::Borrowed(b) => formatter.debug_tuple("Borrowed").field(b).finish(),
            Cow::Owned(b) => formatter.debug_tuple("Owned").field(b).finish()
        }
    }
}

#[cfg(not(feature = "alloc"))]
impl<'a, B: ?Sized + Debug> Debug for Cow<'a, B> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Cow::Borrowed(b) => formatter.debug_tuple("Borrowed").field(b).finish()
        }
    }
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
