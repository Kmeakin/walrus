#![allow(clippy::cast_possible_truncation)]

use std::{cmp, fmt, hash, marker::PhantomData};

/// The index of a value allocated in an arena that holds `T`s.
pub struct Idx<T> {
    pub(crate) raw: u32,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Idx<T> {
    pub fn new(raw: u32) -> Self {
        Self {
            raw,
            _ty: PhantomData,
        }
    }
}

impl<T> From<u32> for Idx<T> {
    fn from(idx: u32) -> Self { Self::new(idx) }
}

impl<T> From<usize> for Idx<T> {
    fn from(idx: usize) -> Self { Self::new(idx as u32) }
}

impl<T> From<Idx<T>> for u32 {
    fn from(idx: Idx<T>) -> Self { idx.raw }
}
impl<T> From<Idx<T>> for usize {
    fn from(idx: Idx<T>) -> Self { idx.raw as Self }
}

impl<T> fmt::Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:?}", self.raw) }
}
impl<T> fmt::Display for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.raw) }
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self { *self }
}
impl<T> Copy for Idx<T> {}

impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool { self.raw == other.raw }
}
impl<T> Eq for Idx<T> {}

impl<T> PartialOrd for Idx<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.raw.partial_cmp(&other.raw)
    }
}

impl<T> Ord for Idx<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering { self.raw.cmp(&other.raw) }
}

impl<T> hash::Hash for Idx<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) { self.raw.hash(state) }
}
