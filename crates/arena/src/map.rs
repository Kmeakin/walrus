use crate::Idx;
use std::{fmt, marker::PhantomData, ops::Index};

/// A map from arena indexes to some other type.
/// Space requirement is O(highest index).
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArenaMap<K, V> {
    data: Vec<Option<V>>,
    _ty: PhantomData<K>,
}

impl<T, V: fmt::Debug> fmt::Debug for ArenaMap<Idx<T>, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T, V> ArenaMap<Idx<T>, V> {
    pub fn new() -> Self { Self::default() }

    /// Inserts a value associated with a given arena index into the map.
    pub fn insert(&mut self, idx: Idx<T>, t: V) {
        let raw = usize::from(idx);
        self.data
            .resize_with((raw + 1).max(self.data.len()), || None);
        self.data[raw] = Some(t);
    }

    /// Returns a reference to the value associated with the provided index
    /// if it is present.
    pub fn get(&self, idx: Idx<T>) -> Option<&V> {
        self.data.get(usize::from(idx)).and_then(Option::as_ref)
    }

    /// Returns a mutable reference to the value associated with the provided
    /// index if it is present.
    pub fn get_mut(&mut self, idx: Idx<T>) -> Option<&mut V> {
        self.data.get_mut(usize::from(idx)).and_then(Option::as_mut)
    }

    /// Returns an iterator over the values in the map.
    pub fn values(&self) -> impl Iterator<Item = &V> { self.data.iter().filter_map(Option::as_ref) }

    /// Returns an iterator over mutable references to the values in the map.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.data.iter_mut().filter_map(Option::as_mut)
    }

    /// Returns an iterator over the arena indexes and values in the map.
    pub fn iter(&self) -> impl Iterator<Item = (Idx<T>, &V)> {
        self.data
            .iter()
            .enumerate()
            .filter_map(|(idx, o)| Some((idx.into(), o.as_ref()?)))
    }

    /// Returns an iterator over the arena indexes and values in the map.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Idx<T>, &mut V)> {
        self.data
            .iter_mut()
            .enumerate()
            .filter_map(|(idx, o)| Some((idx.into(), o.as_mut()?)))
    }
}

impl<T, V> Default for ArenaMap<Idx<T>, V> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            _ty: PhantomData,
        }
    }
}

impl<T, V> Index<Idx<V>> for ArenaMap<Idx<V>, T> {
    type Output = T;
    fn index(&self, idx: Idx<V>) -> &T { self.data[idx.raw as usize].as_ref().unwrap() }
}
