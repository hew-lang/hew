//! An owning pointer for the edges a source chain repeats.
//!
//! A postfix chain such as `a.b().c()[0]?` is flat source but one tree level
//! per link: each link owns its receiver. Built in a loop, it can be as deep as
//! the source is long, so the derived traversals of the tree must not recurse
//! natively once per link. A [`Chain`] edge drops its spine in a loop, and
//! clones, compares, prints and serialises through the stack-growth helper.
//! Edges the parser builds by recursion stay ordinary boxes: their depth is
//! bounded by the nesting limit.

use std::fmt;
use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::stack::ensure_sufficient_stack;

/// A node whose chain edge can be taken out, leaving the node shallow.
pub trait Spine: Sized {
    /// Take out one of this node's chain edges, if it still has one. Called
    /// until it returns `None`.
    fn detach(&mut self) -> Option<Chain<Self>>;
}

/// An owning pointer to the next link of a chain. Empty only while the
/// spine above it is being dropped.
pub struct Chain<T: Spine>(Option<Box<T>>);

impl<T: Spine> Chain<T> {
    /// Own `value` as the next link.
    pub fn new(value: T) -> Self {
        Self(Some(Box::new(value)))
    }

    /// The owned link.
    ///
    /// # Panics
    ///
    /// Never for a chain reachable from a live node.
    #[must_use]
    pub fn into_inner(mut self) -> T {
        *self.0.take().expect("a live chain edge owns its link")
    }

    /// Take the link out, leaving the edge empty for a node being dropped.
    #[must_use]
    pub fn take(&mut self) -> Option<Self> {
        self.0.take().map(|link| Self(Some(link)))
    }
}

impl<T: Spine> Drop for Chain<T> {
    fn drop(&mut self) {
        // Each link is dropped with its chain edges already taken out, so no
        // drop recurses into the next link. A link with a second edge (an
        // optional chain's receiver and body) leaves it pending.
        let mut pending = Vec::new();
        let mut next = self.0.take();
        while let Some(mut link) = next.take().or_else(|| pending.pop()) {
            while let Some(edge) = link.detach().and_then(|mut edge| edge.0.take()) {
                if next.is_none() {
                    next = Some(edge);
                } else {
                    pending.push(edge);
                }
            }
        }
    }
}

impl<T: Spine> Deref for Chain<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.as_deref().expect("a live chain edge owns its link")
    }
}

impl<T: Spine> DerefMut for Chain<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0
            .as_deref_mut()
            .expect("a live chain edge owns its link")
    }
}

impl<T: Spine> AsRef<T> for Chain<T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: Spine + Clone> Clone for Chain<T> {
    fn clone(&self) -> Self {
        ensure_sufficient_stack(|| Self::new((**self).clone()))
    }
}

impl<T: Spine + PartialEq> PartialEq for Chain<T> {
    fn eq(&self, other: &Self) -> bool {
        ensure_sufficient_stack(|| **self == **other)
    }
}

impl<T: Spine + Eq> Eq for Chain<T> {}

impl<T: Spine + std::hash::Hash> std::hash::Hash for Chain<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ensure_sufficient_stack(|| (**self).hash(state));
    }
}

impl<T: Spine + fmt::Debug> fmt::Debug for Chain<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ensure_sufficient_stack(|| (**self).fmt(f))
    }
}

impl<T: Spine + Serialize> Serialize for Chain<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        ensure_sufficient_stack(|| (**self).serialize(serializer))
    }
}

impl<'de, T: Spine + Deserialize<'de>> Deserialize<'de> for Chain<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        ensure_sufficient_stack(|| T::deserialize(deserializer).map(Self::new))
    }
}

#[cfg(test)]
mod tests {
    use super::{Chain, Spine};

    #[derive(Clone, Debug, PartialEq)]
    struct Link {
        value: u32,
        receiver: Option<Chain<Link>>,
    }

    impl Spine for Link {
        fn detach(&mut self) -> Option<Chain<Self>> {
            self.receiver.as_mut().and_then(Chain::take)
        }
    }

    fn spine(length: u32) -> Link {
        let mut link = Link {
            value: 0,
            receiver: None,
        };
        for value in 1..length {
            link = Link {
                value,
                receiver: Some(Chain::new(link)),
            };
        }
        link
    }

    #[test]
    fn a_long_spine_is_cloned_compared_and_dropped_on_a_small_stack() {
        // Every traversal of a long spine on a 256 KiB stack: drop
        // loops, and the others grow the stack instead of overflowing it.
        std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn(|| {
                let original = spine(20_000);
                let copy = original.clone();
                assert_eq!(original, copy);
                let mut root = &copy;
                let mut depth = 1;
                while let Some(next) = &root.receiver {
                    root = next;
                    depth += 1;
                }
                assert_eq!((depth, root.value), (20_000, 0));
            })
            .expect("spawn")
            .join()
            .expect("a long spine overflowed");
    }

    #[test]
    fn into_inner_hands_back_the_link() {
        let link = Chain::new(spine(3)).into_inner();
        assert_eq!(link.value, 2);
        assert_eq!(
            link.receiver.map(Chain::into_inner).map(|link| link.value),
            Some(1)
        );
    }
}
