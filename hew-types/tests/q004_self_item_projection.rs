//! Q004 regression: a trait method's `Self::Item` projection must stay a
//! deferred `Ty::AssocType` carrier when the method signature is re-resolved
//! *inside an impl body* (where an impl alias scope is active).
//!
//! Root cause (fixed in `check/resolution.rs`): the `Self::Bar` carrier path
//! was gated on `impl_alias_scopes.is_empty()`. An impl body-check (e.g.
//! `impl Iterator for Map { type Item = B; fn next … }`) pushes the impl's
//! `type Item = B` alias scope and then, for an inner `it.next()`, re-resolves
//! `Iterator::next`'s `-> Option<Self::Item>` from raw AST. With the scope
//! active the carrier path was skipped and `Self::Item` collapsed to the
//! impl's concrete `B`, mistyping the inner iterator's item and producing a
//! spurious "expected A, found B". The fix prefers the carrier whenever a
//! trait-method signature is being resolved
//! (`current_trait_for_self_projection` is set), independent of the alias
//! scope.
//!
//! These three cases lock the fix:
//!   * `q004_map_next_body_passes`     — the generic `Map<I, A, B>` adapter
//!     (inner item `A`, produced item `B`). FAILS pre-fix ("expected A,
//!     found B"); PASSES post-fix.
//!   * `q004_inherent_impl_next_passes` — a concrete `i64 → bool` adapter
//!     whose `Self::Item` (`bool`) differs from the inner item (`i64`).
//!     FAILS pre-fix ("expected i64, found bool" at `x > 0`); PASSES
//!     post-fix.
//!   * `q004_fold_body_passes`         — a free-function `fold` over
//!     `I: Iterator<Item = A>`. Free functions have no active impl alias
//!     scope, so this projected correctly both before and after the fix; it
//!     is a guard that the carrier still collapses to `A` in a terminal
//!     pump loop.

#![allow(
    clippy::missing_panics_doc,
    reason = "integration-test panics on assertion failure are intentional"
)]

mod common;

use common::typecheck_isolated;

/// The Q001-superseded trait surface as shipped in `std/builtins.hew`:
/// `next` takes a mutable `var self` receiver so an iterator can advance its
/// own cursor in place.
const ITER_PRELUDE: &str = r"
pub trait Iterator {
    type Item;

    fn next(var self) -> Option<Self::Item>;
}
";

#[test]
fn q004_map_next_body_passes() {
    // The generic lazy `Map` adapter. `self.iter` is moved into a `var`
    // local so the inner iterator's `next(var self)` write-back lands on a
    // mutable binding receiver; the advanced inner is stored back. The inner
    // item `<I as Iterator>::Item` must project to `A` (not the impl's
    // `type Item = B`) so `f: fn(A) -> B` accepts it.
    let src = format!(
        "{ITER_PRELUDE}{}",
        r"
pub type Map<I, A, B> {
    iter: I;
    f: fn(A) -> B;
}

impl<I, A, B> Iterator for Map<I, A, B> where I: Iterator<Item = A> {
    type Item = B;
    fn next(var self) -> Option<B> {
        var inner = self.iter;
        let result = match inner.next() {
            Some(x) => Some((self.f)(x)),
            None => None,
        };
        self.iter = inner;
        result
    }
}
"
    );
    let output = typecheck_isolated(&src);
    assert!(
        output.errors.is_empty(),
        "Map::next must type-check post-Q004 (inner item projects to `A`, not the impl's `B`); got: {:#?}",
        output.errors
    );
}

#[test]
fn q004_inherent_impl_next_passes() {
    // A concrete `i64 -> bool` adapter. `Self::Item` is `bool`, but the inner
    // `it.next()` must still yield the inner item type `i64` (via the
    // `I: Iterator<Item = i64>` bound) so the `x > 0` comparison type-checks.
    // Pre-fix the inner item collapsed to the impl's `bool`, rejecting
    // `x > 0`.
    let src = format!(
        "{ITER_PRELUDE}{}",
        r"
pub type IsPositive<I> {
    inner: I;
}

impl<I> Iterator for IsPositive<I> where I: Iterator<Item = i64> {
    type Item = bool;
    fn next(var self) -> Option<bool> {
        var it = self.inner;
        let result = match it.next() {
            Some(x) => Some(x > 0),
            None => None,
        };
        self.inner = it;
        result
    }
}
"
    );
    let output = typecheck_isolated(&src);
    assert!(
        output.errors.is_empty(),
        "inherent impl `next` must type-check post-Q004 (inner item projects to `i64`, not the impl's `bool`); got: {:#?}",
        output.errors
    );
}

#[test]
fn q004_fold_body_passes() {
    // A free-function terminal fold. No impl alias scope is active here, so
    // `iter.next()`'s `Self::Item` carrier already collapsed to `A` both
    // before and after the fix. This guards that the terminal pump loop keeps
    // projecting the item to `A` so `f: fn(B, A) -> B` accepts it.
    let src = format!(
        "{ITER_PRELUDE}{}",
        r"
pub fn fold<I, A, B>(it: I, init: B, f: fn(B, A) -> B) -> B where I: Iterator<Item = A> {
    var acc = init;
    var iter = it;
    loop {
        match iter.next() {
            Some(x) => {
                acc = f(acc, x);
            }
            None => {
                break;
            }
        }
    }
    acc
}
"
    );
    let output = typecheck_isolated(&src);
    assert!(
        output.errors.is_empty(),
        "free-function fold must type-check (item projects to `A`); got: {:#?}",
        output.errors
    );
}
