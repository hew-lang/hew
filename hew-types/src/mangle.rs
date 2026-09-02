//! Symbol rendering for a resolved type (`docs/internal/ir-ladder.md` §5.1).
//!
//! This renders a *symbol* from a [`TypeInstanceKey`](crate::type_facts::TypeInstanceKey)
//! and is never itself a lookup key: the fact tables compare their keys
//! structurally, so the one-authority ban on name-keyed joins holds.

use crate::resolved_ty::{mangle_resolved_ty_segment, ResolvedTy, TypeParamMangle};

/// Render a single `ResolvedTy` as a mangled fragment.
///
/// Compound structure is encoded with `$`-letter tokens: `$l`/`$g` delimit
/// named type arguments, `$x`/`$g` delimit structural compounds, `$c`
/// separates list items, `$r` marks function returns, `$a` marks trait-object
/// associated bindings, and `$m` replaces name qualifiers. Every token starts
/// with `$`, which Hew identifiers cannot contain, so the rendering is
/// structural and injective over the supported `ResolvedTy` identity
/// dimensions. This wrapper selects the shared encoder's total `TypeParam`
/// mode, preserving `typeparam$x{name}$g` for speculative HIR keys.
///
/// # Panics
///
/// Panics if the shared encoder violates the contract that
/// [`TypeParamMangle::Concrete`] renders every [`ResolvedTy`].
#[must_use]
pub fn mangle_resolved_ty(ty: &ResolvedTy) -> String {
    mangle_resolved_ty_segment(ty, TypeParamMangle::Concrete)
        .expect("Concrete TypeParam mangling must render every ResolvedTy")
}
