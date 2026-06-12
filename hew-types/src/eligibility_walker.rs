//! Shared aggregate-field eligibility walker for equality and hash predicates.
//!
//! Both [`crate::eq_eligibility`] and [`crate::hash_eligibility`] delegate
//! named-type field traversal here. This satisfies the `collection-traversal-owner`
//! LESSONS invariant: a single traversal path for Copy-aggregate eligibility checks,
//! not duplicated across every predicate.
//!
//! The walker operates on a `TypeDef`'s `fields` map in deterministic sorted order
//! so that the first-rejection result is stable across runs.

use crate::check::TypeDef;
use crate::ty::Ty;
use std::collections::HashMap;

/// Walk the fields of a `TypeDef` in deterministic sorted order, applying
/// `check_field` to each. Returns the first `Some(E)` rejection produced by
/// `check_field`, or `None` if every field passes.
///
/// # Design note
///
/// A bare `fn` pointer is used instead of `impl Fn` so that callers can pass
/// their own free-function eligibility check that recurses through the same call
/// path. A closure would prevent recursion without boxing; a `fn` pointer is
/// zero-cost and composable.
pub(crate) fn walk_fields_for_eligibility<E>(
    type_def: &TypeDef,
    type_defs: &HashMap<String, TypeDef>,
    check_field: fn(&Ty, &HashMap<String, TypeDef>) -> Option<E>,
) -> Option<E> {
    let mut field_names: Vec<&String> = type_def.fields.keys().collect();
    field_names.sort();
    field_names
        .into_iter()
        .filter_map(|name| type_def.fields.get(name))
        .find_map(|field_ty| check_field(field_ty, type_defs))
}
