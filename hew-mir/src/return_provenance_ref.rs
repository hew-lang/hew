//! FROZEN REFERENCE — a verbatim, independently-executable copy of the
//! pre-refactor call-return freshness transfer (`return_value_may_alias_borrow`
//! and friends, captured at the base before the `return_alias_bits`
//! parameterization).
//!
//! # Why a frozen copy and not a re-derivation
//!
//! The Coarse leaf walk is now shared with the Precise driver
//! ([`super::return_alias_bits`] under [`super::CoarsePolicy`]). A stdlib-corpus
//! golden plus a handful of negatives cannot establish that the shared walk is
//! byte-identical over ALL HIR shapes / generated bodies / future inputs — and a
//! Coarse drift is a silent UAF regression in the funcupdate (#2420 base gate)
//! and reassign consumers. This module keeps the OLD transfer executable so the
//! `coarse_verdict_differential` harness can compare `(ItemId, bool)` for every
//! function in the corpus against the live path; any divergence fails.
//!
//! DO NOT "clean up" or refactor this file to call the new authority — that
//! defeats the entire purpose. It is a snapshot, updated only by an explicit,
//! reviewed baseline-refresh step.
//!
//! `#[cfg(test)]` only.

use std::collections::HashMap;

use hew_hir::{HirExpr, HirExprKind, HirFn, ResolvedRef};
use hew_types::ResolvedTy;

use crate::lower::collect_return_values_in_block;

/// Verbatim copy of the pre-refactor `compute_fn_returns_fresh_owner`.
pub fn compute_fn_returns_fresh_owner_ref(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
) -> HashMap<hew_hir::ItemId, bool> {
    let mut fresh: HashMap<hew_hir::ItemId, bool> = fns.keys().map(|&id| (id, false)).collect();
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            if fresh[&id] {
                continue;
            }
            if fn_body_returns_fresh_owner_ref(f, &fresh) {
                fresh.insert(id, true);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    fresh
}

/// Verbatim copy of the pre-refactor `fn_body_returns_fresh_owner`.
fn fn_body_returns_fresh_owner_ref(f: &HirFn, fresh: &HashMap<hew_hir::ItemId, bool>) -> bool {
    let mut return_values: Vec<&HirExpr> = Vec::new();
    collect_return_values_in_block(&f.body, &mut return_values);
    if let Some(tail) = &f.body.tail {
        if !matches!(tail.ty, ResolvedTy::Unit | ResolvedTy::Never) {
            return_values.push(tail);
        }
    }
    if return_values.is_empty() {
        return false;
    }
    return_values
        .iter()
        .all(|e| !return_value_may_alias_borrow_ref(e, fresh))
}

/// Verbatim copy of the pre-refactor `return_value_may_alias_borrow` — the leaf
/// this lane parameterizes. This is the transfer the differential pins.
fn return_value_may_alias_borrow_ref(
    expr: &HirExpr,
    fresh: &HashMap<hew_hir::ItemId, bool>,
) -> bool {
    match &expr.kind {
        HirExprKind::Block(block) => block
            .tail
            .as_deref()
            .is_none_or(|t| return_value_may_alias_borrow_ref(t, fresh)),
        HirExprKind::If {
            then_expr,
            else_expr,
            ..
        } => {
            return_value_may_alias_borrow_ref(then_expr, fresh)
                || else_expr
                    .as_deref()
                    .is_none_or(|e| return_value_may_alias_borrow_ref(e, fresh))
        }
        HirExprKind::Match { arms, .. } => {
            arms.is_empty()
                || arms
                    .iter()
                    .any(|arm| return_value_may_alias_borrow_ref(&arm.body, fresh))
        }
        HirExprKind::Return { value } => value
            .as_deref()
            .is_none_or(|v| return_value_may_alias_borrow_ref(v, fresh)),
        HirExprKind::RecordCloneCall { .. }
        | HirExprKind::Index { .. }
        | HirExprKind::Slice { .. }
        | HirExprKind::Literal(_) => false,
        HirExprKind::StructInit { fields, base, .. } => {
            fields
                .iter()
                .any(|(_, v)| return_value_may_alias_borrow_ref(v, fresh))
                || base
                    .as_deref()
                    .is_some_and(|b| return_value_may_alias_borrow_ref(b, fresh))
        }
        HirExprKind::TupleLiteral { elements } => elements
            .iter()
            .any(|e| return_value_may_alias_borrow_ref(e, fresh)),
        HirExprKind::MachineVariantCtor { payload, .. } => payload.as_ref().is_some_and(|fields| {
            fields
                .iter()
                .any(|(_, v)| return_value_may_alias_borrow_ref(v, fresh))
        }),
        HirExprKind::Call { callee, args } => {
            !callee_is_resolved_item_ref(callee)
                || (!callee_returns_fresh_owner_ref(callee, fresh)
                    && args
                        .iter()
                        .any(|a| return_value_may_alias_borrow_ref(a, fresh)))
        }
        HirExprKind::FieldAccess { object, .. } => return_value_may_alias_borrow_ref(object, fresh),
        HirExprKind::TupleIndex { tuple, .. } => return_value_may_alias_borrow_ref(tuple, fresh),
        _ => true,
    }
}

/// Verbatim copy of the pre-refactor `callee_returns_fresh_owner`.
fn callee_returns_fresh_owner_ref(
    callee: &HirExpr,
    fresh: &HashMap<hew_hir::ItemId, bool>,
) -> bool {
    if let HirExprKind::BindingRef {
        resolved: ResolvedRef::Item(item_id),
        ..
    } = &callee.kind
    {
        fresh.get(item_id).copied().unwrap_or(true)
    } else {
        false
    }
}

/// Verbatim copy of the pre-refactor `callee_is_resolved_item`.
fn callee_is_resolved_item_ref(callee: &HirExpr) -> bool {
    matches!(
        &callee.kind,
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Item(_),
            ..
        }
    )
}
