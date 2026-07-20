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

use hew_hir::{BindingId, HirBlock, HirExpr, HirExprKind, HirFn, HirStmtKind, ResolvedRef};
use hew_types::ResolvedTy;

use crate::lower::collect_return_values_in_block;
use crate::return_provenance::{
    expr_mentions_binding, method_receiver_and_args, moved_binding_ref, place_root_binding,
    stmt_mentions_binding, SeeThroughScope,
};

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
    let root = SeeThroughScope {
        block: &f.body,
        parent: None,
    };
    return_values
        .iter()
        .all(|e| !return_value_may_alias_borrow_ref(e, Some(&root), fresh))
}

/// Verbatim copy of the pre-refactor `return_value_may_alias_borrow` — the leaf
/// this module parameterizes. This is the transfer the differential pins.
fn return_value_may_alias_borrow_ref(
    expr: &HirExpr,
    scope: Option<&SeeThroughScope>,
    fresh: &HashMap<hew_hir::ItemId, bool>,
) -> bool {
    match &expr.kind {
        HirExprKind::Block(block) => match &block.tail {
            None => true,
            Some(tail) => {
                let child = SeeThroughScope {
                    block,
                    parent: scope,
                };
                return_value_may_alias_borrow_ref(tail, Some(&child), fresh)
            }
        },
        HirExprKind::If {
            then_expr,
            else_expr,
            ..
        } => {
            return_value_may_alias_borrow_ref(then_expr, scope, fresh)
                || else_expr
                    .as_deref()
                    .is_none_or(|e| return_value_may_alias_borrow_ref(e, scope, fresh))
        }
        HirExprKind::Match { arms, .. } => {
            arms.is_empty()
                || arms
                    .iter()
                    .any(|arm| return_value_may_alias_borrow_ref(&arm.body, scope, fresh))
        }
        HirExprKind::Return { value } => value
            .as_deref()
            .is_none_or(|v| return_value_may_alias_borrow_ref(v, scope, fresh)),
        HirExprKind::RecordCloneCall { .. }
        | HirExprKind::Index { .. }
        | HirExprKind::Slice { .. }
        | HirExprKind::Literal(_) => false,
        HirExprKind::StructInit { fields, base, .. } => {
            fields
                .iter()
                .any(|(_, v)| return_value_may_alias_borrow_ref(v, scope, fresh))
                || base
                    .as_deref()
                    .is_some_and(|b| return_value_may_alias_borrow_ref(b, scope, fresh))
        }
        HirExprKind::TupleLiteral { elements } => elements
            .iter()
            .any(|e| return_value_may_alias_borrow_ref(e, scope, fresh)),
        HirExprKind::MachineVariantCtor { payload, .. } => payload.as_ref().is_some_and(|fields| {
            fields
                .iter()
                .any(|(_, v)| return_value_may_alias_borrow_ref(v, scope, fresh))
        }),
        HirExprKind::Call { callee, args } => {
            !callee_is_resolved_item_ref(callee)
                || (!callee_returns_fresh_owner_ref(callee, fresh)
                    && args
                        .iter()
                        .any(|a| return_value_may_alias_borrow_ref(a, scope, fresh)))
        }
        HirExprKind::FieldAccess { object, .. } => {
            return_value_may_alias_borrow_ref(object, scope, fresh)
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            return_value_may_alias_borrow_ref(tuple, scope, fresh)
        }
        // Fresh-owner see-through (fix (i)) — the independent bool twin of
        // `see_through_let_binding_bits`. Reuses the shared structural
        // scope/append/mention plumbing so only the may-alias recursion is
        // reimplemented; the `coarse_verdict_differential` pin then proves the two
        // see-throughs agree over the whole corpus.
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => scope
            .and_then(|s| s.resolve(*id))
            .and_then(|block| block_let_may_alias_ref(block, *id, scope, fresh, None))
            .unwrap_or(true),
        _ => true,
    }
}

/// Independent bool twin of `see_through_let_binding_bits`: `Some(may_alias)`
/// for a single-assignment `let`-bound local seen through to its init plus
/// interior appends and direct immutable single-move chains; `None` (keep the
/// fail-closed leaf) for a `var`, a reassignment, or any other use of `id`.
/// `may_alias` mirrors `!bits.is_fresh()`.
fn block_let_may_alias_ref(
    block: &HirBlock,
    id: BindingId,
    scope: Option<&SeeThroughScope>,
    fresh: &HashMap<hew_hir::ItemId, bool>,
    forwarded_to: Option<BindingId>,
) -> Option<bool> {
    let mut init_may_alias: Option<bool> = None;
    let mut init_is_move = false;
    let mut content_may_alias = false;
    let mut saw_forward = false;
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(binding, init) if binding.id == id => {
                if binding.mutable {
                    return None;
                }
                let init = init.as_ref()?;
                let move_source = moved_binding_ref(init);
                init_is_move = move_source.is_some();
                init_may_alias = Some(if let Some(source_id) = move_source {
                    scope
                        .and_then(|s| s.resolve(source_id))
                        .and_then(|source_block| {
                            block_let_may_alias_ref(source_block, source_id, scope, fresh, Some(id))
                        })
                        .unwrap_or_else(|| return_value_may_alias_borrow_ref(init, scope, fresh))
                } else {
                    return_value_may_alias_borrow_ref(init, scope, fresh)
                });
            }
            HirStmtKind::Assign { target, .. } if place_root_binding(target) == Some(id) => {
                return None;
            }
            _ => {
                if let HirStmtKind::Let(binding, Some(init)) = &stmt.kind {
                    if Some(binding.id) == forwarded_to
                        && !binding.mutable
                        && moved_binding_ref(init) == Some(id)
                    {
                        if init_may_alias.is_none() || saw_forward {
                            return None;
                        }
                        saw_forward = true;
                        continue;
                    }
                }
                if let HirStmtKind::Expr(e) = &stmt.kind {
                    if let Some((receiver, args)) = method_receiver_and_args(e) {
                        if place_root_binding(receiver) == Some(id) {
                            if forwarded_to.is_some() || init_is_move {
                                return None;
                            }
                            for arg in &args {
                                if expr_mentions_binding(arg, id) {
                                    return None;
                                }
                                content_may_alias |=
                                    return_value_may_alias_borrow_ref(arg, scope, fresh);
                            }
                            continue;
                        }
                    }
                }
                if stmt_mentions_binding(stmt, id) {
                    return None;
                }
            }
        }
    }
    if forwarded_to.is_some() && !saw_forward {
        return None;
    }
    init_may_alias.map(|base| base || content_may_alias)
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
