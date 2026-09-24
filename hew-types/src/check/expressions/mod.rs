//! Checker registration/method/expression logic, split into submodules.
#![allow(
    unused_imports,
    redundant_imports,
    reason = "header retained verbatim from the pre-split file"
)]
use super::branch_join::BranchArmExit;
use super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

mod borrow_diagnostics;
mod generics_eq;
mod places_moves;
mod stack_hints;
mod synthesize;
mod synthesize_control;
mod synthesize_spawn_forms;
mod variants_forms;

/// The joined element type of a `Vec<Task<T>>` operand, or `None` for anything
/// else. `await` over a vector of task handles is the only vector form it joins.
fn vec_task_output(ty: &Ty) -> Option<Ty> {
    let Ty::Named {
        builtin: Some(BuiltinType::Vec),
        args,
        ..
    } = ty
    else {
        return None;
    };
    match args.first() {
        Some(Ty::Task(output)) => Some((**output).clone()),
        _ => None,
    }
}

type DangerousRcBinding = String;
type DangerousRcScope = HashMap<String, Option<DangerousRcBinding>>;

/// Build the `E_IS_VALUE_TYPE` diagnostic for a value-type operand of `is`.
///
/// One authority for the wording, shared by the in-place rejection and the
/// deferred re-check of an operand whose type only settled at a call site.
fn is_value_type_diagnostic(span: &Span, ty: &Ty) -> (TypeErrorKind, Span, String) {
    (
        TypeErrorKind::InvalidOperation,
        span.clone(),
        format!(
            "`is` compares identity, and `{}` is a value type \
             (E_IS_VALUE_TYPE) — use `==` to compare it by value; `is` \
             applies to handles: actor references",
            ty.user_facing()
        ),
    )
}
