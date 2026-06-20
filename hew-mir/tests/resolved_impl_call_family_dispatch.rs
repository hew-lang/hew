//! Family-keyed dispatch substrate test for `ResolvedImplCall` lowering.
//!
//! These tests are deliberately constructed to fail under
//! string-keyed (`target_symbol.starts_with("hew_hashmap_") / ...`)
//! dispatch and pass under typed-family dispatch — and vice versa,
//! so a regression that quietly re-introduces the string cascade
//! flips the assertion.
//!
//! The earlier draft of this file only asserted that lowering did
//! not panic; it would have passed even if the consumer ignored both
//! `target_symbol` and `target_family`. That is the "substrate
//! tests the substrate" anti-pattern. Each test below pins a
//! behaviour whose value depends concretely on which field the
//! consumer routes on:
//!
//! - Arity-gate tests: `target_symbol` says one family, `target_family`
//!   says another, and `type_args.len()` is consistent with only ONE
//!   of them. String-keyed dispatch routes by symbol prefix and the
//!   length matches → no panic. Typed-family dispatch routes by
//!   `target_family` and the length disagrees → panic whose message
//!   names the FAMILY (`HashMap`/`HashSet`/`Vec`). The assertions
//!   demand both the panic AND the family-specific drift string. All
//!   three arms of `match target_family` are exercised as the routed-to
//!   family (`HashSet`, `HashMap`, and `Vec`), so a regression that
//!   mis-wires a single arm cannot hide behind the others.
//!
//! - Owned-rewrite test: `target_symbol == "hew_vec_push_layout"`
//!   plus a `Vec<owned>` receiver, with `target_family` set to a
//!   *non-push* Vec variant (`VecMethod::Pop`). The owned-rewrite
//!   in `hew-mir/src/lower.rs::ResolvedImplCall` is gated on
//!   `matches!(target_family, Vec(VecMethod::Push))` — string-keyed
//!   dispatch (pre-S2) checked only the symbol and would rewrite
//!   the callee to `hew_vec_push_owned`. Typed-family dispatch
//!   (post-S2) sees the Pop family discriminator and leaves the
//!   callee as `hew_vec_push_layout`. The test inspects the emitted
//!   `Terminator::Call.callee` directly.
//!
//! - Positive MIR-shape anchor: a well-formed call (symbol and family
//!   agree, arity correct) lowers without panicking and emits the
//!   resolved `target_symbol` as the `Terminator::Call.callee`. This
//!   does not discriminate symbol- vs family-keyed dispatch on its own
//!   (the two agree here); it pins the mainline non-rewrite dispatch
//!   outcome so a regression that drops the call emission or mangles
//!   the callee is caught directly rather than only by the absence of
//!   a panic.
//!
//! Each discriminating test above was confirmed to FAIL when the
//! consumer is reverted to symbol-keyed dispatch (`target_family`
//! ignored) — the mutation check that gives these assertions teeth.

use std::collections::HashMap;

use hew_hir::{
    ids::IdGen, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmt,
    HirStmtKind, IntentKind, ScopeId, ValueClass,
};
use hew_mir::lower_hir_module;
use hew_mir::model::{IrPipeline, Terminator};
use hew_types::{
    BuiltinType, HashMapMethod, HashSetMethod, ImplId, MethodTargetFamily, ResolvedTy, TyPattern,
    VecMethod,
};

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        diagnostic_source_modules: HashMap::default(),
        wire_layouts: std::sync::Arc::new(HashMap::default()),
        type_classes: HashMap::default(),
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        vec_generic_element_abi: HashMap::default(),
        record_layouts: vec![],
        enum_layouts: vec![],
        machine_instantiations: vec![],
        supervisor_child_slots: HashMap::default(),
        regex_literals: vec![],
    }
}

fn unit_expr(ids: &mut IdGen, ty: ResolvedTy) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Unit),
        span: 0..0,
    }
}

fn build_one_call_module(call: HirExpr) -> HirModule {
    let mut ids = IdGen::default();
    let stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Expr(call),
        span: 0..0,
    };
    let body = HirBlock {
        node: ids.node(),
        scope: ScopeId(0),
        statements: vec![stmt],
        tail: None,
        ty: ResolvedTy::Unit,
        span: 0..0,
    };
    empty_module(vec![HirItem::Function(HirFn {
        id: ids.item(),
        node: ids.node(),
        name: "main".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: ResolvedTy::Unit,
        body,
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    })])
}

/// Extract the panic message as a `&str`, panicking with diagnostic
/// detail if the payload is not a String / &str.
fn panic_message(payload: &(dyn std::any::Any + Send)) -> &str {
    payload
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| payload.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic payload>")
}

/// Walk every `Terminator::Call` in the lowered `raw_mir` and return
/// the callee strings in encounter order. Used by the owned-rewrite
/// test to inspect what the family-gated callee selection produced.
fn callees_of(pipeline: &IrPipeline) -> Vec<String> {
    let mut out = Vec::new();
    for f in &pipeline.raw_mir {
        for b in &f.blocks {
            if let Terminator::Call { callee, .. } = &b.terminator {
                out.push(callee.clone());
            }
        }
    }
    out
}

// =============================================================================
// Arity-gate tests: target_symbol and target_family disagree, type_args matches
// only ONE of them. Symbol-keyed dispatch would pass; family-keyed must panic
// with the FAMILY-specific drift message.
// =============================================================================

#[test]
fn arity_dispatched_by_family_not_symbol_hashmap_symbol_hashset_family() {
    // target_symbol looks like a HashMap insert (string-keyed dispatch would
    // route to the HashMap arm and accept arity 2). target_family is
    // HashSet — the family-keyed arm requires arity 1, must panic.
    //
    // A regression that reverts to `target_symbol.starts_with("hew_hashmap_")`
    // would silently accept this call and the assertion below — which
    // requires the panic message to mention "HashSet" — would fail.
    let mut ids = IdGen::default();
    let hashmap_str_i64 = ResolvedTy::named_builtin(
        "HashMap",
        BuiltinType::HashMap,
        vec![ResolvedTy::String, ResolvedTy::I64],
    );
    let receiver = unit_expr(&mut ids, hashmap_str_i64);
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            impl_id: ImplId(0),
            method_name: "len".to_string(),
            target_symbol: "hew_hashmap_len_layout".to_string(),
            target_family: MethodTargetFamily::HashSet(HashSetMethod::Len),
            type_args: vec![
                TyPattern::Primitive("string".into()),
                TyPattern::Primitive("i64".into()),
            ],
            args: vec![],
            ret_ty: ResolvedTy::I64,
        },
        span: 0..0,
    };
    let module = build_one_call_module(call);

    let payload = std::panic::catch_unwind(|| {
        let _ = lower_hir_module(&module);
    })
    .expect_err(
        "family-keyed dispatch must panic when target_family says HashSet \
         but type_args.len() == 2 (HashSet arity = 1); a string-keyed \
         consumer would silently accept because target_symbol starts with \
         `hew_hashmap_` and arity 2 matches that family",
    );
    let msg = panic_message(payload.as_ref());
    assert!(
        msg.contains("hashset") && msg.contains("HashSet impls with 1 type-arg"),
        "panic message must name the HashSet family (proving family-keyed \
         dispatch); a regression to string-keyed dispatch would either \
         not panic at all or panic with the HashMap-family message. Got: \
         {msg}"
    );
}

#[test]
fn arity_dispatched_by_family_not_symbol_hashset_symbol_hashmap_family() {
    // Mirror direction: HashSet-looking symbol, HashMap family. type_args.len()
    // == 1 satisfies string-keyed HashSet dispatch but trips the HashMap
    // family arm (HashMap arity = 2).
    let mut ids = IdGen::default();
    let hashset_str =
        ResolvedTy::named_builtin("HashSet", BuiltinType::HashSet, vec![ResolvedTy::String]);
    let receiver = unit_expr(&mut ids, hashset_str);
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            impl_id: ImplId(1),
            method_name: "len".to_string(),
            target_symbol: "hew_hashset_len_layout".to_string(),
            target_family: MethodTargetFamily::HashMap(HashMapMethod::Len),
            type_args: vec![TyPattern::Primitive("string".into())],
            args: vec![],
            ret_ty: ResolvedTy::I64,
        },
        span: 0..0,
    };
    let module = build_one_call_module(call);

    let payload = std::panic::catch_unwind(|| {
        let _ = lower_hir_module(&module);
    })
    .expect_err(
        "family-keyed dispatch must panic when target_family says HashMap \
         but type_args.len() == 1 (HashMap arity = 2); a string-keyed \
         consumer would route by `hew_hashset_` prefix and accept arity 1",
    );
    let msg = panic_message(payload.as_ref());
    assert!(
        msg.contains("hashmap") && msg.contains("HashMap impls with 2 type-args"),
        "panic message must name the HashMap family. Got: {msg}"
    );
}

#[test]
fn arity_dispatched_by_family_not_symbol_vec_symbol_hashmap_family() {
    // Vec-looking symbol, HashMap family. type_args.len() == 1 satisfies
    // string-keyed Vec dispatch but trips the HashMap family arm.
    let mut ids = IdGen::default();
    let vec_i64 = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]);
    let receiver = unit_expr(&mut ids, vec_i64);
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            impl_id: ImplId(2),
            method_name: "len".to_string(),
            target_symbol: "hew_vec_len".to_string(),
            target_family: MethodTargetFamily::HashMap(HashMapMethod::Len),
            type_args: vec![TyPattern::Primitive("i64".into())],
            args: vec![],
            ret_ty: ResolvedTy::I64,
        },
        span: 0..0,
    };
    let module = build_one_call_module(call);

    let payload = std::panic::catch_unwind(|| {
        let _ = lower_hir_module(&module);
    })
    .expect_err(
        "family-keyed dispatch must panic when target_family says HashMap \
         but type_args.len() == 1 (HashMap arity = 2); a string-keyed \
         consumer would route by `hew_vec_` prefix and accept arity 1",
    );
    let msg = panic_message(payload.as_ref());
    assert!(
        msg.contains("hashmap") && msg.contains("HashMap impls with 2 type-args"),
        "panic message must name the HashMap family. Got: {msg}"
    );
}

#[test]
fn arity_dispatched_by_family_not_symbol_hashmap_symbol_vec_family() {
    // Closes the Vec-arm coverage gap: the other three arity tests trip the
    // HashSet and HashMap arms, so a regression that mis-wired *only* the Vec
    // arm of `match target_family` would slip past them. Here target_symbol is
    // a structurally valid HashMap insert (string-keyed dispatch routes to the
    // HashMap arm and accepts arity 2) while target_family is Vec(Push) and
    // type_args.len() == 2 — consistent with HashMap, wrong for Vec (arity 1).
    //
    // Family-keyed dispatch routes by target_family, lands in the Vec arm, sees
    // arity 2 != 1, and panics with the Vec-family drift message. A regression
    // to `target_symbol.starts_with("hew_hashmap_")` would accept arity 2 and
    // not panic at all — the `expect_err` below would then fail.
    let mut ids = IdGen::default();
    let vec_i64 = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]);
    let receiver = unit_expr(&mut ids, vec_i64);
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            impl_id: ImplId(3),
            method_name: "push".to_string(),
            target_symbol: "hew_hashmap_insert_layout".to_string(),
            target_family: MethodTargetFamily::Vec(VecMethod::Push),
            type_args: vec![
                TyPattern::Primitive("string".into()),
                TyPattern::Primitive("i64".into()),
            ],
            args: vec![],
            ret_ty: ResolvedTy::Unit,
        },
        span: 0..0,
    };
    let module = build_one_call_module(call);

    let payload = std::panic::catch_unwind(|| {
        let _ = lower_hir_module(&module);
    })
    .expect_err(
        "family-keyed dispatch must panic when target_family says Vec \
         but type_args.len() == 2 (Vec arity = 1); a string-keyed consumer \
         would route by the `hew_hashmap_` prefix and accept arity 2",
    );
    let msg = panic_message(payload.as_ref());
    assert!(
        msg.contains("vec") && msg.contains("Vec impls with 1 type-arg"),
        "panic message must name the Vec family (proving the Vec arm of \
         `match target_family` is reached by family, not the HashMap symbol \
         prefix). Got: {msg}"
    );
}

// =============================================================================
// Owned-rewrite test: the `hew_vec_push_layout` → `hew_vec_push_owned`
// rewrite at hew-mir/src/lower.rs is now gated on
// `matches!(target_family, Vec(VecMethod::Push))`. String-keyed dispatch
// (pre-S2) would rewrite based purely on `target_symbol`. Family-keyed
// dispatch (post-S2) refuses to rewrite when the family says a non-Push
// Vec method — even though the symbol still says push_layout.
// =============================================================================

fn vec_owned_receiver(ids: &mut IdGen) -> HirExpr {
    // Vec<(string,)> — element is a heap-owning tuple per
    // hew-mir/src/model.rs::ty_contains_heap_owning. This is what makes
    // vec_receiver_has_owned_element return true at the rewrite predicate.
    let elem = ResolvedTy::Tuple(vec![ResolvedTy::String]);
    let vec_ty = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![elem]);
    unit_expr(ids, vec_ty)
}

#[test]
fn owned_push_rewrite_fires_when_target_family_is_vec_push() {
    // Baseline: family IS Vec(Push), symbol IS hew_vec_push_layout, receiver
    // IS Vec<owned>. The owned-rewrite must fire; the emitted callee in
    // Terminator::Call is hew_vec_push_owned. Establishes that the rewrite
    // is reachable so the negative test below is a meaningful counterpoint.
    let mut ids = IdGen::default();
    let receiver = vec_owned_receiver(&mut ids);
    let arg = unit_expr(&mut ids, ResolvedTy::Tuple(vec![ResolvedTy::String]));
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            impl_id: ImplId(2),
            method_name: "push".to_string(),
            target_symbol: "hew_vec_push_layout".to_string(),
            target_family: MethodTargetFamily::Vec(VecMethod::Push),
            type_args: vec![TyPattern::Tuple(vec![TyPattern::Primitive(
                "string".into(),
            )])],
            args: vec![arg],
            ret_ty: ResolvedTy::Unit,
        },
        span: 0..0,
    };
    let module = build_one_call_module(call);
    let pipeline = lower_hir_module(&module);
    let callees = callees_of(&pipeline);
    assert!(
        callees.iter().any(|c| c == "hew_vec_push_owned"),
        "owned-rewrite must fire when target_family = Vec(Push) and \
         receiver is Vec<owned>; emitted callees: {callees:?}"
    );
    assert!(
        !callees.iter().any(|c| c == "hew_vec_push_layout"),
        "the rewritten callee replaces the layout one; if both appear \
         the rewrite is no longer the sole authority. Got: {callees:?}"
    );
}

#[test]
fn owned_push_rewrite_skipped_when_target_family_is_not_vec_push() {
    // Same symbol, same receiver — but target_family is Vec(Pop), not
    // Vec(Push). The S2 owned-rewrite predicate is gated on
    // matches!(target_family, Vec(VecMethod::Push)), so the rewrite must
    // NOT fire and the callee must stay as the literal target_symbol
    // (hew_vec_push_layout).
    //
    // A regression that reverts the gate to a symbol-only check
    // (`target_symbol == "hew_vec_push_layout"` alone) would rewrite
    // to hew_vec_push_owned and the assertion below — that the layout
    // callee survives — would fail.
    let mut ids = IdGen::default();
    let receiver = vec_owned_receiver(&mut ids);
    let arg = unit_expr(&mut ids, ResolvedTy::Tuple(vec![ResolvedTy::String]));
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            impl_id: ImplId(2),
            method_name: "push".to_string(),
            target_symbol: "hew_vec_push_layout".to_string(),
            // Wrong sub-discriminator on purpose. Vec(Pop) satisfies the
            // Vec arity gate (1 type-arg) but not the owned-rewrite gate.
            target_family: MethodTargetFamily::Vec(VecMethod::Pop),
            type_args: vec![TyPattern::Tuple(vec![TyPattern::Primitive(
                "string".into(),
            )])],
            args: vec![arg],
            ret_ty: ResolvedTy::Unit,
        },
        span: 0..0,
    };
    let module = build_one_call_module(call);
    let pipeline = lower_hir_module(&module);
    let callees = callees_of(&pipeline);
    assert!(
        callees.iter().any(|c| c == "hew_vec_push_layout"),
        "callee must stay as target_symbol when target_family is not \
         Vec(Push); the owned-rewrite is family-gated. Got: {callees:?}"
    );
    assert!(
        !callees.iter().any(|c| c == "hew_vec_push_owned"),
        "owned-rewrite must NOT fire when target_family says Pop; a \
         regression to symbol-keyed dispatch would rewrite anyway. \
         Got: {callees:?}"
    );
}

// =============================================================================
// Positive MIR-shape anchor: a well-formed family call (symbol and family
// agree, arity correct) must lower WITHOUT panicking and emit the resolved
// `target_symbol` verbatim as the `Terminator::Call.callee`. This is the
// observable mainline-dispatch outcome the negative arity tests can only
// assert by their absence; pinning it directly guards against a regression
// that drops the call emission or mangles the callee for a non-rewrite
// family (the owned-rewrite arm only covers Vec push). It does not by
// itself discriminate symbol- vs family-keyed dispatch — symbol and family
// agree here — so it complements rather than replaces the discriminating
// tests above.
// =============================================================================

#[test]
fn wellformed_hashmap_dispatch_emits_symbol_as_callee() {
    let mut ids = IdGen::default();
    let hashmap_str_i64 = ResolvedTy::named_builtin(
        "HashMap",
        BuiltinType::HashMap,
        vec![ResolvedTy::String, ResolvedTy::I64],
    );
    let receiver = unit_expr(&mut ids, hashmap_str_i64);
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            impl_id: ImplId(0),
            method_name: "len".to_string(),
            target_symbol: "hew_hashmap_len_layout".to_string(),
            target_family: MethodTargetFamily::HashMap(HashMapMethod::Len),
            type_args: vec![
                TyPattern::Primitive("string".into()),
                TyPattern::Primitive("i64".into()),
            ],
            args: vec![],
            ret_ty: ResolvedTy::I64,
        },
        span: 0..0,
    };
    let module = build_one_call_module(call);
    let pipeline = lower_hir_module(&module);
    let callees = callees_of(&pipeline);
    assert!(
        callees.iter().any(|c| c == "hew_hashmap_len_layout"),
        "well-formed HashMap dispatch must emit the resolved target_symbol \
         as the Terminator::Call callee; emitted callees: {callees:?}"
    );
    assert!(
        !callees.iter().any(|c| c == "hew_vec_push_owned"),
        "the owned-rewrite must not fire for a non-Vec-push family; a \
         spurious rewrite would prove the callee selection is not \
         family-gated. Got: {callees:?}"
    );
}
