//! Structural tests for `Instr::CallRuntimeAbi` — the M2-substrate
//! runtime-ABI seam.
//!
//! These tests are construction-side: they hand-build `Instr`
//! values and verify (1) the symbol allowlist rejects unknown
//! symbols, (2) `instr_places` reports every Place the variant
//! mentions, (3) the variant participates in the standard MIR
//! exhaustive-match discipline. End-to-end source → MIR → codegen
//! coverage waits on the typecheck→HIR/MIR bridge that lights up
//! producers (see `runtime_symbols.rs` WHY/WHEN-OBSOLETE).

use hew_mir::runtime_symbols::{is_known_runtime_symbol, known_runtime_symbols};
use hew_mir::{Instr, Place};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// Allowlist boundary
// ---------------------------------------------------------------------------

#[test]
fn allowlist_accepts_every_listed_symbol() {
    // Round-trip: each symbol the static list publishes must
    // round-trip through the public membership predicate. This is
    // the contract between the producer (which validates before
    // construction) and the allowlist (which must be the source of
    // truth).
    for sym in known_runtime_symbols() {
        assert!(
            is_known_runtime_symbol(sym),
            "publicly listed runtime symbol `{sym}` must be recognised by \
             is_known_runtime_symbol",
        );
    }
}

#[test]
fn allowlist_rejects_off_by_one_typos() {
    // Three real-world typo classes. Each must fail closed.
    // Without the allowlist, `Instr::CallRuntimeAbi { symbol: <typo>, .. }`
    // would survive MIR and only surface as an undefined symbol at
    // codegen link time — a failure mode the allowlist eliminates
    // structurally (boundary-fail-closed, P0 row 49).
    assert!(!is_known_runtime_symbol("hew_duplex_sned")); // transposed
    assert!(!is_known_runtime_symbol("hew_duplex_send_")); // trailing underscore
    assert!(!is_known_runtime_symbol("hew_Duplex_send")); // case
}

#[test]
fn allowlist_rejects_non_hew_prefix() {
    // The substrate ABI is `hew_*`-prefixed; any non-prefixed
    // symbol is a category mismatch, not a substrate call.
    assert!(!is_known_runtime_symbol("malloc"));
    assert!(!is_known_runtime_symbol("memcpy"));
    assert!(!is_known_runtime_symbol("printf"));
}

#[test]
fn allowlist_covers_full_duplex_lifecycle() {
    // The slice 4.5b method-dispatch table publishes these symbols
    // (per hew-types/src/check/methods.rs:1040-1310). When the
    // bridge slice wires producers, every one of these must be
    // emittable; the allowlist is the structural pre-commitment.
    let lifecycle = [
        "hew_duplex_pair",
        "hew_duplex_send",
        "hew_duplex_try_send",
        "hew_duplex_recv",
        "hew_duplex_try_recv",
        "hew_duplex_send_half",
        "hew_duplex_recv_half",
        "hew_duplex_close",
        "hew_duplex_close_half",
        "hew_send_half_send",
        "hew_send_half_try_send",
        "hew_recv_half_recv",
        "hew_recv_half_try_recv",
    ];
    for sym in &lifecycle {
        assert!(
            is_known_runtime_symbol(sym),
            "Duplex lifecycle symbol `{sym}` must be in the allowlist",
        );
    }
}

#[test]
fn allowlist_covers_lambda_actor_lifecycle() {
    // Plan deltas in slice 4 noted that `hew_lambda_actor_new`
    // survives even though the runtime reuses `hew_duplex_pair`
    // for construction — the surface-level distinction is the
    // dedup-semantic-boundary (P2 row 208). The allowlist
    // preserves all eight surface symbols so a future runtime
    // refactor (lambda-actor diverges from Duplex) does not
    // require an allowlist change at the same time.
    let surface = [
        "hew_lambda_actor_new",
        "hew_lambda_actor_send",
        "hew_lambda_actor_release",
        "hew_lambda_actor_clone",
        "hew_lambda_actor_downgrade",
        "hew_lambda_actor_weak_clone",
        "hew_lambda_actor_weak_send",
        "hew_lambda_actor_weak_drop",
    ];
    for sym in &surface {
        assert!(
            is_known_runtime_symbol(sym),
            "Lambda-actor surface symbol `{sym}` must be in the allowlist",
        );
    }
}

// ---------------------------------------------------------------------------
// RuntimeCall constructor — allowlist enforced at construction
// ---------------------------------------------------------------------------

/// `RuntimeCall::new` returns `Ok` for a known symbol.
/// Pins the primary success path of the validated constructor.
#[test]
fn runtime_call_new_ok_for_known_symbol() {
    let result = hew_mir::RuntimeCall::new(
        "hew_duplex_send",
        vec![Place::DuplexHandle(0), Place::Local(1), Place::Local(2)],
        None,
    );
    assert!(
        result.is_ok(),
        "RuntimeCall::new must return Ok for allowlisted symbol 'hew_duplex_send'"
    );
    let call = result.unwrap();
    assert_eq!(call.symbol(), "hew_duplex_send");
    assert_eq!(call.args().len(), 3);
    assert!(call.dest().is_none());
}

/// `RuntimeCall::new` returns `Err(UnknownRuntimeSymbol)` for a symbol
/// not in the allowlist — enforcing the boundary-fail-closed invariant
/// at construction in all build profiles (LESSONS P0 `boundary-fail-closed`).
#[test]
fn runtime_call_new_err_for_unknown_symbol() {
    let result = hew_mir::RuntimeCall::new("not_in_allowlist", vec![], None);
    match result {
        Err(hew_mir::UnknownRuntimeSymbol(sym)) => {
            assert_eq!(
                sym, "not_in_allowlist",
                "UnknownRuntimeSymbol must carry the rejected symbol string"
            );
        }
        Ok(_) => panic!(
            "RuntimeCall::new must return Err for unknown symbol 'not_in_allowlist'; \
             allowlist boundary-fail-closed regression"
        ),
    }
}

// ---------------------------------------------------------------------------
// Variant construction + `instr_places` exhaustiveness
// ---------------------------------------------------------------------------

/// Reach `instr_places` indirectly through one of the public
/// codepaths that calls it. The fastest probe is to construct a
/// minimal `RawMirFunction`-like surface; but `instr_places`
/// lives behind a `pub(crate)` boundary in `lower.rs`, so we
/// exercise the variant's structural shape directly here. The
/// real `instr_places` coverage comes from the cross-block
/// proptests in `lower::slice35_cross_block_proptests`, which
/// would panic on a non-exhaustive match the moment a producer
/// emits the new variant.

#[test]
fn call_runtime_abi_carries_symbol_and_places() {
    // Pin the structural shape: the variant carries a symbol
    // string, an args vector, and an optional dest. Future
    // refactors that reshape these fields trip this test.
    let instr = Instr::CallRuntimeAbi(
        hew_mir::RuntimeCall::new(
            "hew_duplex_send",
            vec![Place::DuplexHandle(0), Place::Local(1), Place::Local(2)],
            Some(Place::Local(3)),
        )
        .expect("hew_duplex_send is allowlisted"),
    );
    let Instr::CallRuntimeAbi(ref call) = instr else {
        panic!("constructed Instr::CallRuntimeAbi but matched as something else");
    };
    assert_eq!(call.symbol(), "hew_duplex_send");
    assert_eq!(call.args().len(), 3);
    assert_eq!(call.args()[0], Place::DuplexHandle(0));
    assert_eq!(call.dest(), Some(Place::Local(3)));
}

#[test]
fn call_runtime_abi_supports_unit_return_via_none_dest() {
    // The discarded-result case: `.close()` consumes the handle
    // but the user binds nothing. Codegen emits the call without
    // a return-slot write. The shape must accept `dest = None`.
    let instr = Instr::CallRuntimeAbi(
        hew_mir::RuntimeCall::new("hew_duplex_close", vec![Place::DuplexHandle(7)], None)
            .expect("hew_duplex_close is allowlisted"),
    );
    if let Instr::CallRuntimeAbi(ref call) = instr {
        assert!(
            call.dest().is_none(),
            "discarded-result calls carry dest=None"
        );
    } else {
        panic!("variant match failed");
    }
}

#[test]
fn call_runtime_abi_zero_args_is_structurally_allowed() {
    // A future allocator-style call (e.g. a stdlib counter) might
    // emit a zero-arg runtime call. The variant must not bake in a
    // minimum-arity assumption.
    let instr = Instr::CallRuntimeAbi(
        hew_mir::RuntimeCall::new("hew_duplex_pair", vec![], Some(Place::Local(0)))
            .expect("hew_duplex_pair is allowlisted"),
    );
    if let Instr::CallRuntimeAbi(ref call) = instr {
        assert_eq!(call.args().len(), 0);
    } else {
        panic!("variant match failed");
    }
}

// ---------------------------------------------------------------------------
// Type-aware probes — Place-kind invariants
// ---------------------------------------------------------------------------

/// The half-handle close path passes a `SendHalf` or `RecvHalf` as
/// `args[0]`. Pin that the variant accepts the half-handle Place
/// shapes — codegen will need to load them through a different
/// LLVM intrinsic than `DuplexHandle` (different runtime ABI
/// pointer layout), so the structural information must survive.
#[test]
fn call_runtime_abi_accepts_half_handle_places() {
    let send_close = Instr::CallRuntimeAbi(
        hew_mir::RuntimeCall::new("hew_duplex_close_half", vec![Place::SendHalf(5)], None)
            .expect("hew_duplex_close_half is allowlisted"),
    );
    let recv_close = Instr::CallRuntimeAbi(
        hew_mir::RuntimeCall::new("hew_duplex_close_half", vec![Place::RecvHalf(5)], None)
            .expect("hew_duplex_close_half is allowlisted"),
    );
    // Both must construct cleanly. The structural assertion is
    // that the variant accepts any Place kind in args — the
    // semantic check (only half-handle Places call close_half,
    // only DuplexHandle Places call close) lives in the producer.
    if let Instr::CallRuntimeAbi(ref call) = send_close {
        assert!(matches!(call.args()[0], Place::SendHalf(_)));
    } else {
        unreachable!();
    }
    if let Instr::CallRuntimeAbi(ref call) = recv_close {
        assert!(matches!(call.args()[0], Place::RecvHalf(_)));
    } else {
        unreachable!();
    }
}

#[test]
fn call_runtime_abi_accepts_lambda_actor_handle_place() {
    // Symmetric for lambda-actor lifecycle calls.
    let release = Instr::CallRuntimeAbi(
        hew_mir::RuntimeCall::new(
            "hew_lambda_actor_release",
            vec![Place::LambdaActorHandle(2)],
            None,
        )
        .expect("hew_lambda_actor_release is allowlisted"),
    );
    if let Instr::CallRuntimeAbi(ref call) = release {
        assert!(matches!(call.args()[0], Place::LambdaActorHandle(_)));
    } else {
        unreachable!();
    }
}

// ---------------------------------------------------------------------------
// Pipeline integration — the variant survives an end-to-end IrPipeline
// ---------------------------------------------------------------------------

/// The MIR `IrPipeline` is the cross-pass carrier. Even though no
/// producer emits `Instr::CallRuntimeAbi` today, a hand-built
/// `BasicBlock` that contains it must round-trip through the
/// pipeline data structures without triggering an exhaustive-match
/// panic in `instr_places` or sibling code paths.
#[test]
fn raw_mir_basic_block_round_trips_call_runtime_abi() {
    use hew_mir::{BasicBlock, RawMirFunction, Terminator};

    // Hand-built RawMirFunction: one block, terminator Return,
    // a single Instr::CallRuntimeAbi in the instruction stream.
    let func = RawMirFunction {
        name: "probe".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64],
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::CallRuntimeAbi(
                hew_mir::RuntimeCall::new(
                    "hew_duplex_send",
                    vec![Place::DuplexHandle(0), Place::Local(0)],
                    None,
                )
                .expect("hew_duplex_send is allowlisted"),
            )],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::HashMap::new(),
    };

    // The Clone + PartialEq derives let us assert the variant
    // preserves identity through the standard MIR carriers. A
    // future refactor that drops Clone on Instr would surface
    // here.
    let copy = func.clone();
    assert_eq!(
        func, copy,
        "RawMirFunction with CallRuntimeAbi must clone-eq"
    );
    assert_eq!(copy.blocks[0].instructions.len(), 1);
    if let Instr::CallRuntimeAbi(ref call) = copy.blocks[0].instructions[0] {
        assert_eq!(call.symbol(), "hew_duplex_send");
    } else {
        panic!("CallRuntimeAbi did not round-trip through RawMirFunction");
    }
}
