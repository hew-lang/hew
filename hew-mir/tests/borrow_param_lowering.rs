//! W5.011-P4 — immutable-borrow (`&T`) as a first-class MIR type fact.
//!
//! These tests pin the lowering and classification of an immutable-borrow
//! parameter (`&T`) down to the raw MIR boundary. They establish the seam that
//! W5.011-P3 consumes when it begins emitting retain-on-copy for by-value heap
//! arguments: a borrow argument is non-owning, so its retain (`VWT.copy`) is
//! skipped. The classifier [`hew_mir::callee_param_is_borrow`] is that gate.
//!
//! Surface note (intentional, not a gap in coverage): v0.5 has no way to
//! *construct* a borrow value from source — there is no borrow-of-local
//! expression (`&x` parses as bitwise-AND) and no `T -> &T` coercion at call
//! sites. So a `&T` parameter can be *declared* (and reaches MIR as
//! [`ResolvedTy::Borrow`], proven below) but cannot yet be *passed* by any
//! source program. The `true` arm of the classifier is therefore exercised by
//! synthetic MIR here rather than by an end-to-end call. When a
//! borrow-introduction form lands, the same classifier upgrades to a
//! non-vacuous zero-`VWT.copy` codegen assertion (see `// P3-FOLLOWUP` markers).

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    call_arg_source_escapes, callee_param_is_borrow, container_ingress_is_copy_in,
    lower_hir_module, FunctionCallConv, IrPipeline, RawMirFunction,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

/// Parse → typecheck → HIR-lower → raw/elaborated MIR. Mirrors the helper in
/// `producer_drop_elaboration.rs`; asserts a clean parse so a test that means
/// to exercise lowering cannot silently pass on a parse error.
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

fn raw_fn<'a>(p: &'a IrPipeline, name: &str) -> &'a RawMirFunction {
    p.raw_mir
        .iter()
        .find(|f| f.name == name)
        .expect("function must be present in raw_mir")
}

/// §4a Test 1 — a declared `&string` parameter survives lowering as a
/// first-class [`ResolvedTy::Borrow`] in the raw-MIR signature, not collapsed
/// to a `Pointer`. This is the load-bearing type fact for P4: without it the
/// retain-skip classifier (and the downstream P3/P5/liveness consumers) have
/// nothing to key on.
#[test]
fn borrow_param_lowers_to_resolved_borrow_in_raw_mir() {
    let p = pipeline_with_tc("fn f(x: &string) -> i64 { return 0; }");
    let f = raw_fn(&p, "f");
    assert_eq!(f.params.len(), 1, "f has exactly one parameter");
    assert!(
        matches!(f.params[0], ResolvedTy::Borrow { .. }),
        "borrow param must lower to ResolvedTy::Borrow, got {:?}",
        f.params[0]
    ); // The borrow's pointee is preserved (here `string`), so a later pass can
       // recover the borrowed type without re-resolving the surface syntax.
    let ResolvedTy::Borrow { pointee } = &f.params[0] else {
        unreachable!("matched Borrow above")
    };
    assert!(
        matches!(pointee.as_ref(), ResolvedTy::String),
        "pointee of `&string` must be ResolvedTy::String, got {pointee:?}"
    );
}

/// §4a Test 2 — the classifier reports the borrow parameter positively from a
/// real lowered pipeline, and stays conservative everywhere it lacks a fact.
/// This is the exact lookup the call-site argument convention performs to
/// decide whether a retain is owed.
#[test]
fn callee_param_is_borrow_classifies_real_pipeline() {
    let p = pipeline_with_tc(
        "fn f(x: &string) -> i64 { return 0; }\n\
         fn g(y: i64) -> i64 { return y; }",
    );

    // Positive: f's arg 0 is a borrow.
    assert!(
        callee_param_is_borrow(&p.raw_mir, "f", 0),
        "f arg0 (&string) must classify as borrow"
    );
    // Negative: g's arg 0 is an owned i64, not a borrow.
    assert!(
        !callee_param_is_borrow(&p.raw_mir, "g", 0),
        "g arg0 (i64) must not classify as borrow"
    );
    // Fail-safe (R5): out-of-range index on a known callee → false, no panic.
    assert!(
        !callee_param_is_borrow(&p.raw_mir, "f", 1),
        "out-of-range arg index must classify as non-borrow"
    );
    // Fail-safe (R5): unknown callee → false, no panic.
    assert!(
        !callee_param_is_borrow(&p.raw_mir, "does_not_exist", 0),
        "unknown callee must classify as non-borrow"
    );
}

/// Classifier-level proof against a hand-built signature, independent of the
/// source frontend. Pins the contract the codegen retain-skip branch relies on:
/// only a [`ResolvedTy::Borrow`] parameter answers `true`; an owned pointer or
/// any other owned type answers `false`. Synthetic because no v0.5 source can
/// place a borrow value at a call-argument position (see module note).
#[test]
fn callee_param_is_borrow_distinguishes_borrow_from_owned_pointer() {
    let borrow_fn = RawMirFunction {
        name: "takes_borrow".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::String),
        }],
        locals: Vec::new(),
        blocks: Vec::new(),
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::HashMap::new(),
    };
    let owned_ptr_fn = RawMirFunction {
        name: "takes_owned_pointer".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![ResolvedTy::Pointer {
            pointee: Box::new(ResolvedTy::String),
            is_mutable: false,
        }],
        locals: Vec::new(),
        blocks: Vec::new(),
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::HashMap::new(),
    };
    let module = vec![borrow_fn, owned_ptr_fn];

    assert!(
        callee_param_is_borrow(&module, "takes_borrow", 0),
        "&T parameter must classify as borrow"
    );
    assert!(
        !callee_param_is_borrow(&module, "takes_owned_pointer", 0),
        "an owned `Pointer (is_mutable: false)` must NOT classify as borrow — \
         the whole point of P4 is that these are distinct facts"
    );
}

/// W5-011 P3, Delta (a) — the borrow-skip decision is *non-vacuous*: chaining
/// the real type-fact classifier (`callee_param_is_borrow`) into the escape
/// classifier (`call_arg_source_escapes`) yields opposite answers for a borrow
/// vs a non-borrow parameter. A non-borrow by-value heap argument *escapes*
/// (its source binding must be excluded from scope-exit drop, lest the callee's
/// shared/returned alias double-free); a borrow argument does *not* escape (the
/// source keeps sole ownership and stays drop-eligible). Synthetic because no
/// v0.5 source can place a borrow at a call-argument position (module note).
#[test]
fn borrow_param_call_is_non_consuming() {
    let borrow_fn = RawMirFunction {
        name: "takes_borrow".to_string(),
        return_ty: ResolvedTy::String,
        call_conv: FunctionCallConv::Default,
        params: vec![ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::String),
        }],
        locals: Vec::new(),
        blocks: Vec::new(),
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::HashMap::new(),
    };
    let by_value_fn = RawMirFunction {
        name: "takes_owned".to_string(),
        return_ty: ResolvedTy::String,
        call_conv: FunctionCallConv::Default,
        params: vec![ResolvedTy::String],
        locals: Vec::new(),
        blocks: Vec::new(),
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::HashMap::new(),
    };
    let module = vec![borrow_fn, by_value_fn];

    // Borrow param: classifier says borrow → does NOT escape → drop-eligible.
    let borrow_is_borrow = callee_param_is_borrow(&module, "takes_borrow", 0);
    assert!(borrow_is_borrow, "&string param must classify as borrow");
    assert!(
        !call_arg_source_escapes(borrow_is_borrow),
        "argument to a borrow parameter must NOT escape — source stays drop-owner"
    );

    // By-value param: classifier says non-borrow → escapes → excluded from drop.
    let owned_is_borrow = callee_param_is_borrow(&module, "takes_owned", 0);
    assert!(!owned_is_borrow, "string param must NOT classify as borrow");
    assert!(
        call_arg_source_escapes(owned_is_borrow),
        "argument to a by-value heap parameter must escape — source excluded from drop"
    );

    // The two answers genuinely differ — the test is not vacuous.
    assert_ne!(
        call_arg_source_escapes(borrow_is_borrow),
        call_arg_source_escapes(owned_is_borrow),
        "borrow vs by-value must drive opposite escape decisions"
    );
}

/// W5-011 P3, Delta (a) fail-safe — an unresolved callee or out-of-range index
/// resolves to non-borrow (`false`), which the escape classifier reads as
/// *escapes* (`true`): the conservative direction excludes the source from
/// drop rather than risk a double-free against an unknown convention.
#[test]
fn callee_param_is_borrow_failsafe() {
    let module: Vec<RawMirFunction> = Vec::new();
    // Unknown callee → non-borrow → escapes (conservative exclude).
    assert!(!callee_param_is_borrow(&module, "ghost", 0));
    assert!(
        call_arg_source_escapes(callee_param_is_borrow(&module, "ghost", 0)),
        "unresolved callee must fail safe to escape (exclude source from drop)"
    );
}

/// W5-011 P3, Delta (b) — container ingress is classified by release contract,
/// not by argument intent. `hew_vec_push` / `hew_vec_set` deep-copy the element
/// (copy-in: source keeps its buffer, stays drop-eligible); every other ingress
/// symbol moves the handle into the container (move-in: source excluded). The
/// escape decision is the negation of copy-in.
#[test]
fn container_ingress_copy_in_vs_move_in() {
    // Copy-in (Delta b.1): vec push/set leave the source owning its buffer.
    assert!(container_ingress_is_copy_in("hew_vec_push"));
    assert!(container_ingress_is_copy_in("hew_vec_set"));

    // Move-in (Delta b.2/b.3): map/set insert take ownership of the handle.
    assert!(!container_ingress_is_copy_in("hew_hashmap_insert_layout"));
    assert!(!container_ingress_is_copy_in("hew_hashset_insert_layout"));

    // Fail-closed: an unknown ingress symbol is treated as move-in (excluded),
    // never as copy-in — conservative direction never double-frees.
    assert!(!container_ingress_is_copy_in("hew_some_unknown_ingress"));
}
