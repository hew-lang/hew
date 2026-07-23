//! #2648 — call-scrutinee return-provenance PREFLIGHT wiring (S2).
//!
//! These tests exercise the full pipeline (parse → typecheck → HIR lower → MIR
//! lower) and assert the preflight admission classifier's behaviour at the
//! call-scrutinee consumers:
//!
//! - a forwarder whose summary carries `PARAM` (`fn passthru(x) { x }`) used as a
//!   match / while-let / let-else / if-let / discarded scrutinee REJECTS with
//!   exactly one `NotYetImplemented` diagnostic and NO partial scrutinee MIR
//!   (no `__hew_call_scrutinee` owner mint and no neutralization of a call
//!   result; independent call-carrier transfers may neutralize argument slots);
//! - an `OPAQUE`-only module fn (a Hew fn forwarding a heap-returning extern
//!   result, the jwt/encrypt shape) takes the interim `LegacyModuleCall` path —
//!   it COMPILES and STILL mints the `__hew_call_scrutinee` owner byte-for-byte as
//!   today (the legacy-output regression);
//! - a fresh producer (`match make_fresh()`) admits and mints the owner;
//! - a direct heap-returning extern scrutinee, and a user extern whose NAME spoofs
//!   the `hew_channel_recv_layout` runtime symbol, both REJECT (keyed on the
//!   `ItemId`, never the display name — the Rev-6 NEW-P0 typed-identity carve-out).
//!
//! LESSONS applied: `boundary-fail-closed` (the reject is a real diagnostic with
//! no partial codegen), exact-value assertions (exactly one diagnostic; owner
//! present/absent asserted, never `len() > 0`).

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, MirDiagnosticKind};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

const SYNTHETIC_CALL_SCRUTINEE_NAME: &str = "__hew_call_scrutinee";

fn pipeline(source: &str) -> IrPipeline {
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

/// Count the #2648 preflight reject diagnostics (a `NotYetImplemented` whose
/// construct names the call-scrutinee reject).
fn reject_count(p: &IrPipeline) -> usize {
    p.diagnostics
        .iter()
        .filter(|d| {
            matches!(
                &d.kind,
                MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct.contains("call-scrutinee")
            )
        })
        .count()
}

/// Any other MIR diagnostic (a signal the program failed to lower for an
/// unrelated reason — the test source must be otherwise clean).
fn unrelated_diag_count(p: &IrPipeline) -> usize {
    p.diagnostics.len() - reject_count(p)
}

/// True when ANY lowered function mints the `__hew_call_scrutinee` owner.
fn any_owner_minted(p: &IrPipeline) -> bool {
    p.raw_mir.iter().any(|f| {
        f.blocks.iter().any(|b| {
            b.statements.iter().any(|s| {
                matches!(
                    s,
                    hew_mir::MirStatement::Bind { name, .. }
                        if name == SYNTHETIC_CALL_SCRUTINEE_NAME
                )
            })
        })
    })
}

/// Count `__hew_call_scrutinee` owner mints across every lowered function.
fn count_owner_mints(p: &IrPipeline) -> usize {
    p.raw_mir
        .iter()
        .flat_map(|f| f.blocks.iter())
        .flat_map(|b| b.statements.iter())
        .filter(|s| {
            matches!(
                s,
                hew_mir::MirStatement::Bind { name, .. }
                    if name == SYNTHETIC_CALL_SCRUTINEE_NAME
            )
        })
        .count()
}

/// True when any lowered function neutralizes a direct call's result place.
///
/// Other ownership authorities may legitimately neutralize call arguments or
/// callee parameters. The #2648 invariant is narrower: a rejected scrutinee
/// must never partially consume the result produced by its rejected call.
fn any_call_result_neutralize(p: &IrPipeline) -> bool {
    p.raw_mir.iter().any(|f| {
        let call_results: Vec<_> = f
            .blocks
            .iter()
            .filter_map(|b| match &b.terminator {
                hew_mir::Terminator::Call {
                    dest: Some(dest), ..
                } => Some(*dest),
                _ => None,
            })
            .collect();
        f.blocks.iter().any(|b| {
            b.instructions.iter().any(|i| {
                matches!(
                    i,
                    hew_mir::Instr::NeutralizePayloadSlot { place, .. }
                        if call_results.contains(place)
                )
            })
        })
    })
}

// ---------------------------------------------------------------------------
// PARAM forwarder → REJECT at every consumer, no partial MIR [F4]
// ---------------------------------------------------------------------------

const FORWARDER: &str = r"
    fn passthru(x: Result<string, string>) -> Result<string, string> { x }
";

#[test]
fn forwarder_borrow_only_match_rejects_with_no_owner_or_neutralize() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) -> i64 {{\n\
            match passthru(r) {{ Ok(_) => 1, Err(_) => 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        1,
        "exactly one #2648 reject; diags: {:#?}",
        p.diagnostics
    );
    assert_eq!(
        unrelated_diag_count(&p),
        0,
        "no unrelated diagnostics: {:#?}",
        p.diagnostics
    );
    assert!(
        !any_owner_minted(&p),
        "a rejected forwarder scrutinee must mint NO __hew_call_scrutinee owner"
    );
    assert!(
        !any_call_result_neutralize(&p),
        "a rejected forwarder scrutinee must not neutralize its call result"
    );
}

#[test]
fn forwarder_while_let_rejects() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) {{\n\
            while let Ok(_v) = passthru(r) {{ break; }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(reject_count(&p), 1, "diags: {:#?}", p.diagnostics);
    assert!(!any_owner_minted(&p));
}

#[test]
fn forwarder_let_else_rejects() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) -> i64 {{\n\
            let Ok(_v) = passthru(r) else {{ return 0 }};\n\
            1\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(reject_count(&p), 1, "diags: {:#?}", p.diagnostics);
    assert!(!any_owner_minted(&p));
}

#[test]
fn forwarder_if_let_rejects() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) -> i64 {{\n\
            if let Ok(_v) = passthru(r) {{ 1 }} else {{ 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(reject_count(&p), 1, "diags: {:#?}", p.diagnostics);
    assert!(!any_owner_minted(&p));
}

#[test]
fn forwarder_discarded_statement_rejects() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) {{\n\
            passthru(r);\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(reject_count(&p), 1, "diags: {:#?}", p.diagnostics);
    assert!(!any_owner_minted(&p));
}

#[test]
fn forwarder_over_fresh_ctor_arg_admits_with_arg_scan() {
    // `match forwarder(fresh_ctor())` — the callee summary is `{PARAM}`-only and
    // the argument is inline-fresh, so the S2b caller arg-scan ADMITS: the
    // forwarded return can only alias the fresh ctor result, a fresh sole owner.
    // (This flips the interim over-reject pin — the explicit, reviewed
    // behaviour change the old test's comment promised.)
    let src = format!(
        "{FORWARDER}\n\
         fn fresh_ctor() -> Result<string, string> {{ Ok(\"x\") }}\n\
         fn use_it() -> i64 {{\n\
            match passthru(fresh_ctor()) {{ Ok(_) => 1, Err(_) => 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        0,
        "a ParamsOnly forwarder over an inline-fresh arg admits (arg-scan rescue): {:#?}",
        p.diagnostics
    );
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
    assert_eq!(
        count_owner_mints(&p),
        1,
        "the admitted scrutinee mints EXACTLY ONE owner"
    );
}

// ---------------------------------------------------------------------------
// S2b — the ParamsOnly caller arg-scan (template/semver-shaped admits + the
// fail-closed arg rejects)
// ---------------------------------------------------------------------------

/// A `ParamsOnly` stdlib-parser shape: the result embeds the string parameter
/// (`Ok(Template { src: src })` — summary `{PARAM}`).
const PARSER: &str = r"
    fn wrap(s: string) -> Result<string, string> { Ok(s) }
";

#[test]
fn params_only_inline_literal_arg_admits_and_mints_owner() {
    // `match template.try_parse("hello {{.name")` — the template fixture shape.
    let src = format!(
        "{PARSER}\n\
         fn use_it() -> i64 {{\n\
            match wrap(\"hello\") {{ Ok(_) => 1, Err(_) => 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        0,
        "a ParamsOnly callee over an inline literal admits: {:#?}",
        p.diagnostics
    );
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
    assert_eq!(count_owner_mints(&p), 1, "exactly one owner over the admit");
}

#[test]
fn params_only_let_bound_fresh_local_arg_admits() {
    // `let v = "x"; match parse(v)` — the semver_test shape: a plain `let`
    // local whose S1 bits are `∅`, unaliased, read exactly once.
    let src = format!(
        "{PARSER}\n\
         fn use_it() -> i64 {{\n\
            let v = \"1.2.3\";\n\
            match wrap(v) {{ Ok(_) => 1, Err(_) => 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        0,
        "a let-bound provably-fresh local arg admits: {:#?}",
        p.diagnostics
    );
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
    assert_eq!(count_owner_mints(&p), 1, "exactly one owner over the admit");
}

#[test]
fn params_only_mixed_fresh_and_borrowed_args_reject() {
    // One fresh literal + one borrowed place: the place could be the forwarded
    // buffer, so the scan fails closed.
    let src = r#"
        type Holder { b: string; }
        fn wrap2(a: string, b: string) -> Result<string, string> { Ok(a) }
        fn use_it(h: Holder) -> i64 {
            match wrap2("lit", h.b) { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        1,
        "mixed fresh + borrowed-place args must reject: {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

#[test]
fn params_only_param_of_caller_arg_rejects() {
    // The caller's own by-value heap param is a borrow ITS caller still owns —
    // never provably fresh.
    let src = format!(
        "{PARSER}\n\
         fn use_it(s: string) -> i64 {{\n\
            match wrap(s) {{ Ok(_) => 1, Err(_) => 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        1,
        "a param-of-caller arg must reject: {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

#[test]
fn params_only_unknown_callee_arg_rejects() {
    // An argument produced by an un-audited heap extern is not provably fresh.
    let src = format!(
        "{PARSER}\n\
         extern \"C\" {{\n\
            fn ext_make() -> string;\n\
         }}\n\
         fn use_it() -> i64 {{\n\
            match wrap(ext_make()) {{ Ok(_) => 1, Err(_) => 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        1,
        "an unknown/extern-produced arg must reject: {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

#[test]
fn params_only_aliased_local_arg_rejects() {
    // `let w = v` — both alias one value; a second in-scope release authority
    // exists, so neither is admissible as a fresh argument.
    let src = format!(
        "{PARSER}\n\
         fn use_it() -> i64 {{\n\
            let v = \"x\";\n\
            let w = v;\n\
            match wrap(w) {{ Ok(_) => 1, Err(_) => 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        1,
        "an aliased local arg must reject: {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

#[test]
fn params_only_reread_local_arg_rejects() {
    // The local is read again after the scrutinee — the minted owner would
    // free the buffer the later read still derives from.
    let src = format!(
        "{PARSER}\n\
         fn take(s: string) -> i64 {{ 1 }}\n\
         fn use_it() -> i64 {{\n\
            let v = \"x\";\n\
            let n = match wrap(v) {{ Ok(_) => 1, Err(_) => 0 }};\n\
            n + take(v)\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        1,
        "a re-read local arg must reject: {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

#[test]
fn params_only_pattern_binder_arg_rejects() {
    // A match-payload binder aliases a payload slot another owner may release —
    // never treated as an independently-owned fresh value.
    let src = format!(
        "{PARSER}\n\
         fn make() -> Result<string, string> {{ Ok(\"x\") }}\n\
         fn use_it() -> i64 {{\n\
            match make() {{\n\
                Ok(inner) => match wrap(inner) {{ Ok(_) => 1, Err(_) => 0 }},\n\
                Err(_) => 0,\n\
            }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        1,
        "a pattern-binder arg must reject: {:#?}",
        p.diagnostics
    );
}

#[test]
fn extern_result_bound_module_fn_takes_legacy_path() {
    // The jwt/encrypt shape END-TO-END: a module fn binding an extern result
    // and returning it through a catch-all error arm is `OPAQUE`-only (the
    // extern-name id-collision and the catch-all `err =>` binder both stay out
    // of the PARAM channel) → interim LegacyModuleCall: compiles and mints the
    // owner as today.
    let src = r#"
        extern "C" {
            fn ext_encode(payload: string) -> string;
        }
        fn last_err() -> Result<string, string> { Err("e") }
        fn try_encode(payload: string) -> Result<string, string> {
            let token = ext_encode(payload);
            match last_err() {
                Ok(_) => Ok(token),
                err => err,
            }
        }
        fn use_it() -> i64 {
            match try_encode("{}") { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        0,
        "an OPAQUE-only extern-result module fn must take the legacy path: {:#?}",
        p.diagnostics
    );
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
    assert!(any_owner_minted(&p), "legacy mint preserved");
}

#[test]
fn forwarder_reused_in_loop_rejects() {
    // The #2648 loop-back-edge repro: a PARAM forwarder scrutinee inside a loop
    // body would mint one owner per back-edge over the same forwarded buffer.
    // The preflight rejects it before lowering — one diagnostic, no owner mint.
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) {{\n\
            var i = 0;\n\
            while i < 2 {{\n\
                match passthru(r) {{ Ok(_) => {{}}, Err(_) => {{}} }}\n\
                i = i + 1;\n\
            }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        1,
        "a forwarder scrutinee reused across a loop back-edge rejects: {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

// ---------------------------------------------------------------------------
// Fresh producer + interim LegacyModuleCall → ADMIT, owner minted as today
// ---------------------------------------------------------------------------

#[test]
fn fresh_producer_match_admits_and_mints_owner() {
    let src = r#"
        fn make_fresh() -> Result<string, string> { Ok("x") }
        fn use_it() -> i64 {
            match make_fresh() { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_eq!(reject_count(&p), 0, "a fresh producer must not reject");
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
    assert_eq!(
        count_owner_mints(&p),
        1,
        "a single fresh-producer scrutinee mints EXACTLY ONE __hew_call_scrutinee \
         owner — never two over the same buffer (the #2648 double-mint invariant)"
    );
}

#[test]
fn opaque_only_module_fn_takes_legacy_path_and_mints_owner() {
    // A Hew fn forwarding a heap-returning extern's result is `OPAQUE`-only (no
    // PARAM) — the jwt/encrypt `try_encode` shape. Interim: it takes the
    // `LegacyModuleCall` path → COMPILES and STILL mints the owner exactly as
    // today (the legacy-output regression). Its precise-Fresh admit lands at S4b.
    let src = r#"
        extern "C" {
            fn ext_make() -> Result<string, string>;
        }
        fn wrap() -> Result<string, string> { ext_make() }
        fn use_it() -> i64 {
            match wrap() { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        0,
        "an OPAQUE-only module fn must take the legacy path, not reject: {:#?}",
        p.diagnostics
    );
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
    assert_eq!(
        count_owner_mints(&p),
        1,
        "the interim LegacyModuleCall path preserves the owner mint byte-for-byte \
         (exactly one owner, as today)"
    );
}

// ---------------------------------------------------------------------------
// Direct heap extern + spoofed runtime-symbol extern → REJECT (keyed on ItemId)
// ---------------------------------------------------------------------------

#[test]
fn direct_heap_extern_scrutinee_rejects() {
    let src = r#"
        extern "C" {
            fn ext_make() -> Result<string, string>;
        }
        fn use_it() -> i64 {
            match ext_make() { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        1,
        "an un-audited heap extern scrutinee must reject: {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

#[test]
fn spoofed_recv_symbol_extern_scrutinee_rejects() {
    // A user extern whose NAME spoofs the compiler runtime recv symbol resolves to
    // `ResolvedRef::Item` (not `Builtin`), so it is keyed by `ItemId` and REJECTS
    // as a heap extern — it does NOT hit the name-only recv carve-out.
    let src = r#"
        extern "C" {
            fn hew_channel_recv_layout(ch: i64) -> Result<string, string>;
        }
        fn use_it() -> i64 {
            match hew_channel_recv_layout(0) { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        1,
        "a spoofed recv-symbol extern must reject (typed identity, not name): {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

// ---------------------------------------------------------------------------
// S3 — the #2523 projected-payload twin gate (classify_scrutinee_origin)
//
// The twin classifier no longer admits every call/method/aggregate arm
// unconditionally: it consults the same return-provenance authority. These
// tests exercise the observable move-out behaviour through the in-memory MIR
// (p.raw_mir / p.diagnostics), NOT --dump-mir.
// ---------------------------------------------------------------------------

/// A projected-payload move-out diagnostic (#2523's fail-closed reject at
/// consume time), distinct from the preflight `NotYetImplemented` reject.
fn payload_move_reject_count(p: &IrPipeline) -> usize {
    p.diagnostics
        .iter()
        .filter(|d| {
            matches!(
                &d.kind,
                MirDiagnosticKind::ProjectedPayloadMoveFromReadablePlace { .. }
            )
        })
        .count()
}

#[test]
fn twin_call_forwarder_move_out_rejects_with_no_neutralize() {
    // The #2523 twin repro: a PARAM forwarder scrutinee whose Ok payload is
    // MOVED OUT. The preflight rejects it before lowering, so no owner is
    // minted and its call-result place is never neutralized (the twin
    // double-free is closed).
    let src = format!(
        "{FORWARDER}\n\
         fn sink(s: string) -> i64 {{ 1 }}\n\
         fn use_it(r: Result<string, string>) -> i64 {{\n\
            match passthru(r) {{ Ok(inner) => sink(inner), Err(_) => 0 }}\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(
        reject_count(&p),
        1,
        "the twin forwarder move-out must reject: {:#?}",
        p.diagnostics
    );
    assert!(
        !any_call_result_neutralize(&p),
        "a rejected twin scrutinee must not neutralize its call result"
    );
    assert!(!any_owner_minted(&p), "and NO owner mint");
}

#[test]
fn owned_record_getter_move_out_admits_not_rejected() {
    // A `Vec<Rec>` `.get` lowers to the fresh-owner clone choke
    // (`hew_vec_get_clone`), so the F1 emitted-symbol contract classifies it
    // Fresh → EphemeralTemp: the Some-payload move-out ADMITS. No preflight
    // reject (a getter is a `ResolvedImplCall`, not a `Call`) and, crucially, no
    // projected-payload reject (the twin gate does not false-reject the clone
    // getter that keeps `owned_nested_tuple_record` green).
    let src = r"
        type Rec { s: string; }
        fn take(r: Rec) -> i64 { 1 }
        fn use_it(ys: Vec<Rec>) -> i64 {
            match ys.get(0) { Some(v) => take(v), None => 0 }
        }
    ";
    let p = pipeline(src);
    assert_eq!(reject_count(&p), 0, "getter must not preflight-reject");
    assert_eq!(
        payload_move_reject_count(&p),
        0,
        "an owned clone getter must not projected-payload-reject: {:#?}",
        p.diagnostics
    );
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
}

#[test]
fn opaque_only_module_fn_move_out_admits_and_mints_owner() {
    // An `OPAQUE`-only module fn (forwarding a heap extern result) is admitted
    // via the interim LegacyModuleCall path; a move-out of its payload keeps the
    // legacy classification (mints the owner as today, not rejected).
    let src = r#"
        extern "C" {
            fn ext_make() -> Result<string, string>;
        }
        fn wrap() -> Result<string, string> { ext_make() }
        fn sink(s: string) -> i64 { 1 }
        fn use_it() -> i64 {
            match wrap() { Ok(inner) => sink(inner), Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        0,
        "an OPAQUE-only module fn move-out must not reject: {:#?}",
        p.diagnostics
    );
    assert_eq!(
        payload_move_reject_count(&p),
        0,
        "diags: {:#?}",
        p.diagnostics
    );
    assert!(
        any_owner_minted(&p),
        "the interim LegacyModuleCall path mints the owner byte-for-byte as today"
    );
}

#[test]
fn method_call_forwarder_move_out_rejects_with_no_owner_or_neutralize() {
    // A user METHOD that forwards a by-value heap parameter
    // (`fn forward(self, x) -> T { x }`) used as a match scrutinee whose payload
    // is moved out. The method-call scrutinee resolves through the same
    // return-provenance authority (summary `{PARAM}`), so the preflight rejects
    // it before lowering — exactly one diagnostic, no owner mint, and no
    // neutralization of the rejected call result.
    let src = r"
        type Holder { tag: i64; }
        impl Holder {
            fn forward(self, x: Result<string, string>) -> Result<string, string> { x }
        }
        fn sink(s: string) -> i64 { 1 }
        fn use_it(h: Holder, r: Result<string, string>) -> i64 {
            match h.forward(r) { Ok(inner) => sink(inner), Err(_) => 0 }
        }
    ";
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        1,
        "a method-call forwarder scrutinee must reject: {:#?}",
        p.diagnostics
    );
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
    assert!(
        !any_owner_minted(&p),
        "a rejected method-call forwarder must mint NO owner"
    );
    assert!(
        !any_call_result_neutralize(&p),
        "a rejected method-call forwarder must not neutralize its call result"
    );
}

// ---------------------------------------------------------------------------
// S2c — child-builder provenance threading: closure / generator bodies are
// USER code and must see the same #2648 verdicts as top-level bodies (the
// cross-review P0: a child builder falling back to `Builder::default()` sent
// `match wrap(s)` inside a closure through the unknown-item legacy fail-open
// mint — a reproduced double-free under the poisoned allocator).
// ---------------------------------------------------------------------------

#[test]
fn closure_match_forwarder_over_capture_rejects() {
    // The cross-review repro: a ParamsOnly forwarder matched over a captured
    // heap value inside a closure invoked twice. Must reject with exactly one
    // diagnostic and NO owner mint in any lowered function (including the
    // closure shim) — never compile into a double-freeing binary.
    let src = r"
        fn wrap(s: Vec<i64>) -> Result<Vec<i64>, Vec<i64>> { Ok(s) }
        fn runner(s: Vec<i64>) {
            let f = || {
                match wrap(s) {
                    Ok(_) => match Ok(1) { Ok(_) => {}, Err(_) => {} },
                    Err(_) => {},
                }
            };
            f();
            f();
        }
    ";
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        1,
        "a forwarder over a captured heap value inside a closure must reject: {:#?}",
        p.diagnostics
    );
    assert!(
        !any_owner_minted(&p),
        "no __hew_call_scrutinee owner may be minted in ANY lowered function \
         (including the closure shim)"
    );
    assert!(
        !any_call_result_neutralize(&p),
        "and no neutralized call result"
    );
}

#[test]
fn closure_match_params_only_literal_arg_admits() {
    // The provenance thread must not blanket-reject closures: a ParamsOnly
    // callee over an inline literal inside a closure is still a fresh sole
    // owner — admitted, one owner minted in the shim.
    let src = r#"
        fn wrap(s: string) -> Result<string, string> { Ok(s) }
        fn use_it() -> i64 {
            let f = || {
                match wrap("lit") { Ok(_) => 1, Err(_) => 0 }
            };
            f()
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        0,
        "an inline-literal ParamsOnly scrutinee inside a closure admits: {:#?}",
        p.diagnostics
    );
    assert_eq!(unrelated_diag_count(&p), 0, "diags: {:#?}", p.diagnostics);
    assert_eq!(
        count_owner_mints(&p),
        1,
        "exactly one owner, minted in the closure shim"
    );
}

#[test]
fn closure_local_arg_is_not_admitted_fail_closed() {
    // A closure-body local is NOT in any freshness map (child builders keep
    // the empty fail-closed facts — a child body can run any number of times
    // per parent execution), so a local argument inside a closure rejects
    // even though the same shape admits at top level.
    let src = r#"
        fn wrap(s: string) -> Result<string, string> { Ok(s) }
        fn use_it() -> i64 {
            let f = || {
                let v = "x";
                match wrap(v) { Ok(_) => 1, Err(_) => 0 }
            };
            f()
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        1,
        "a closure-body local arg fails closed: {:#?}",
        p.diagnostics
    );
    assert!(!any_owner_minted(&p));
}

#[test]
fn closure_while_let_forwarder_rejects() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) {{\n\
            let f = || {{\n\
                while let Ok(_v) = passthru(r) {{ break; }}\n\
            }};\n\
            f();\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(reject_count(&p), 1, "diags: {:#?}", p.diagnostics);
    assert!(!any_owner_minted(&p));
}

#[test]
fn closure_let_else_forwarder_rejects() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) -> i64 {{\n\
            let f = || {{\n\
                let Ok(_v) = passthru(r) else {{ return 0 }};\n\
                1\n\
            }};\n\
            f()\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(reject_count(&p), 1, "diags: {:#?}", p.diagnostics);
    assert!(!any_owner_minted(&p));
}

#[test]
fn closure_if_let_forwarder_rejects() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) -> i64 {{\n\
            let f = || {{\n\
                if let Ok(_v) = passthru(r) {{ 1 }} else {{ 0 }}\n\
            }};\n\
            f()\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(reject_count(&p), 1, "diags: {:#?}", p.diagnostics);
    assert!(!any_owner_minted(&p));
}

#[test]
fn closure_discarded_forwarder_rejects() {
    let src = format!(
        "{FORWARDER}\n\
         fn use_it(r: Result<string, string>) {{\n\
            let f = || {{\n\
                passthru(r);\n\
            }};\n\
            f();\n\
         }}\n"
    );
    let p = pipeline(&src);
    assert_eq!(reject_count(&p), 1, "diags: {:#?}", p.diagnostics);
    assert!(!any_owner_minted(&p));
}

#[test]
fn generator_body_matches_mint_no_owner() {
    // A generator body is lowered by a child builder that carries NO
    // `enum_layouts`, so `ty_is_heap_owning_enum_composite` is false for every
    // scrutinee there: the from-call owner is NEVER minted in a gen body (the
    // pre-existing leak-biased posture — the Result temp is not released) and
    // the preflight's ty-gate returns `NotApplicable` for the same reason.
    // The load-bearing safety fact is NO OWNER MINT — with no owner there is
    // no second release authority, so the #2648 double-free class cannot fire
    // in a gen body. Pinned for both a local-arg forwarder shape and an
    // inline-literal shape; when generator bodies gain enum layouts (and with
    // them scrutinee owners), these pins must flip to the closure-shim
    // verdicts (reject / admit-with-one-mint).
    for scrutinee_src in [
        r#"
        fn wrap(s: string) -> Result<string, string> { Ok(s) }
        fn use_it() -> i64 {
            var total = 0;
            for v in gen {
                let s = "x";
                let n = match wrap(s) { Ok(_) => 1, Err(_) => 0 };
                yield n;
            } {
                total = total + v;
            }
            total
        }
        "#,
        r#"
        fn wrap(s: string) -> Result<string, string> { Ok(s) }
        fn use_it() -> i64 {
            var total = 0;
            for v in gen {
                let n = match wrap("lit") { Ok(_) => 1, Err(_) => 0 };
                yield n;
            } {
                total = total + v;
            }
            total
        }
        "#,
    ] {
        let p = pipeline(scrutinee_src);
        assert_eq!(
            unrelated_diag_count(&p),
            0,
            "gen-body sources must lower clean: {:#?}",
            p.diagnostics
        );
        assert_eq!(
            count_owner_mints(&p),
            0,
            "no __hew_call_scrutinee owner may exist in a gen body (no layouts, no mint,              no double-free surface)"
        );
        assert_eq!(
            reject_count(&p),
            0,
            "the ty-gate returns NotApplicable in gen bodies today: {:#?}",
            p.diagnostics
        );
    }
}

#[test]
fn guard_buried_return_forwarder_rejects() {
    // The in-lane codegen-review exploit: `evil` forwards its caller-owned
    // borrow `p` through a `return` buried in a match-arm GUARD while its
    // straight-line return is fresh. Missing the guard from the return-value
    // collection read `evil` as Fresh(∅) — the preflight admitted and minted
    // an owner over the forwarded borrow (REJECTS=0 MINTS=1, the double-free
    // class this check closes). The guard path must union {PARAM} → REJECT.
    let src = r#"
        fn evil(p: Result<string, string>, k: i64) -> Result<string, string> {
            let d = match k {
                0 if { return p; } => 0,
                _ => 1,
            };
            if d > 0 { Ok("fresh") } else { Ok("fresh") }
        }
        fn use_it(p: Result<string, string>) -> i64 {
            match evil(p, 0) { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_eq!(
        reject_count(&p),
        1,
        "a guard-buried return-forwarder scrutinee must reject: {:#?}",
        p.diagnostics
    );
    assert!(
        !any_owner_minted(&p),
        "no owner may be minted over the guard-forwarded borrow"
    );
    assert!(!any_call_result_neutralize(&p));
}
