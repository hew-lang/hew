//! #2648 — call-scrutinee return-provenance PREFLIGHT wiring (S2).
//!
//! These tests exercise the full pipeline (parse → typecheck → HIR lower → MIR
//! lower) and assert the preflight admission classifier's behaviour at the
//! call-scrutinee consumers:
//!
//! - a forwarder whose summary carries `PARAM` (`fn passthru(x) { x }`) used as a
//!   match / while-let / let-else / if-let / discarded scrutinee REJECTS with
//!   exactly one `NotYetImplemented` diagnostic and NO partial MIR (no
//!   `__hew_call_scrutinee` owner mint, no `NeutralizePayloadSlot`);
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

/// True when ANY lowered function emits a `NeutralizePayloadSlot` instruction.
fn any_neutralize(p: &IrPipeline) -> bool {
    p.raw_mir.iter().any(|f| {
        f.blocks.iter().any(|b| {
            b.instructions
                .iter()
                .any(|i| matches!(i, hew_mir::Instr::NeutralizePayloadSlot { .. }))
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
        !any_neutralize(&p),
        "a rejected forwarder scrutinee must emit NO NeutralizePayloadSlot"
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
    assert!(
        any_owner_minted(&p),
        "a fresh heap-enum producer scrutinee mints the __hew_call_scrutinee owner"
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
    assert!(
        any_owner_minted(&p),
        "the interim LegacyModuleCall path preserves the owner mint byte-for-byte"
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
