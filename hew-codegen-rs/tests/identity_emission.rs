//! LLVM IR emission tests for D-3 `is` operator identity comparison.
//!
//! The MIR-shape tests live in `hew-mir/tests/identity_lowering.rs`; this
//! file pins the LLVM-IR side: that `lhs is rhs` lowers to an `icmp eq`
//! instruction in the emitted `.ll` and that the module verifies.
//!
//! Operand types used here are integer-shaped (`i64`) because those are
//! within the current spine subset that reaches the LLVM emitter without
//! a `D10 violation`.  Pointer-shaped operands (`Duplex`, actor handles,
//! heap-backed types) exercise the `ptrtoint` path; that requires the
//! Duplex/actor construction surface to be in the spine subset, which is
//! not yet wired end-to-end (pending actor-call-lowering cluster).  The
//! `icmp eq` shape is shared across both integer and pointer paths; this
//! test pins its presence.
//!
//! LESSONS applied:
//! - `checker-authority` (P0): operand-type dispatch (ptrtoint vs.
//!   integer cast) is read off the LLVM value kind at codegen time, never
//!   re-derived from the checker's allowance set.
//! - `exhaustive-coverage` (P0): one assertion per code-path shape in the
//!   `Instr::IdentityCompare` arm (i64-value path exercised here; pointer
//!   path asserted structurally in `llvm.rs`).

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::TypeCheckOutput;

fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir: {:?} verify: {:?}",
        output.diagnostics,
        verify
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-d3-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(&pipeline, &options)
        .expect("D-3 identity-compare pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

#[test]
fn identity_compare_emits_icmp_eq() {
    // `a is b` on integer-shaped operands must lower to `icmp eq` in the
    // emitted LLVM IR. The checker (D-2) gates the allowance set; the
    // codegen test runs below the checker and pins the IR shape.
    let ll = emit_ll(
        "fn f() -> i64 { let a: i64 = 1; let b: i64 = 2; let r = a is b; 0 }",
        "identity_cmp_icmp_eq",
    );
    assert!(
        ll.contains("icmp eq"),
        "`a is b` must emit `icmp eq` in LLVM IR; got:\n{ll}"
    );
}

#[test]
fn identity_compare_module_verifies() {
    // The emitted module must pass LLVM's verifier (emit_module runs it
    // internally; a failing verify returns Err). This pins that the
    // ptrtoint/icmp/zext sequence is well-typed by LLVM standards.
    // `emit_ll` already asserts `emit_module` returns Ok; this test adds
    // an explicit name so regressions are immediately attributable.
    let ll = emit_ll(
        "fn f() -> i64 { let a: i64 = 3; let b: i64 = 3; let r = a is b; 0 }",
        "identity_cmp_verify",
    );
    // Module is non-empty — a verified module always has at least a function body.
    assert!(!ll.is_empty(), "emitted module must not be empty");
    assert!(
        ll.contains("define"),
        "emitted module must define at least one function; got:\n{ll}"
    );
}
