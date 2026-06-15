//! End-to-end LLVM-IR emission tests for B-5: divide-by-zero,
//! signed-MIN/-1, and shift-out-of-range traps.
//!
//! These tests pin the LLVM-IR shape — that the correct div/rem/shift
//! instructions are emitted (`sdiv`, `udiv`, `srem`, `urem`, `shl`,
//! `ashr`, `lshr`), that `@llvm.trap` appears on the trap path, and
//! that the emitted module verifies cleanly.
//!
//! MIR-level shape is covered in `hew-mir/tests/div_shift_trap.rs`;
//! this file focuses on the LLVM-IR surface.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): every divide and shift must emit a
//!   reachable `@llvm.trap`. A regression that emits `sdiv` without a
//!   preceding divisor check would silently allow UB.

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
    assert!(
        output.diagnostics.is_empty() && verify_hir(&output.module).is_empty(),
        "hir: {:?} verify: {:?}",
        output.diagnostics,
        verify_hir(&output.module)
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-b5-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("div/shift trap pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

// ---------------------------------------------------------------------------
// Signed integer division emits `sdiv` + trap paths
// ---------------------------------------------------------------------------

#[test]
fn signed_div_emits_sdiv_and_trap() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 10; let b: i64 = 2; a / b }",
        "signed_div_i64",
    );
    assert!(
        ll.contains("sdiv"),
        "signed `/` on i64 must emit `sdiv`; got:\n{ll}"
    );
    assert!(
        ll.contains("@llvm.trap"),
        "signed `/` must emit @llvm.trap for divide-by-zero; got:\n{ll}"
    );
    assert!(
        ll.contains("unreachable"),
        "trap path must end with unreachable; got:\n{ll}"
    );
    // Must NOT emit a raw unchecked `udiv` for a signed operation.
    assert!(
        !ll.contains("udiv"),
        "signed `/` must not emit udiv; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// Unsigned integer division emits `udiv` + trap path
// ---------------------------------------------------------------------------

#[test]
fn unsigned_div_emits_udiv_and_trap() {
    let ll = emit_ll(
        "fn main() -> u64 { let a: u64 = 10; let b: u64 = 2; a / b }",
        "unsigned_div_u64",
    );
    assert!(
        ll.contains("udiv"),
        "unsigned `/` on u64 must emit `udiv`; got:\n{ll}"
    );
    assert!(
        ll.contains("@llvm.trap"),
        "unsigned `/` must emit @llvm.trap; got:\n{ll}"
    );
    // Must NOT emit sdiv for an unsigned operation.
    assert!(
        !ll.contains("sdiv"),
        "unsigned `/` must not emit sdiv; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// Signed integer remainder emits `srem`
// ---------------------------------------------------------------------------

#[test]
fn signed_rem_emits_srem_and_trap() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 10; let b: i64 = 3; a % b }",
        "signed_rem_i64",
    );
    assert!(
        ll.contains("srem"),
        "signed `%` on i64 must emit `srem`; got:\n{ll}"
    );
    assert!(
        ll.contains("@llvm.trap"),
        "signed `%` must emit @llvm.trap; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// Unsigned integer remainder emits `urem`
// ---------------------------------------------------------------------------

#[test]
fn unsigned_rem_emits_urem_and_trap() {
    // Use u64 to avoid the i64-literal-coercion gap (documented in
    // overflow_trap_emission.rs: narrower widths trigger a Move type mismatch
    // until B-1 literal-coercion is wired end-to-end).
    let ll = emit_ll(
        "fn main() -> u64 { let a: u64 = 10; let b: u64 = 3; a % b }",
        "unsigned_rem_u64",
    );
    assert!(
        ll.contains("urem"),
        "unsigned `%` on u32 must emit `urem`; got:\n{ll}"
    );
    assert!(
        ll.contains("@llvm.trap"),
        "unsigned `%` must emit @llvm.trap; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// Left shift emits `shl` + trap path
// ---------------------------------------------------------------------------

#[test]
fn shl_emits_shl_instruction_and_trap() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 3; a << b }",
        "shl_i64",
    );
    assert!(
        ll.contains(" shl "),
        "<<` on i64 must emit `shl`; got:\n{ll}"
    );
    assert!(
        ll.contains("@llvm.trap"),
        "`<<` must emit @llvm.trap for out-of-range shift; got:\n{ll}"
    );
    assert!(
        ll.contains("unreachable"),
        "trap path must end with unreachable; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// Signed right shift emits `ashr` (arithmetic)
// ---------------------------------------------------------------------------

#[test]
fn shr_signed_emits_ashr_and_trap() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 8; let b: i64 = 1; a >> b }",
        "shr_signed_i64",
    );
    assert!(
        ll.contains("ashr"),
        "signed `>>` on i64 must emit `ashr`; got:\n{ll}"
    );
    assert!(
        ll.contains("@llvm.trap"),
        "signed `>>` must emit @llvm.trap; got:\n{ll}"
    );
    // Must NOT emit lshr for a signed right-shift.
    assert!(
        !ll.contains("lshr"),
        "signed `>>` must not emit lshr; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// Unsigned right shift emits `lshr` (logical)
// ---------------------------------------------------------------------------

#[test]
fn shr_unsigned_emits_lshr_and_trap() {
    // Use u64 to avoid the i64-literal-coercion gap (same limitation as
    // documented in overflow_trap_emission.rs for narrower widths).
    let ll = emit_ll(
        "fn main() -> u64 { let a: u64 = 8; let b: u64 = 1; a >> b }",
        "shr_unsigned_u64",
    );
    assert!(
        ll.contains("lshr"),
        "unsigned `>>` on u32 must emit `lshr`; got:\n{ll}"
    );
    assert!(
        ll.contains("@llvm.trap"),
        "unsigned `>>` must emit @llvm.trap; got:\n{ll}"
    );
    // Must NOT emit ashr for an unsigned right-shift.
    assert!(
        !ll.contains("ashr"),
        "unsigned `>>` must not emit ashr; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// UGE compare is present in shift IR (the unsigned-≥-width check)
// ---------------------------------------------------------------------------

#[test]
fn shl_emits_uge_compare_for_range_check() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 3; a << b }",
        "shl_uge_check",
    );
    // LLVM IR uses `icmp uge` for unsigned ≥ comparisons.
    assert!(
        ll.contains("icmp uge"),
        "`<<` must emit `icmp uge` for the out-of-range shift check; got:\n{ll}"
    );
}
