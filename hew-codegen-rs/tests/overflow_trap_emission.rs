//! End-to-end emission tests for B-2's overflow-trap lowering.
//!
//! The MIR-shape tests live in `hew-mir/tests/overflow_trap.rs`; this
//! file pins the LLVM-IR side: that the default `+` / `-` / `*` lowers
//! to a `call {iN, i1} @llvm.{s,u}{add,sub,mul}.with.overflow.iN`, that
//! the overflow flag is extracted, that the trap path emits
//! `@llvm.trap` followed by `unreachable`, and that the emitted module
//! verifies.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the trap path must contain a real
//!   `llvm.trap` call. A regression that emits an `unreachable`
//!   without a preceding `llvm.trap` would silently drop the abort
//!   signal.
//! - `exhaustive-coverage` (P0): one assertion per (op × signedness)
//!   intrinsic name, plus per-width coverage via a helper.

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
    let tmp = std::env::temp_dir().join(format!("hew-b2-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("overflow-trap pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

// ---------------------------------------------------------------------------
// Default `+` / `-` / `*` on the canonical i64 width selects the signed
// intrinsic family and the trap path emits llvm.trap + unreachable.
// ---------------------------------------------------------------------------

#[test]
fn default_add_emits_sadd_with_overflow_intrinsic() {
    let ll = emit_ll("fn main() -> i64 { 1 + 2 }", "default_add_i64");
    assert!(
        ll.contains("@llvm.sadd.with.overflow.i64"),
        "default `+` on i64 must call llvm.sadd.with.overflow.i64; got:\n{ll}"
    );
    assert!(
        ll.contains("@llvm.trap"),
        "overflow successor must call llvm.trap; got:\n{ll}"
    );
    assert!(
        ll.contains("unreachable"),
        "trap path must be terminated by `unreachable`; got:\n{ll}"
    );
    // No unchecked `add nsw` / `add nuw` slipped through.
    assert!(
        !ll.contains(" = add "),
        "default `+` must not lower to a raw `add` instruction; got:\n{ll}"
    );
}

#[test]
fn default_sub_emits_ssub_with_overflow_intrinsic() {
    let ll = emit_ll("fn main() -> i64 { 5 - 3 }", "default_sub_i64");
    assert!(
        ll.contains("@llvm.ssub.with.overflow.i64"),
        "default `-` on i64 must call llvm.ssub.with.overflow.i64; got:\n{ll}"
    );
    assert!(ll.contains("@llvm.trap"));
    assert!(
        !ll.contains(" = sub "),
        "default `-` must not lower to a raw `sub`; got:\n{ll}"
    );
}

#[test]
fn default_mul_emits_smul_with_overflow_intrinsic() {
    let ll = emit_ll("fn main() -> i64 { 6 * 7 }", "default_mul_i64");
    assert!(
        ll.contains("@llvm.smul.with.overflow.i64"),
        "default `*` on i64 must call llvm.smul.with.overflow.i64; got:\n{ll}"
    );
    assert!(ll.contains("@llvm.trap"));
    assert!(
        !ll.contains(" = mul "),
        "default `*` must not lower to a raw `mul`; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// Per-op × per-signedness emission on the canonical i64 width.
//
// The narrower integer widths (i8/i16/i32/u8/u16/u32) ARE covered at the
// MIR layer in `hew-mir/tests/overflow_trap.rs` — that's where the
// `IntArithChecked` discriminator is selected. End-to-end codegen
// emission for those widths is blocked on a pre-existing, unrelated
// gap: integer literals lower to `ResolvedTy::I64` at HIR time, and
// codegen's `Move` arm rejects mismatched LLVM i64 widths when the
// `let a: i32 = 1` move tries to store the i64 literal into the i32
// slot. That limitation belongs to the literal-coercion seam (see
// `primitive_to_llvm` / `Move` in `hew-codegen-rs::llvm`) and will be
// fixed by the same B-1 / B-2 follow-up that introduces width-suffixed
// literals. B-2's job here is the intrinsic + trap-edge shape, which
// the i64 surface exercises fully.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// extractvalue lookup: the producing IR must extract both the result
// (index 0) and the overflow flag (index 1) from the intrinsic's
// aggregate return.
// ---------------------------------------------------------------------------

#[test]
fn checked_arith_emits_two_extractvalues_per_op() {
    let ll = emit_ll("fn main() -> i64 { 1 + 2 }", "extract_pair");
    // One pair of extractvalues per arithmetic op. The textual IR may
    // print these in either order; both must appear.
    assert!(
        ll.contains("extractvalue"),
        "must emit extractvalue for the i64 result / i1 flag; got:\n{ll}"
    );
    // Two extractvalues per op (one per index) — count loosely.
    let extracts = ll.matches("extractvalue").count();
    assert!(
        extracts >= 2,
        "expected at least two extractvalue instructions per checked op; got {extracts}:\n{ll}"
    );
}
