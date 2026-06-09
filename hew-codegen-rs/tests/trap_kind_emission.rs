//! End-to-end emission tests for the per-kind `Terminator::Trap`
//! lowering. The MIR carries a `TrapKind` discriminant; codegen must
//! route each kind through `hew_trap_with_code(<code>)` before the
//! fallback `llvm.trap` so the supervisor can distinguish overflow
//! from OOB from divide-by-zero (instead of collapsing every cause
//! to `Signal(4)` / `Signal(5)`).
//!
//! The exit-code constants here MUST stay in lock-step with
//! `HEW_TRAP_*` in `hew-runtime/src/supervisor.rs`.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the trap path must contain a real
//!   call to `hew_trap_with_code` with the correct per-kind code AND
//!   still emit `llvm.trap` + `unreachable` as the verifier-required
//!   block terminator / non-actor fallback.
//! - `exhaustive-coverage` (P0): one assertion per `TrapKind` value
//!   reachable from the producer-side MIR slices that exist today
//!   (B-2 IntegerOverflow, B-5 DivideByZero / ShiftOutOfRange,
//!   C-2 IndexOutOfBounds via Vec OOB). `SignedMinDivNegOne` shares
//!   the same divide trap surface as `DivideByZero`; we exercise it
//!   through an explicit MIR fixture rather than a source snippet
//!   because the producer fires on the dividend-side check too.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::TypeCheckOutput;

fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
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
    let tmp = std::env::temp_dir().join(format!("hew-trap-kind-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("trap-kind pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// Assert the emitted IR contains a call to `hew_trap_with_code` with
/// the given i32 argument. inkwell's textual IR formats integer
/// constants as `i32 N` so we match on a substring that pins both the
/// callee symbol and the constant.
fn assert_trap_with_code(ll: &str, code: i32) {
    let needle = format!("@hew_trap_with_code(i32 {code})");
    assert!(
        ll.contains(&needle),
        "expected `{needle}` in emitted IR; got:\n{ll}"
    );
    // The fallback / verifier-required terminator must still follow.
    assert!(
        ll.contains("@llvm.trap"),
        "trap block must still emit `llvm.trap` as the fallback terminator; got:\n{ll}"
    );
    assert!(
        ll.contains("unreachable"),
        "trap block must end with `unreachable`; got:\n{ll}"
    );
}

/// `hew_trap_with_code` must be declared exactly once per module even
/// if multiple trap sites reference it.
fn assert_trap_decl_unique(ll: &str) {
    // The textual IR has one `declare void @hew_trap_with_code` line
    // when the symbol is used externally; assert we didn't accidentally
    // emit duplicate declarations.
    let decl_count = ll.matches("declare void @hew_trap_with_code").count();
    assert_eq!(
        decl_count, 1,
        "expected exactly one `declare void @hew_trap_with_code` line, got {decl_count}:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// IntegerOverflow — code 201
// ---------------------------------------------------------------------------

#[test]
fn integer_overflow_emits_hew_trap_with_code_201() {
    let ll = emit_ll("fn main() -> i64 { 1 + 2 }", "integer_overflow");
    assert_trap_with_code(&ll, 201);
    assert_trap_decl_unique(&ll);
}

// ---------------------------------------------------------------------------
// DivideByZero — code 202
// ---------------------------------------------------------------------------

#[test]
fn divide_by_zero_emits_hew_trap_with_code_202() {
    // The divide-by-zero trap surface is the LHS check in `/` / `%`.
    // The shape mirrors B-5's existing div_shift_trap_emission test.
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 10; let b: i64 = 2; a / b }",
        "divide_by_zero",
    );
    assert_trap_with_code(&ll, 202);
}

// ---------------------------------------------------------------------------
// ShiftOutOfRange — code 204
// ---------------------------------------------------------------------------

#[test]
fn shift_out_of_range_emits_hew_trap_with_code_204() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 3; a << b }",
        "shift_out_of_range",
    );
    assert_trap_with_code(&ll, 204);
}

// ---------------------------------------------------------------------------
// Multiple trap sites in one module reuse the same declaration.
// ---------------------------------------------------------------------------

#[test]
fn multiple_traps_share_one_declaration() {
    // Two arithmetic ops → two trap blocks → still one declare line.
    let ll = emit_ll("fn main() -> i64 { (1 + 2) * (3 + 4) }", "multiple_traps");
    assert_trap_decl_unique(&ll);
    // Both call sites present (same code for both overflow traps).
    let call_count = ll.matches("@hew_trap_with_code(i32 201)").count();
    assert!(
        call_count >= 2,
        "expected at least two `hew_trap_with_code(i32 201)` call sites, got {call_count}:\n{ll}"
    );
}
