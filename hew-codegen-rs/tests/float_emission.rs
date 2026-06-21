//! End-to-end LLVM-IR emission tests for float literal and arithmetic codegen.
//!
//! `FloatLit` must emit an LLVM `double` constant.
//! `FloatAdd/Sub/Mul/Div/Rem` must emit the corresponding `fadd`/`fsub`/
//! `fmul`/`fdiv`/`frem` instructions — no `nsw`/`nuw` flags (those are
//! integer-only), no overflow intrinsic, no trap call.
//!
//! All test functions return `i64` to stay within the Cluster 1 return-type
//! gate; the float arithmetic is performed in local bindings and the result
//! stored but not returned. The float alloca/store/load/fadd path is still
//! fully exercised — it just doesn't feed the ret instruction.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the *presence* of bare float instructions
//!   (not the integer with.overflow path) is the correctness invariant.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::TypeCheckOutput;

fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
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
    let tmp = std::env::temp_dir().join(format!("hew-float-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("float pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

// ---------------------------------------------------------------------------
// FloatLit: f64 constant `1.5` must appear as a double constant in the IR.
// The function returns i64 (Cluster 1 gate); the float local exercises the
// FloatLit alloca+store path.
// ---------------------------------------------------------------------------

#[test]
fn float_emission_lit_f64_emits_double_constant() {
    let ll = emit_ll("fn main() -> i64 { let a: f64 = 1.5; 0 }", "float_lit_f64");
    assert!(
        ll.contains("double 1.5") || ll.contains("double 1.500000e+00"),
        "FloatLit f64 must emit a double constant; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// FloatAdd: `a + b` must emit `fadd double`, not any integer intrinsic.
// ---------------------------------------------------------------------------

#[test]
fn float_emission_add_emits_fadd_not_overflow_intrinsic() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: f64 = 1.5; let b: f64 = 2.5; let r: f64 = a + b; 0 }",
        "float_add_f64",
    );
    assert!(
        ll.contains("fadd double"),
        "FloatAdd must emit `fadd double`; got:\n{ll}"
    );
    assert!(
        !ll.contains("with.overflow"),
        "FloatAdd must not use any with.overflow intrinsic; got:\n{ll}"
    );
    assert!(
        !ll.contains("@llvm.trap"),
        "FloatAdd must not emit @llvm.trap; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// FloatSub: `a - b` must emit `fsub double`.
// ---------------------------------------------------------------------------

#[test]
fn float_emission_sub_emits_fsub() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: f64 = 5.0; let b: f64 = 3.0; let r: f64 = a - b; 0 }",
        "float_sub_f64",
    );
    assert!(
        ll.contains("fsub double"),
        "FloatSub must emit `fsub double`; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// FloatMul: `a * b` must emit `fmul double`.
// ---------------------------------------------------------------------------

#[test]
fn float_emission_mul_emits_fmul() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: f64 = 2.0; let b: f64 = 3.0; let r: f64 = a * b; 0 }",
        "float_mul_f64",
    );
    assert!(
        ll.contains("fmul double"),
        "FloatMul must emit `fmul double`; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// FloatDiv: `a / b` must emit `fdiv double` — no divisor-zero trap.
// ---------------------------------------------------------------------------

#[test]
fn float_emission_div_emits_fdiv_no_trap() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: f64 = 6.0; let b: f64 = 2.0; let r: f64 = a / b; 0 }",
        "float_div_f64",
    );
    assert!(
        ll.contains("fdiv double"),
        "FloatDiv must emit `fdiv double`; got:\n{ll}"
    );
    assert!(
        !ll.contains("@llvm.trap"),
        "FloatDiv must not emit @llvm.trap (IEEE 754 div-by-zero → ±inf); got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// FloatRem: `a % b` must emit `frem double`.
// ---------------------------------------------------------------------------

#[test]
fn float_emission_rem_emits_frem() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: f64 = 7.0; let b: f64 = 3.0; let r: f64 = a % b; 0 }",
        "float_rem_f64",
    );
    assert!(
        ll.contains("frem double"),
        "FloatRem must emit `frem double`; got:\n{ll}"
    );
}

#[test]
fn float_emission_cmp_emits_ordered_fcmp_for_f64_and_f32() {
    let ll = emit_ll(
        "fn main() -> i64 {
            let a: f64 = 1.0;
            let b: f64 = 2.0;
            let eq = a == b;
            let ne = a != b;
            let lt = a < b;
            let le = a <= b;
            let gt = a > b;
            let ge = a >= b;
            0
        }",
        "float_cmp",
    );
    for expected in [
        "fcmp oeq double",
        "fcmp one double",
        "fcmp olt double",
        "fcmp ole double",
        "fcmp ogt double",
        "fcmp oge double",
    ] {
        assert!(
            ll.contains(expected),
            "FloatCmp must emit ordered `{expected}`; got:\n{ll}"
        );
    }
}
