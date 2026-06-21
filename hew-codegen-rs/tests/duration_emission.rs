//! End-to-end LLVM-IR emission tests for `DurationLit` codegen.
//!
//! A Hew duration literal must emit an `i64` constant whose value is the
//! nanosecond representation of the literal. `5s` is 5,000,000,000 ns.
//!
//! The function returns `i64` to stay within the Cluster 1 return-type gate;
//! the duration local exercises the `DurationLit` alloca+store path without
//! requiring a `Duration`-returning function.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): absence of the `Unsupported` error from
//!   `primitive_to_llvm` for `ResolvedTy::Duration` is the gate condition.

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
    let tmp = std::env::temp_dir().join(format!("hew-duration-{module_name}"));
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
        emit_module(&pipeline, &options).expect("duration pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

// ---------------------------------------------------------------------------
// DurationLit: `5s` (5,000,000,000 ns) must emit i64 constant 5000000000.
// ---------------------------------------------------------------------------

#[test]
fn duration_emission_lit_emits_i64_nanoseconds() {
    // `5s` = 5,000,000,000 nanoseconds. The duration local is allocated as
    // i64 (from `primitive_to_llvm(Duration)`) and the `DurationLit` arm
    // stores the constant nanosecond value into it.
    let ll = emit_ll(
        "fn main() -> i64 { let d: duration = 5s; 0 }",
        "duration_lit_5s",
    );
    // 5 seconds = 5_000_000_000 nanoseconds. LLVM renders i64 constants
    // with their decimal value.
    assert!(
        ll.contains("i64 5000000000"),
        "DurationLit 5s must emit i64 constant 5000000000; got:\n{ll}"
    );
}
