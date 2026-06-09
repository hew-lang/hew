//! End-to-end LLVM-IR emission tests for `CharLit` codegen.
//!
//! A Hew `char` literal must emit an `i32` constant whose value is the
//! Unicode scalar value of the character. `'A'` is U+0041 (65 decimal).
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): absence of the `Unsupported` error from
//!   `primitive_to_llvm` for `ResolvedTy::Char` is the gate condition.

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
    let tmp = std::env::temp_dir().join(format!("hew-char-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(&pipeline, &options).expect("char pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

// ---------------------------------------------------------------------------
// CharLit: `'A'` (U+0041 = 65) must emit an i32 constant value 65.
// ---------------------------------------------------------------------------

#[test]
fn char_emission_lit_emits_i32_scalar_value() {
    // The function stores 'A' (scalar value 65) into a `char` local; the
    // function itself returns i64 so we stay inside Cluster 1's supported
    // return-type set. The `char` local is allocated as i32 (from
    // `primitive_to_llvm(Char)`) and the `CharLit` arm stores the constant
    // 65 into it. The emitted IR must contain `i32 65`.
    let ll = emit_ll("fn main() -> i64 { let c: char = 'A'; 42 }", "char_lit_a");
    // LLVM must contain the constant value 65 somewhere in the emitted IR.
    // The CharLit alloca is i32 and the stored constant is the scalar value.
    assert!(
        ll.contains("i32 65"),
        "CharLit 'A' must emit i32 constant 65; got:\n{ll}"
    );
}
