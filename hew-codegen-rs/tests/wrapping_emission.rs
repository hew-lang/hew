//! End-to-end LLVM-IR emission tests for B-4's wrapping operator sugar.
//!
//! `&+` / `&-` / `&*` must emit plain LLVM `add` / `sub` / `mul`
//! instructions — no `llvm.*.with.overflow.*` intrinsic, no `@llvm.trap`
//! call, no `unreachable` terminator from the overflow path.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the *absence* of overflow intrinsics is
//!   the correctness invariant. A regression that accidentally routes `&+`
//!   through `IntArithChecked` would silently add a trap path the user
//!   explicitly opted out of by choosing `&+`.
//! - `exhaustive-coverage` (P0): one test per operator.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::TypeCheckOutput;

fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);
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
    let tmp = std::env::temp_dir().join(format!("hew-b4-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("wrapping pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

// ---------------------------------------------------------------------------
// `&+` emits plain `add`, not a with.overflow intrinsic.
// ---------------------------------------------------------------------------

#[test]
fn wrapping_emission_add_emits_plain_add_not_intrinsic() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 2; a &+ b }",
        "wrap_add_i64",
    );
    // Must contain a plain `add` instruction.
    assert!(
        ll.contains(" = add "),
        "`&+` must emit a plain LLVM `add`; got:\n{ll}"
    );
    // Must NOT call any with.overflow intrinsic.
    assert!(
        !ll.contains("with.overflow"),
        "`&+` must not call any with.overflow intrinsic; got:\n{ll}"
    );
    // Must NOT emit a trap call (no overflow path exists).
    assert!(
        !ll.contains("@llvm.trap"),
        "`&+` must not emit @llvm.trap; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// `&-` emits plain `sub`, not a with.overflow intrinsic.
// ---------------------------------------------------------------------------

#[test]
fn wrapping_emission_sub_emits_plain_sub_not_intrinsic() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 5; let b: i64 = 3; a &- b }",
        "wrap_sub_i64",
    );
    assert!(
        ll.contains(" = sub "),
        "`&-` must emit a plain LLVM `sub`; got:\n{ll}"
    );
    assert!(
        !ll.contains("with.overflow"),
        "`&-` must not call any with.overflow intrinsic; got:\n{ll}"
    );
    assert!(
        !ll.contains("@llvm.trap"),
        "`&-` must not emit @llvm.trap; got:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// `&*` emits plain `mul`, not a with.overflow intrinsic.
// ---------------------------------------------------------------------------

#[test]
fn wrapping_emission_mul_emits_plain_mul_not_intrinsic() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 7; let b: i64 = 6; a &* b }",
        "wrap_mul_i64",
    );
    assert!(
        ll.contains(" = mul "),
        "`&*` must emit a plain LLVM `mul`; got:\n{ll}"
    );
    assert!(
        !ll.contains("with.overflow"),
        "`&*` must not call any with.overflow intrinsic; got:\n{ll}"
    );
    assert!(
        !ll.contains("@llvm.trap"),
        "`&*` must not emit @llvm.trap; got:\n{ll}"
    );
}
