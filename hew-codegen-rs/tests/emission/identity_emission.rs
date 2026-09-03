//! LLVM IR emission tests for D-3 `is` operator identity comparison.
//!
//! The MIR-shape tests live in `hew-mir/tests/identity_lowering.rs`; this
//! file pins the LLVM-IR side: that `lhs is rhs` lowers to an `icmp eq`
//! instruction in the emitted `.ll` and that the module verifies.
//!
//! Two operand shapes are covered. Integer-shaped (`i64`) operands are
//! within the current spine subset and pin the shared `icmp eq` tail.
//! `bytes` operands pin the struct path: a `bytes` local is a
//! `BytesTriple { ptr, offset: u32, len: u32 }`, so the identity word is
//! field 0 rather than the whole slot. Loading the slot whole is what made
//! `is` on `bytes` fail closed in the codegen front (#3134).
//!
//! LESSONS applied:
//! - `checker-authority` (P0): operand-type dispatch (buffer GEP vs.
//!   ptrtoint vs. integer cast) is read off the operand's own resolved type
//!   at codegen time, never re-derived from the checker's allowance set.
//! - `exhaustive-coverage` (P0): one assertion per code-path shape in the
//!   `Instr::IdentityCompare` arm (i64-value and `bytes` struct paths
//!   exercised here; the plain pointer path is asserted structurally in
//!   `llvm.rs`).

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};

fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &crate::mir_fixture::checker_output(&parsed.program),
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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

#[test]
fn identity_compare_on_bytes_compares_the_buffer_pointer() {
    // Regression for #3134: a `bytes` local is a three-field struct, so
    // loading the whole slot yields a `StructValue` and the arm used to
    // fail closed with "IdentityCompare lhs must be a pointer or integer
    // value". `emit_ll` panics on that error, so reaching the assertion at
    // all is half the test; the `ptrtoint` proves the emitted comparison is
    // on the buffer pointer rather than on some widened tag.
    let ll = emit_ll(
        "fn f() -> i64 { let a = bytes.new(); let b = bytes.new(); let r = a is b; 0 }",
        "identity_cmp_bytes",
    );
    assert!(
        ll.contains("ptrtoint"),
        "`a is b` on `bytes` must compare the buffer pointer via `ptrtoint`; got:\n{ll}"
    );
    assert!(
        ll.contains("icmp eq"),
        "`a is b` on `bytes` must reach the shared `icmp eq` tail; got:\n{ll}"
    );
}
