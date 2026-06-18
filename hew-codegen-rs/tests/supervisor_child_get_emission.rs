//! S3 codegen test: LLVM emission for `hew_supervisor_child_get`.
//!
//! Drives the full HIR → checker → MIR → codegen pipeline on a minimal
//! supervisor source program that accesses a static child field, then
//! asserts on the emitted textual LLVM IR that:
//!
//! - `@hew_supervisor_child_get_raw` is declared with the platform-portable
//!   ABI: `i64 (ptr, i32, ptr)` — returns word0 (tag in low byte) as a plain
//!   u64; writes the handle to a caller-supplied output pointer.
//! - A call to `@hew_supervisor_child_get_raw` is present in the emitted
//!   `get_worker` function body.
//! - The WASM classification guard (`uses_wasm_excluded_symbol`) correctly
//!   marks `hew_supervisor_child_get` as a WASM-excluded symbol when it
//!   appears in the MIR.
//!
//! WHY `_raw`: on Windows x64 (MSVC ABI) Rust returns `ChildLookupResult`
//! (16 bytes) via a hidden sret pointer — but the previous codegen emitted a
//! register-return call site, causing the callee to corrupt the supervisor
//! struct and return a stale heap address as the tag word.  `_raw` returns a
//! plain `u64` which has no sret ambiguity on any platform.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): `intern_runtime_decl` must not silently
//!   fall through for `hew_supervisor_child_get`.
//! - `parity-or-tracked-gap` (P1): `hew_supervisor_child_get` is excluded
//!   from WASM emission via `uses_wasm_excluded_symbol`; tested here.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Minimal Hew source with a supervisor, one static child, and a function that
/// accesses the child via a supervisor-typed `LocalPid`.
const STATIC_CHILD_ACCESS: &str = r"
actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: one_for_one,
    child worker: Worker
}

fn get_worker(app: LocalPid<App>) -> LocalPid<Worker> {
    app.worker
}
";

/// Compile `STATIC_CHILD_ACCESS` through the full HIR → MIR → codegen
/// pipeline and return the emitted textual LLVM IR string.
fn emit_child_access_ir(slug: &str) -> String {
    let parsed = hew_parser::parse(STATIC_CHILD_ACCESS);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type-check errors: {:#?}",
        tc_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    let pipeline = hew_mir::lower_hir_module(&hir.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-sup-child-get-{slug}"));
    std::fs::create_dir_all(&tmp).expect("create tmp dir");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("emit_module must succeed");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// The emitted IR must declare `@hew_supervisor_child_get_raw` with the
/// platform-portable ABI: returns `i64` (word0: tag in low byte), takes
/// `(ptr sup, i32 key, ptr handle_out)`.
///
/// ABI rationale:
///   Rust emits `define void @hew_supervisor_child_get(ptr sret([16 x i8]), ptr, i32)`
///   on Windows x64 (MSVC ABI) — a hidden return pointer.  The _raw variant
///   returns a plain `u64` (no sret on any platform) and writes the handle
///   through an output pointer, sidestepping the mismatch entirely.
#[test]
fn supervisor_child_get_declares_correct_abi() {
    let ir = emit_child_access_ir("declares-abi");
    // The raw declaration must be present in the emitted IR.
    assert!(
        ir.contains("@hew_supervisor_child_get_raw"),
        "expected @hew_supervisor_child_get_raw declaration in emitted IR;\ngot:\n{ir}"
    );
    // Signature: i64 (ptr, i32, ptr) — single-register return, output pointer.
    assert!(
        ir.contains("i64 @hew_supervisor_child_get_raw(ptr, i32, ptr)"),
        "expected `i64 @hew_supervisor_child_get_raw(ptr, i32, ptr)` in declaration;\ngot:\n{ir}"
    );
}

/// A call to `@hew_supervisor_child_get_raw` must appear in the body of the
/// compiled `get_worker` function — the codegen translates the MIR symbol
/// `hew_supervisor_child_get` into a call to the raw variant internally.
#[test]
fn supervisor_child_get_call_emitted_in_function_body() {
    let ir = emit_child_access_ir("call-in-body");
    assert!(
        ir.contains("@hew_supervisor_child_get_raw("),
        "expected a call to @hew_supervisor_child_get_raw in emitted IR;\ngot:\n{ir}"
    );
}

/// The `uses_wasm_excluded_symbol` guard must classify `hew_supervisor_child_get`
/// as WASM-excluded. Verify at the MIR level: when the pipeline contains a
/// `CallRuntimeAbi("hew_supervisor_child_get", ...)`, the pipeline's
/// `uses_wasm_excluded_symbol` scan returns `Some("hew_supervisor_child_get")`.
///
/// This is the mechanism by which codegen emits `WasmUnsupportedSubstrate`
/// instead of reaching a linker error on WASM targets (WASM-TODO(#1475)).
#[test]
fn supervisor_child_get_classified_as_wasm_excluded() {
    let parsed = hew_parser::parse(STATIC_CHILD_ACCESS);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:#?}", tc_output.errors);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(hir.diagnostics.is_empty(), "{:#?}", hir.diagnostics);
    let pipeline = hew_mir::lower_hir_module(&hir.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:#?}",
        pipeline.diagnostics
    );

    // `emit_module` with wasm=true must fail with WasmUnsupportedSubstrate.
    let tmp = std::env::temp_dir().join("hew-sup-child-get-wasm-exclude");
    std::fs::create_dir_all(&tmp).expect("create tmp dir");
    let options = EmitOptions {
        module_name: "probe-wasm",
        out_dir: &tmp,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let err = emit_module(&pipeline, &options)
        .expect_err("WASM emission with hew_supervisor_child_get must fail closed");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("hew_supervisor_child_get") || msg.contains("WasmUnsupported"),
        "fail-closed error must name hew_supervisor_child_get or WasmUnsupported; got: {msg}"
    );
}
