//! S3 codegen test: LLVM emission for `hew_supervisor_child_get`.
//!
//! Drives the full HIR → checker → MIR → codegen pipeline on a minimal
//! supervisor source program that accesses a static child field, then asserts
//! on the emitted textual LLVM IR that the R5 ABI classifier emits the
//! per-target canonical `@hew_supervisor_child_get` (the `_raw` out-pointer
//! twin is removed):
//!
//! - On SysV/AAPCS (linux / aarch64-darwin) the 16-byte `ChildLookupResult` is
//!   a register pair: the symbol is declared `[2 x i64] (ptr, i32)` and called
//!   by value — NO sret, NO `_raw`.
//! - On Windows x64 MSVC the aggregate is returned INDIRECTLY: the symbol is
//!   declared `void (ptr noalias sret(...), ptr, i32)` and the caller allocates
//!   the result slot. This is the correct ABI the `_raw` out-pointer twin used
//!   to fake; the `sret(T)` attribute makes the canonical symbol right on MSVC.
//! - The WASM classification guard (`uses_wasm_excluded_symbol`) correctly
//!   marks `hew_supervisor_child_get` as a WASM-excluded symbol when it
//!   appears in the MIR.
//!
//! Trap-206 regression guard: the historical trap 206 was declaring the
//! field-accurate struct WITHOUT a matching ABI attribute, letting LLVM pick
//! indirect-sret on MSVC while the caller read a register pair. The SysV
//! register-pair assertion + the MSVC sret assertion together pin the declared
//! carrier and the attribute to AGREE on every target.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the classifier must not silently fall through
//!   for `hew_supervisor_child_get`.
//! - `aggregate-abi-by-classifier-not-per-symbol` (P0): one classifier keyed on
//!   `(type, target)`, not a hand-encoded register-pair plus a `_raw` twin.
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

/// The SysV/AAPCS triple the register-pair assertions classify against.
///
/// PINNED, not host (`None`): the register-pair ABI is the SysV/AAPCS shape,
/// which is NOT the host shape on the Windows MSVC runner (there the 16-byte
/// aggregate classifies `Indirect`/sret). A host-naive `None` made these tests
/// silently target whatever the runner's host ABI was — green on Linux/macOS,
/// red on the Windows runner where the host is `x86_64-pc-windows-msvc`. The
/// textual `.ll` is classified against the requested triple (the diagnostic IR
/// matches the object emission's ABI), so pinning SysV asserts the SysV shape
/// on every host. The MSVC shape has its own triple-pinned test below.
const SYSV_TRIPLE: &str = "x86_64-unknown-linux-gnu";

/// Compile `STATIC_CHILD_ACCESS` for an explicit SysV/AAPCS target and return
/// the emitted textual LLVM IR string. SysV is pinned (not host) so the
/// register-pair assertions hold on every CI host, including the Windows MSVC
/// runner whose host ABI returns the 16-byte aggregate indirectly.
fn emit_child_access_ir(slug: &str) -> String {
    emit_child_access_ir_for(slug, Some(SYSV_TRIPLE))
}

/// Compile `STATIC_CHILD_ACCESS` through the full HIR → MIR → codegen pipeline
/// for `target_triple` (`None` = host) and return the emitted textual LLVM IR.
/// The textual `.ll` is classified against the requested target, so the
/// aggregate ABI shape in the IR reflects that target's C ABI.
fn emit_child_access_ir_for(slug: &str, target_triple: Option<&str>) -> String {
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
        target_triple,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("emit_module must succeed");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// On SysV/AAPCS (pinned `x86_64-unknown-linux-gnu`) the canonical
/// `@hew_supervisor_child_get` is declared with the classified register-pair
/// ABI: `[2 x i64] (ptr, i32)`. The removed `_raw` out-pointer twin must NOT
/// appear.
///
/// ABI rationale:
///   The 16-byte `ChildLookupResult` aggregate classifies `RegisterPair` on
///   SysV/AAPCS, so LLVM returns it in `x0:x1` (aarch64) / `rax:rdx` (x86_64)
///   as a `[2 x i64]` — exactly the shape the `#[repr(C)]` Rust callee returns.
///   The classifier replaces the old hand-encoded `{ i64, i64 }` decl plus the
///   `_raw` out-pointer twin with one classified declaration.
#[test]
fn supervisor_child_get_declares_register_pair_abi_on_sysv() {
    let ir = emit_child_access_ir("declares-abi");
    // The removed `_raw` twin must be absent everywhere.
    assert!(
        !ir.contains("hew_supervisor_child_get_raw"),
        "the removed _raw twin must NOT appear in emitted IR;\ngot:\n{ir}"
    );
    // Register-pair declaration: `[2 x i64] (ptr, i32)` — no sret, by-value.
    assert!(
        ir.contains("declare [2 x i64] @hew_supervisor_child_get(ptr, i32)"),
        "expected `declare [2 x i64] @hew_supervisor_child_get(ptr, i32)`;\ngot:\n{ir}"
    );
    // NEGATIVE: the SysV declaration must NOT be an sret (that would be the
    // MSVC shape leaking onto a register-pair target — the trap-206 mismatch).
    assert!(
        !ir.contains("sret") || !ir.contains("@hew_supervisor_child_get(ptr noalias sret"),
        "SysV target must NOT emit an sret return for hew_supervisor_child_get;\ngot:\n{ir}"
    );
}

/// A by-value call to the register-pair `@hew_supervisor_child_get` must appear
/// in the body of the compiled `get_worker` function — the codegen routes the
/// MIR symbol through the classified canonical symbol, not the removed twin.
#[test]
fn supervisor_child_get_call_emitted_in_function_body() {
    let ir = emit_child_access_ir("call-in-body");
    assert!(
        !ir.contains("hew_supervisor_child_get_raw"),
        "the removed _raw twin must NOT be called;\ngot:\n{ir}"
    );
    assert!(
        ir.contains("call [2 x i64] @hew_supervisor_child_get("),
        "expected a register-pair call to @hew_supervisor_child_get;\ngot:\n{ir}"
    );
}

/// On Windows x64 MSVC the 16-byte aggregate classifies `Indirect`, so the
/// canonical `@hew_supervisor_child_get` is declared `void` with a leading
/// `sret(...) noalias` pointer parameter and the caller allocates the result
/// slot. This is the correct MSVC ABI the `_raw` twin used to fake — the
/// register-pair (SysV) + sret (MSVC) pair is the trap-206 regression guard.
#[test]
fn supervisor_child_get_declares_sret_abi_on_windows_msvc() {
    let ir = emit_child_access_ir_for("declares-abi-msvc", Some("x86_64-pc-windows-msvc"));
    // No removed twin on MSVC either.
    assert!(
        !ir.contains("hew_supervisor_child_get_raw"),
        "the removed _raw twin must NOT appear on MSVC;\ngot:\n{ir}"
    );
    // Indirect declaration: void return, leading `ptr noalias sret(...)` param.
    assert!(
        ir.contains(
            "declare void @hew_supervisor_child_get(ptr noalias sret({ i8, i8, [6 x i8], ptr }), ptr, i32)"
        ),
        "expected the MSVC sret declaration of @hew_supervisor_child_get;\ngot:\n{ir}"
    );
    // The caller must allocate the result slot and pass it by sret.
    assert!(
        ir.contains("call void @hew_supervisor_child_get(ptr %child_result_sret"),
        "expected the MSVC sret call passing the caller-allocated result slot;\ngot:\n{ir}"
    );
    // NEGATIVE: MSVC must NOT emit the register-pair return shape (the
    // historical trap-206 mismatch where the caller read a stale register pair).
    assert!(
        !ir.contains("[2 x i64] @hew_supervisor_child_get"),
        "MSVC must NOT emit the register-pair return for hew_supervisor_child_get;\ngot:\n{ir}"
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
        opt_level: hew_codegen_rs::OptLevel::O0,
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

const POOL_ACCESS: &str = r"
actor Worker {
    receive fn ping() {}
}

supervisor Pool {
    strategy: simple_one_for_one,
    pool workers: Worker(count: 2)
}

fn inspect(sup: LocalPid<Pool>) -> i64 {
    let workers = sup.workers;
    match workers.get(0) {
        Some(_) => workers.len(),
        None => -1,
    }
}
";

#[test]
fn supervisor_pool_get_materialises_option_and_bound_pool_view() {
    let parsed = hew_parser::parse(POOL_ACCESS);
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
    let tmp = std::env::temp_dir().join("hew-supervisor-pool-get-option");
    std::fs::create_dir_all(&tmp).expect("create tmp dir");
    let options = EmitOptions {
        module_name: "pool-option",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: Some(SYSV_TRIPLE),
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("pool Option emission must succeed");
    let ll_path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    let ir = std::fs::read_to_string(ll_path).expect("read emitted .ll");
    assert!(
        ir.contains("call [2 x i64] @hew_supervisor_pool_child_get("),
        "pool get must call the canonical aggregate-return runtime ABI:\n{ir}"
    );
    assert!(
        ir.contains("pool_get_some") && ir.contains("pool_get_none"),
        "pool get must branch to exact Some/None construction blocks:\n{ir}"
    );
    assert!(
        ir.contains("pool_get_handle_ptr_value = inttoptr i64"),
        "the live handle must populate the LocalPid payload through inttoptr:\n{ir}"
    );
    assert!(
        ir.contains("store i8 0") && ir.contains("store i8 1"),
        "layout-aware Option construction must write both Some and None tags:\n{ir}"
    );
}
