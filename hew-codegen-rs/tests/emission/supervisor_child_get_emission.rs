//! S3 codegen tests for stable supervisor `ChildRef` values.
//!
//! A static child accessor constructs the pointer-free `(supervisor token,
//! slot)` role directly. It must not snapshot the current child through
//! `hew_supervisor_child_get` on any target. Pool lookup is the separate dynamic
//! case and uses the owner-scoped `hew_local_pid_supervisor_pool_child_ref_get`
//! ABI before materialising `Option<ChildRef<T>>`.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Minimal Hew source with a supervisor, one static child, and a function that
/// accesses the child via a supervisor-typed `LocalPid` and returns the stable
/// `ChildRef` role value.
const STATIC_CHILD_ACCESS: &str = r"
actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: one_for_one,
    child worker: Worker
}

fn get_worker(app: LocalPid<App>) -> ChildRef<Worker> {
    app.worker
}
";

/// Pin the textual IR target so these assertions are host-independent.
const SYSV_TRIPLE: &str = "x86_64-unknown-linux-gnu";

/// Compile `STATIC_CHILD_ACCESS` for the pinned SysV target.
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

#[test]
fn static_child_ref_uses_value_representation_on_sysv() {
    let ir = emit_child_access_ir("declares-abi");
    assert!(
        ir.contains("define internal { i64, i64 } @get_worker(ptr"),
        "ChildRef must use its two-word value representation;\ngot:\n{ir}"
    );
    assert!(
        ir.contains("@hew_supervisor_direct_id("),
        "ChildRef construction must capture the stable supervisor token;\ngot:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_supervisor_child_get("),
        "static ChildRef construction must not snapshot a live child;\ngot:\n{ir}"
    );
}

#[test]
fn static_child_ref_materialises_token_and_slot_in_function_body() {
    let ir = emit_child_access_ir("call-in-body");
    assert!(
        ir.contains("%field_0_init_ptr = getelementptr")
            && ir.contains("%field_1_init_ptr = getelementptr")
            && ir.contains("store i64 0, ptr %local_2"),
        "static ChildRef construction must materialise its token and slot zero;\ngot:\n{ir}"
    );
    assert!(
        !ir.contains("@hew_supervisor_child_get(")
            && !ir.contains("@hew_local_pid_supervisor_child_get("),
        "static ChildRef construction must carry the role without either lookup ABI;\ngot:\n{ir}"
    );
}

#[test]
fn static_child_ref_remains_pointer_free_on_windows_msvc() {
    let ir = emit_child_access_ir_for("declares-abi-msvc", Some("x86_64-pc-windows-msvc"));
    assert!(
        ir.contains("define internal { i64, i64 } @get_worker(ptr"),
        "ChildRef must remain a two-word value on MSVC;\ngot:\n{ir}"
    );
    assert!(
        ir.contains("@hew_supervisor_direct_id(") && !ir.contains("@hew_supervisor_child_get("),
        "MSVC ChildRef construction must capture only the stable supervisor token;\ngot:\n{ir}"
    );
}

#[test]
fn static_child_ref_wasm_failure_names_the_actual_substrate() {
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
        .expect_err("WASM emission with supervisor substrate must fail closed");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("hew_supervisor_direct_id") && !msg.contains("hew_supervisor_child_get"),
        "fail-closed error must name the token lookup, not the removed child snapshot; got: {msg}"
    );
}

const POOL_ACCESS: &str = r"
actor Worker {
    receive fn ping() {}
}

supervisor Pool {
    strategy: simple_one_for_one,
    pool workers: Worker count: 2
}

fn inspect(sup: LocalPid<Pool>) -> i64 {
    let workers = sup.workers;
    match workers.get(0) {
        .Some(_) => workers.len(),
        .None => -1,
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
        ir.contains("invoke [2 x i64] @hew_local_pid_supervisor_pool_child_ref_get(i64"),
        "pool get must invoke the owner-scoped ChildRef lookup ABI:\n{ir}"
    );
    assert!(
        ir.contains("pool_get_some") && ir.contains("pool_get_none"),
        "pool get must branch to exact Some/None construction blocks:\n{ir}"
    );
    assert!(
        ir.contains("pool_get_child_ref_token") && ir.contains("pool_get_child_ref_slot"),
        "a valid pool member must populate both ChildRef role words:\n{ir}"
    );
    assert!(
        ir.contains("store i8 0") && ir.contains("store i8 1"),
        "layout-aware Option construction must write both Some and None tags:\n{ir}"
    );
}
