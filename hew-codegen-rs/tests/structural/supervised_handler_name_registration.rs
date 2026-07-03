//! Codegen test: a SUPERVISED actor's handler names are registered into the
//! runtime profiler registry so a crash inside a supervised handler is reported
//! by name (`Worker::run`) instead of the bare `msg_type` discriminant.
//!
//! Background: the direct-`spawn` path emits the handler-name registration via
//! `emit_native_actor_metadata_registration`. A supervised child, however, is
//! spawned by the runtime supervisor from its child spec and never passes
//! through a codegen `spawn` site, so before this guard its
//! `(dispatch, msg_type) → name` rows were never seeded and the crash reporter
//! (`hew-runtime/src/signal.rs`) fell back to printing the raw `msg_type`
//! integer.
//!
//! The source below supervises `Worker` but NEVER directly `spawn`s it, so the
//! only path that can register `Worker::run` is the supervisor child-spec
//! emission. The IR assertions therefore pin that path specifically: drop the
//! registration and `Worker::run` disappears from the emitted module.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// A supervisor with one static child whose `run` handler panics on a poison
/// job. `main` spawns the SUPERVISOR (not the worker) and sends one message, so
/// `Worker` is reachable only as a supervised child.
const SUPERVISED_PANIC: &str = r#"
actor Worker {
    var processed: i64;
    receive fn run(job_id: i64) {
        if job_id == 13 {
            panic("poison job");
        }
        processed = processed + 1;
    }
}

supervisor App {
    strategy: one_for_one,
    child w: Worker(processed: 0)
}

fn main() {
    let sup = spawn App;
    let w = sup.w;
    w.run(1);
}
"#;

/// Drive parse → check → HIR → MIR → codegen and return the emitted textual
/// LLVM IR. Pinned to a SysV/AAPCS triple so the assertions are host-stable
/// (the registration calls are target-independent, but pinning keeps the IR
/// shape deterministic across CI hosts).
fn emit_supervised_ir() -> String {
    let parsed = hew_parser::parse(SUPERVISED_PANIC);
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
    // RAII temp dir — dropped at function exit (after `.ll` is slurped
    // into memory), and unique per call so the two sibling tests below
    // (which both call this fixture) never race on the same path under
    // parallel nextest execution. LESSONS: `cleanup-all-exits` P0.
    let tmp = tempfile::Builder::new()
        .prefix("hew-supervised-handler-name-reg-")
        .tempdir()
        .expect("create temp dir");
    let options = EmitOptions {
        module_name: "probe",
        out_dir: tmp.path(),
        native: false,
        wasm: false,
        target_triple: Some("x86_64-unknown-linux-gnu"),
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

/// The supervised child's per-handler name is registered: the emitted IR both
/// references `hew_register_handler_name` and embeds the `Worker::run` name
/// string. Because `Worker` is never directly `spawn`ed, this proves the
/// registration came from the supervisor child-spec path.
#[test]
fn supervised_child_registers_handler_name() {
    let ir = emit_supervised_ir();
    assert!(
        ir.contains("hew_register_handler_name"),
        "expected a `hew_register_handler_name` reference in the emitted IR;\ngot:\n{ir}"
    );
    assert!(
        ir.contains("Worker::run"),
        "expected the `Worker::run` handler-name string in the emitted IR \
         (registered via the supervisor child-spec path);\ngot:\n{ir}"
    );
    // A `call void @hew_register_handler_name(` must actually be emitted, not
    // merely the extern declaration.
    assert!(
        ir.contains("call void @hew_register_handler_name("),
        "expected an emitted CALL to hew_register_handler_name;\ngot:\n{ir}"
    );
}

/// The supervised child's TYPE name is registered too, so the crash reporter
/// can name the actor type alongside the handler.
#[test]
fn supervised_child_registers_actor_type_name() {
    let ir = emit_supervised_ir();
    assert!(
        ir.contains("call void @hew_actor_register_type("),
        "expected an emitted CALL to hew_actor_register_type for the supervised \
         child;\ngot:\n{ir}"
    );
}
