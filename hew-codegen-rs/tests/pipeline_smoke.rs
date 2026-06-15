use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::MirDiagnosticKind;
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

fn emit_ll_for_source(src: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(src);
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
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "hir diagnostics: {:?}",
        output.diagnostics
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-pipeline-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("tmp dir");
    let options = EmitOptions {
        module_name,
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
    std::fs::read_to_string(ll_path).expect("read ll")
}

#[test]
fn pipeline_rejects_nested_named_type_before_codegen() {
    let parsed = hew_parser::parse("fn f(x: (Foo, i64)) -> (Foo, i64) { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Foo")
        }),
        "nested named type must be rejected before codegen: {:?}",
        pipeline.diagnostics
    );
}

/// Bool literals lower to `Instr::ConstI64 { value: 0/1 }` once the CFG
/// construction lane lands its bool + cmp prerequisite (Slice 0). The
/// stored width is whatever HIR resolved (`ResolvedTy::Bool`, mapped to
/// i8 by `primitive_to_llvm`); `ConstI64`'s store truncates the value
/// to the dest local's width. The MIR pipeline must accept `fn main()
/// -> bool { true }` cleanly — no `NotYetImplemented` for bool
/// literals, no `UnresolvedPlace`, nothing else.
#[test]
fn pipeline_accepts_bool_literal_return() {
    let parsed = hew_parser::parse("fn main() -> bool { true }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir gate must accept this: hir={:?} verify={:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "bool literal must lower cleanly without diagnostics: {:?}",
        pipeline.diagnostics
    );
}

/// Float literals now lower to `Instr::FloatLit` in the MIR. The MIR
/// diagnostic stream must be empty — no `NotYetImplemented` — for a
/// simple `fn main() -> f64 { 1.5 }` program.
#[test]
fn pipeline_accepts_float_literal_in_mir() {
    let parsed = hew_parser::parse("fn main() -> f64 { 1.5 }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir must accept float literal: hir={:?} verify={:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "float literal must lower without MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
}

/// A direct call to a module function now lowers cleanly via
/// `Terminator::Call`. `main` returns `add(10, 32)`; the MIR pipeline must
/// accept the program without `NotYetImplemented` or `UnresolvedPlace`
/// diagnostics, and codegen must emit valid LLVM IR.
#[test]
fn pipeline_accepts_user_fn_call_via_call_terminator() {
    let parsed = hew_parser::parse(
        "fn add(x: i64, y: i64) -> i64 { x + y }\n\
         fn main() -> i64 { add(10, 32) }\n",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir must accept user-fn call: hir={:?} verify={:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "user-fn call pipeline must have no MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
}

/// Verify that an actor with `#[on(stop)]` emits the terminate trampoline
/// and the `hew_actor_set_terminate` registration call in the LLVM IR.
#[test]
fn actor_on_stop_emits_terminate_trampoline_and_registration() {
    let src = r#"
        actor Counter {
            var count: i64;
            receive fn noop() { }
            #[on(stop)]
            fn shutdown() { count = 0; }
        }
        fn main() -> i64 {
            let c = spawn Counter(count: 0);
            0
        }
    "#;
    let ir = emit_ll_for_source(src, "actor_on_stop_ir");
    // Terminate trampoline function must be defined.
    assert!(
        ir.contains("__terminate_Counter"),
        "LLVM IR must define the terminate trampoline;\ngot:\n{ir}"
    );
    // The on(stop) handler must be defined with ActorHandler ABI (indexed symbol).
    assert!(
        ir.contains("Counter__on_stop__0"),
        "LLVM IR must define Counter__on_stop__0;\ngot:\n{ir}"
    );
    // hew_actor_set_terminate must be called at spawn time.
    assert!(
        ir.contains("hew_actor_set_terminate"),
        "LLVM IR must call hew_actor_set_terminate;\ngot:\n{ir}"
    );
    // The lock acquire call must appear inside the terminate trampoline.
    assert!(
        ir.contains("hew_actor_state_lock_acquire"),
        "LLVM IR must call hew_actor_state_lock_acquire inside the trampoline;\ngot:\n{ir}"
    );
    // hew_require_execution_context must be called inside the trampoline.
    assert!(
        ir.contains("hew_require_execution_context"),
        "LLVM IR must call hew_require_execution_context inside the trampoline;\ngot:\n{ir}"
    );
}

#[test]
fn actor_multiple_on_stop_emits_fan_out_trampoline_calling_both_hooks() {
    let src = r#"
        actor Sequencer {
            var counter: i64;
            receive fn value() -> i64 { counter }
            #[on(stop)]
            fn cleanup_a() { counter = 1; }
            #[on(stop)]
            fn cleanup_b() { counter = counter + 1; }
        }
        fn main() -> i64 {
            let s = spawn Sequencer(counter: 0);
            match await s.value() {
                Ok(v) => v,
                Err(_e) => 0,
            }
        }
    "#;
    let ir = emit_ll_for_source(src, "actor_multi_on_stop_ir");
    // Both indexed per-hook symbols must appear in the IR.
    assert!(
        ir.contains("Sequencer__on_stop__0"),
        "LLVM IR must define Sequencer__on_stop__0;\ngot:\n{ir}"
    );
    assert!(
        ir.contains("Sequencer__on_stop__1"),
        "LLVM IR must define Sequencer__on_stop__1;\ngot:\n{ir}"
    );
    // A single terminate trampoline must be registered (fan-out wrapper).
    assert!(
        ir.contains("__terminate_Sequencer"),
        "LLVM IR must define the terminate trampoline;\ngot:\n{ir}"
    );
    // hew_actor_set_terminate called once to register the fan-out wrapper.
    assert!(
        ir.contains("hew_actor_set_terminate"),
        "LLVM IR must call hew_actor_set_terminate;\ngot:\n{ir}"
    );
}
