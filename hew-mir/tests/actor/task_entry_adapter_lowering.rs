use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module, FunctionCallConv, Instr, IrPipeline, MirDiagnosticKind, Terminator,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn lower_source(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
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
    lower_hir_module(&hir.module)
}

#[test]
fn free_fn_spawn_uses_task_entry_adapter_shape() {
    let pipeline = lower_source(
        r"
        fn worker() {
        }

        actor _Driver {
            receive fn drive() {
                scope {
                    fork { worker(); }
                    fork { worker(); }
                };
            }
        }

        fn main() -> i64 {
            let d = spawn _Driver;
            d.drive();
            0
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );

    let worker = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "worker")
        .expect("user worker function");
    assert_eq!(worker.call_conv, FunctionCallConv::Default);
    assert!(
        !worker
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(instr, Instr::EnterContext | Instr::ExitContext)),
        "user function ABI must remain unchanged: {worker:#?}"
    );

    let adapters: Vec<_> = pipeline
        .raw_mir
        .iter()
        .filter(|func| func.name == "__hew_task_entry_worker")
        .collect();
    assert_eq!(adapters.len(), 1, "adapter must be de-duplicated");
    let adapter = adapters[0];
    assert_eq!(adapter.call_conv, FunctionCallConv::TaskEntry);
    assert!(FunctionCallConv::TaskEntry.carries_execution_context());
    assert_ne!(FunctionCallConv::TaskEntry, FunctionCallConv::ActorHandler);
    assert!(adapter.params.is_empty());
    assert!(adapter.locals.is_empty());
    assert_eq!(adapter.blocks.len(), 2);
    assert!(matches!(
        adapter.blocks[0].instructions.as_slice(),
        [Instr::EnterContext]
    ));
    assert!(matches!(
        &adapter.blocks[0].terminator,
        Terminator::Call {
            callee,
            args,
            dest: None,
            next: 1,
            ..
        } if callee == "worker" && args.is_empty()
    ));
    assert!(matches!(
        adapter.blocks[1].instructions.as_slice(),
        [Instr::ExitContext]
    ));
    assert!(matches!(adapter.blocks[1].terminator, Terminator::Return));

    let spawn_callees: Vec<_> = pipeline
        .raw_mir
        .iter()
        .flat_map(|func| &func.blocks)
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| {
            if let Instr::SpawnTaskDirect { callee_symbol, .. } = instr {
                Some(callee_symbol.as_str())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        spawn_callees,
        vec!["__hew_task_entry_worker", "__hew_task_entry_worker"]
    );
}

#[test]
fn default_callconv_spawn_rejects_before_spawn_task_direct() {
    let pipeline = lower_source(
        r"
        fn worker() {
        }

        fn main() {
            scope {
                fork { worker(); }
            };
        }
        ",
    );

    assert!(
        pipeline.diagnostics.iter().any(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct.contains("cannot spawn `worker` from `main`")
            ) && diag.note.contains("ctx-bearing execution context")
                && diag.note.contains("Default call-conv")
                && diag.note.contains("W4.010-followup-caller-ctx-routing")
        }),
        "expected Default-callconv caller-context diagnostic: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        !pipeline
            .raw_mir
            .iter()
            .flat_map(|func| &func.blocks)
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(instr, Instr::SpawnTaskDirect { .. })),
        "Default-callconv rejected spawn must not emit SpawnTaskDirect"
    );
}

#[test]
fn generic_free_fn_spawn_uses_monomorphized_task_entry_adapters() {
    // After this change: a generic callee spawned with a concrete type arg
    // resolves to the monomorphized symbol (worker$$i64) and gets its own
    // task-entry adapter (__hew_task_entry_worker__i64). No NYI diagnostic.
    let pipeline = lower_source(
        r"
        fn worker<T>() {
        }

        actor _Driver {
            receive fn drive() {
                scope {
                    fork { worker::<i64>(); }
                    fork { worker::<string>(); }
                };
            }
        }

        fn main() -> i64 {
            let d = spawn _Driver;
            d.drive();
            0
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "generic spawn must not emit any diagnostic: {:#?}",
        pipeline.diagnostics
    );
    // Each concrete type arg gets its own task-entry adapter.
    assert!(
        pipeline
            .raw_mir
            .iter()
            .any(|func| func.name == "__hew_task_entry_worker__i64"),
        "expected __hew_task_entry_worker__i64 adapter in MIR"
    );
    assert!(
        pipeline
            .raw_mir
            .iter()
            .any(|func| func.name == "__hew_task_entry_worker__string"),
        "expected __hew_task_entry_worker__string adapter in MIR"
    );
    // Each adapter must use the TaskEntry call-convention.
    for func in &pipeline.raw_mir {
        if func.name.starts_with("__hew_task_entry_worker__") {
            assert_eq!(
                func.call_conv,
                FunctionCallConv::TaskEntry,
                "adapter {} must use TaskEntry call-conv",
                func.name
            );
        }
    }
}
