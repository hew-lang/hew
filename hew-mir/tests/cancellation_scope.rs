use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr, IrPipeline, MirCheck};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Parse + check + HIR-lower + MIR-lower a source string, asserting the
/// parse/check/HIR layers are clean so failures pin to the MIR layer.
fn lower_clean_to_mir(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "checker errors: {:#?}",
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
    lower_hir_module(&hir.module)
}

/// All `MirCheck` entries across every function in the pipeline output.
fn all_checks(mir: &IrPipeline) -> Vec<&MirCheck> {
    mir.checked_mir
        .iter()
        .flat_map(|func| func.checks.iter())
        .collect()
}

const FORK_AWAIT_DRIVER: &str = r"
    fn ping() {
    }

    actor _Driver {
        receive fn drive() {
            scope {
                fork t = ping();
                await t;
            };
        }
    }

    fn main() -> i64 {
        let d = spawn _Driver;
        d.drive();
        0
    }
";

#[test]
fn fork_binding_awaited_emits_no_must_consume() {
    // `await t` consumes the linear Task<()> handle: the MustConsume exit
    // check must not fire for the awaited binding and the pipeline must be
    // diagnostic-free.
    let mir = lower_clean_to_mir(FORK_AWAIT_DRIVER);
    assert!(
        mir.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        mir.diagnostics
    );
    assert!(
        !all_checks(&mir)
            .iter()
            .any(|check| matches!(check, MirCheck::MustConsume { name, .. } if name == "t")),
        "awaited fork binding must not fire MustConsume; checks: {:#?}",
        all_checks(&mir)
    );
}

#[test]
fn fork_binding_double_await_fires_use_after_consume() {
    // The first `await t` consumes the handle; a second await is a
    // use-after-consume, not a silent double join.
    let source = FORK_AWAIT_DRIVER.replace("await t;", "await t;\n                await t;");
    let mir = lower_clean_to_mir(&source);
    assert!(
        all_checks(&mir)
            .iter()
            .any(|check| matches!(check, MirCheck::UseAfterConsume { name, .. } if name == "t")),
        "double await must fire UseAfterConsume; checks: {:#?}",
        all_checks(&mir)
    );
}

#[test]
fn fork_binding_unawaited_fires_must_consume() {
    // Named fork handles are linear (consume-once): binding a name and never
    // awaiting it is rejected. Fire-and-forget spawns use the unbound
    // `fork call();` form instead.
    let source = FORK_AWAIT_DRIVER.replace("await t;", "");
    let mir = lower_clean_to_mir(&source);
    assert!(
        all_checks(&mir)
            .iter()
            .any(|check| matches!(check, MirCheck::MustConsume { name, .. } if name == "t")),
        "unawaited named fork binding must fire MustConsume; checks: {:#?}",
        all_checks(&mir)
    );
}

// ── Arg-bearing fork callees (fork-entry shim env) ──────────────────────────

const FORK_ARGS_DRIVER: &str = r#"
    fn shout(msg: string) {
        println(f"{msg}!");
    }

    actor _Driver {
        receive fn drive() {
            let greeting = "hello" + " world";
            scope {
                fork { shout(greeting); }
            };
        }
    }

    fn main() -> i64 {
        let d = spawn _Driver;
        d.drive();
        0
    }
"#;

#[test]
fn fork_string_arg_spawns_via_fork_entry_shim() {
    // An arg-bearing fork callee dispatches through SpawnTaskClosure to a
    // synthesized fork-entry shim that loads the arg back out of the env
    // record and calls the target.
    let mir = lower_clean_to_mir(FORK_ARGS_DRIVER);
    assert!(
        mir.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        mir.diagnostics
    );
    let spawn_symbol = mir
        .raw_mir
        .iter()
        .flat_map(|func| &func.blocks)
        .flat_map(|block| block.instructions.iter())
        .find_map(|instr| match instr {
            Instr::SpawnTaskClosure { fn_symbol, .. } => Some(fn_symbol.clone()),
            _ => None,
        })
        .expect("arg-bearing fork must lower to SpawnTaskClosure");
    assert!(
        spawn_symbol.starts_with("__hew_fork_entry_"),
        "spawn must dispatch to a fork-entry shim, got `{spawn_symbol}`"
    );
    let shim = mir
        .raw_mir
        .iter()
        .find(|func| func.name == spawn_symbol)
        .expect("fork-entry shim function must be in raw_mir");
    let loads = shim
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| matches!(instr, Instr::ClosureEnvFieldLoad { .. }))
        .count();
    assert_eq!(loads, 1, "shim must load the one env arg field back out");
    assert!(
        shim.blocks.iter().any(|block| matches!(
            &block.terminator,
            hew_mir::Terminator::Call { callee, .. } if callee == "shout"
        )),
        "shim must call the user callee; blocks: {:#?}",
        shim.blocks
    );
}

#[test]
fn fork_string_arg_parent_and_shim_emit_no_drops_for_moved_arg() {
    // Drop-plan oracle (the truth standard): the moved-in string rides the
    // env bytes into the child. The parent's emitted drop plans carry NO
    // release for it (the consume fact removed it) and the shim's plans are
    // empty — byte-for-byte the posture of the direct-call baseline, which
    // also emits no drops for a by-value string arg under the move-only
    // M-COW spine. Leak-as-before; never a double free.
    let mir = lower_clean_to_mir(FORK_ARGS_DRIVER);
    for func in &mir.elaborated_mir {
        if func.name.contains("drive") || func.name.starts_with("__hew_fork_entry_") {
            for (exit, plan) in &func.drop_plans {
                assert!(
                    plan.drops.is_empty(),
                    "{}: expected empty drop plan at {exit:?}, got {:#?}",
                    func.name,
                    plan.drops
                );
            }
        }
    }
}

#[test]
fn fork_vec_arg_fails_closed() {
    // The arg-type restriction (BitCopy scalars + string) refuses richer
    // owned types with a diagnostic instead of miscompiling the transfer.
    let source = r"
        fn consume_vec(v: Vec<i64>) {
        }

        actor _Driver {
            receive fn drive() {
                let v: Vec<i64> = Vec::new();
                scope {
                    fork { consume_vec(v); }
                };
            }
        }

        fn main() -> i64 {
            let d = spawn _Driver;
            d.drive();
            0
        }
    ";
    let mir = lower_clean_to_mir(source);
    assert!(
        mir.diagnostics.iter().any(|d| d
            .note
            .contains("task spawn argument of type `Vec<i64>` is not yet supported")),
        "Vec arg must fail closed with the targeted note; diagnostics: {:#?}",
        mir.diagnostics
    );
}

#[test]
fn fork_nonunit_arg_bearing_callee_fails_closed() {
    // Value-bearing arg-spawns stay fail-closed until result propagation
    // lands; the spawn site refuses rather than discarding the result
    // silently behind a Task<i64> binding nothing can await yet.
    let source = r"
        fn compute(x: i64) -> i64 {
            x + 1
        }

        actor _Driver {
            receive fn drive() {
                scope {
                    fork t = compute(41);
                    await t;
                };
            }
        }

        fn main() -> i64 {
            let d = spawn _Driver;
            d.drive();
            0
        }
    ";
    let mir = lower_clean_to_mir(source);
    assert!(
        mir.diagnostics.iter().any(|d| d
            .note
            .contains("arg-bearing task spawn currently requires a unit-returning callee")),
        "non-unit arg-bearing callee must fail closed; diagnostics: {:#?}",
        mir.diagnostics
    );
}

#[test]
fn scope_fork_after_lowers_to_executable_task_and_deadline_abi() {
    let source = r"
        fn long_op() {
        }

        actor _Driver {
            receive fn drive() {
                scope {
                    fork { long_op() }
                    after(1ms) { }
                };
            }
        }

        fn main() -> i64 {
            let d = spawn _Driver;
            d.drive();
            0
        }
    ";
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

    let mir = lower_hir_module(&hir.module);
    assert!(
        mir.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        mir.diagnostics
    );

    let instructions: Vec<_> = mir
        .raw_mir
        .iter()
        .flat_map(|func| &func.blocks)
        .flat_map(|block| block.instructions.iter())
        .collect();
    assert!(
        instructions
            .iter()
            .any(|instr| matches!(instr, Instr::SpawnTaskDirect { callee_symbol, .. } if callee_symbol == "__hew_task_entry_long_op")),
        "scope fork must lower to SpawnTaskDirect; instructions: {instructions:#?}"
    );
    assert!(
        instructions.iter().any(|instr| matches!(
            instr,
            Instr::CallRuntimeAbi(call)
                if call.symbol() == "hew_task_scope_cancel_after_ns"
        )),
        "after(duration) must lower to deadline cancellation ABI; instructions: {instructions:#?}"
    );
}
