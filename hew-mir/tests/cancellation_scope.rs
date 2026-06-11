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
