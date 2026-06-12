use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

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
