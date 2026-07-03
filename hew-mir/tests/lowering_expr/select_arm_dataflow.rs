//! Init-check dataflow coverage for `Terminator::Select` arm bodies.
//!
//! A select arm's `body_block` is a real runtime successor of the
//! originating block (codegen jumps there when the arm wins), and the
//! arm's binding is written by the runtime dispatch before the body
//! runs. The init-check CFG must reflect that, or any aggregate-typed
//! arm binding whose uses span Call-terminated blocks inside the arm
//! body trips a false `InitialisedBeforeUse`.
//!
//! With the arm-body edges wired into `build_preds`/`successors`, the
//! originating block's exit state reaches every arm body and propagates
//! across the blocks inside it, so aggregate arm bindings check clean.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, MirCheck, MirDiagnosticKind};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Pipe source through parser → checker → HIR → MIR. Asserts no parser,
/// checker, or HIR diagnostics; per-test assertions inspect the MIR
/// checks.
fn lower_source(src: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    lower_hir_module(&output.module)
}

/// An actor-ask select arm with a record-typed reply binding whose field
/// reads span two Call-terminated blocks inside the arm body. The second
/// read sits in a block whose only predecessor is another arm-body block.
const ASK_RECORD_CROSS_BLOCK: &str = r#"
record Pair {
    a: string,
    b: string
}

actor Svc {
    receive fn get() -> Pair {
        Pair { a: "x", b: "y" }
    }
}

fn main() {
    let svc = spawn Svc;
    select {
        r from svc.get() => {
            println(r.a);
            println(r.b);
        },
        after 1s => println("timeout"),
    };
}
"#;

/// An aggregate (record) arm binding whose field reads span blocks inside
/// the arm body must check clean: the binding is written by the dispatch
/// before the body runs, and arm-body state propagates block to block.
/// Was the stage-0 false-positive pin
/// (`select_arm_aggregate_binding_cross_block_use_currently_flags_init_check`)
/// before the arm-body CFG edges landed.
#[test]
fn select_arm_aggregate_binding_initialised_passes() {
    let pipeline = lower_source(ASK_RECORD_CROSS_BLOCK);
    let init_checks: Vec<_> = pipeline
        .checked_mir
        .iter()
        .flat_map(|f| f.checks.iter())
        .filter(|check| matches!(check, MirCheck::InitialisedBeforeUse { .. }))
        .collect();
    assert!(
        init_checks.is_empty(),
        "cross-block use of a select arm binding must not flag \
         InitialisedBeforeUse: {init_checks:?}"
    );
    assert!(
        !pipeline
            .diagnostics
            .iter()
            .any(|d| matches!(&d.kind, MirDiagnosticKind::InitialisedBeforeUse { .. })),
        "no init-check diagnostic may reach the CLI rejection channel: {:?}",
        pipeline.diagnostics
    );
}

/// Control: the same binding consumed entirely within the first arm-body
/// block (single use, no cross-block span) must NOT flag — pins that the
/// false positive is specifically the cross-block propagation gap.
#[test]
fn select_arm_aggregate_binding_single_block_use_passes() {
    let pipeline = lower_source(
        r#"
record Pair {
    a: string,
    b: string
}

actor Svc {
    receive fn get() -> Pair {
        Pair { a: "x", b: "y" }
    }
}

fn main() {
    let svc = spawn Svc;
    select {
        r from svc.get() => println(r.a),
        after 1s => println("timeout"),
    };
}
"#,
    );
    let init_checks: Vec<_> = pipeline
        .checked_mir
        .iter()
        .flat_map(|f| f.checks.iter())
        .filter(|check| matches!(check, MirCheck::InitialisedBeforeUse { .. }))
        .collect();
    assert!(
        init_checks.is_empty(),
        "single-block arm-binding use must stay clean: {init_checks:?}"
    );
}
