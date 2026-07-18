//! Back-edge-only ownership for a reassigned `BindingRef` while-let scrutinee.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module, DropKind, ExitPath, Instr, IrPipeline, MirDiagnosticKind, MirStatement, Place,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

const ITERATION_OWNER_NAME: &str = "__hew_while_let_iteration";

fn pipeline(source: &str) -> IrPipeline {
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
        "type errors: {:#?}",
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
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    lower_hir_module(&output.module)
}

const REASSIGNED_WITH_BREAK: &str = r#"
enum Packet {
    Payload(string);
    Done;
}

fn next(i: i64, cap: i64) -> Packet {
    if i < cap {
        Packet::Payload("while-let-backedge-payload-abcdefghijklmnopqrstuvwxyz".to_upper())
    } else {
        Packet::Done
    }
}

fn probe(cap: i64) -> i64 {
    var i = 0;
    var q = next(i, cap);
    var total = 0;
    while let Packet::Payload(s) = q {
        if i == 2 {
            break;
        }
        total = total + s.len();
        i = i + 1;
        q = next(i, cap);
    }
    total
}
"#;

fn snapshot_and_source_locals(pipeline: &IrPipeline) -> (u32, u32, u32) {
    let raw = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "probe")
        .expect("probe raw MIR");
    let header = raw
        .blocks
        .iter()
        .find(|block| {
            block.statements.iter().any(|statement| {
                matches!(
                    statement,
                    MirStatement::Bind { name, .. } if name == ITERATION_OWNER_NAME
                )
            })
        })
        .expect("while-let header with iteration owner");
    let snapshot_local = header
        .instructions
        .iter()
        .find_map(|instruction| match instruction {
            Instr::Move {
                src: Place::EnumTag(local),
                ..
            } => Some(*local),
            _ => None,
        })
        .expect("tag read from snapshot local");
    let source_local = header
        .instructions
        .iter()
        .find_map(|instruction| match instruction {
            Instr::Move {
                dest: Place::Local(dest),
                src: Place::Local(src),
            } if *dest == snapshot_local => Some(*src),
            _ => None,
        })
        .expect("source binding copied into snapshot");
    (header.id, snapshot_local, source_local)
}

#[test]
fn reassigned_binding_snapshot_drops_once_on_back_edge_only() {
    let pipeline = pipeline(REASSIGNED_WITH_BREAK);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let (header_id, snapshot_local, source_local) = snapshot_and_source_locals(&pipeline);
    let function = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "probe")
        .expect("probe elaborated MIR");

    let mut back_edge_snapshot_drops = 0;
    let mut other_snapshot_drops = 0;
    let mut return_source_drops = 0;
    for (exit, plan) in &function.drop_plans {
        for drop in &plan.drops {
            if drop.place == Place::Local(snapshot_local)
                && matches!(drop.kind, DropKind::EnumInPlace)
            {
                if matches!(exit, ExitPath::Goto { target, .. } if *target == header_id) {
                    back_edge_snapshot_drops += 1;
                } else {
                    other_snapshot_drops += 1;
                }
            }
            if drop.place == Place::Local(source_local)
                && matches!(drop.kind, DropKind::EnumInPlace)
                && matches!(exit, ExitPath::Return { .. })
            {
                return_source_drops += 1;
            }
        }
    }

    assert_eq!(
        back_edge_snapshot_drops, 1,
        "the discarded iteration composite must have one back-edge release"
    );
    assert_eq!(
        other_snapshot_drops, 0,
        "break, tag-false, and function-exit plans must not release the aliasing snapshot"
    );
    assert_eq!(
        return_source_drops, 1,
        "the binding's final value must retain exactly one scope-exit release"
    );
}

fn reject_count(pipeline: &IrPipeline, construct: &str) -> usize {
    pipeline
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                &diagnostic.kind,
                MirDiagnosticKind::NotYetImplemented {
                    construct: actual,
                    ..
                } if actual == construct
            )
        })
        .count()
}

#[test]
fn conditional_reassignment_fails_closed() {
    let source = REASSIGNED_WITH_BREAK.replace(
        "        q = next(i, cap);",
        "        if i < cap {\n            q = next(i, cap);\n        }",
    );
    let pipeline = pipeline(&source);
    assert_eq!(
        reject_count(
            &pipeline,
            "while-let scrutinee conditionally reassigned in loop body"
        ),
        1,
        "diagnostics: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn non_fresh_reassignment_fails_closed() {
    let source = REASSIGNED_WITH_BREAK.replace(
        "var total = 0;",
        "var other = next(0, cap);\n    var total = 0;",
    );
    let source = source.replace("        q = next(i, cap);", "        q = other;");
    let pipeline = pipeline(&source);
    assert_eq!(
        reject_count(
            &pipeline,
            "while-let scrutinee reassigned from non-fresh value"
        ),
        1,
        "diagnostics: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn continue_with_reassignment_fails_closed() {
    let source = REASSIGNED_WITH_BREAK.replace(
        "        q = next(i, cap);",
        "        q = next(i, cap);\n        continue;",
    );
    let pipeline = pipeline(&source);
    assert_eq!(
        reject_count(&pipeline, "while-let reassigned scrutinee with continue"),
        1,
        "diagnostics: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn post_reassignment_break_fails_closed() {
    let source = REASSIGNED_WITH_BREAK
        .replace("        if i == 2 {\n            break;\n        }\n", "")
        .replace(
            "        q = next(i, cap);",
            "        q = next(i, cap);\n        break;",
        );
    let pipeline = pipeline(&source);
    assert_eq!(
        reject_count(
            &pipeline,
            "while-let reassigned scrutinee with post-reassignment exit"
        ),
        1,
        "diagnostics: {:#?}",
        pipeline.diagnostics
    );
}
