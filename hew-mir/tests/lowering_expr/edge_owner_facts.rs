//! HIR-to-Raw-MIR coverage for replayed loop-owner joins.

use std::collections::{BTreeSet, HashMap};

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, OwnershipEvent};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let typed = checker.check_program(&parsed.program);
    assert!(typed.errors.is_empty(), "type errors: {:#?}", typed.errors);
    let hir = lower_program(
        &parsed.program,
        &typed,
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
fn loop_continue_replays_distinct_owner_inputs_into_one_join_generation() {
    let pipeline = pipeline(
        r#"
        fn collect_lines(limit: i64) -> string {
            var text = "";
            var i = 0;
            while i < limit {
                i = i + 1;
                if i == 2 {
                    text = text + "skip";
                    continue;
                }
                text = text + "keep";
            }
            text
        }
        "#,
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );

    let raw = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "collect_lines")
        .expect("collect_lines Raw MIR");
    let predecessors =
        raw.blocks
            .iter()
            .fold(HashMap::<u32, BTreeSet<u32>>::new(), |mut map, block| {
                for successor in block.successors() {
                    map.entry(successor).or_default().insert(block.id);
                }
                map
            });
    let joins = raw
        .blocks
        .iter()
        .flat_map(|block| {
            block.instructions.iter().filter_map(move |instruction| {
                let hew_mir::Instr::OwnershipEvent(OwnershipEvent::Join {
                    incoming,
                    replacement,
                    ..
                }) = instruction
                else {
                    return None;
                };
                Some((block.id, incoming, replacement))
            })
        })
        .collect::<Vec<_>>();
    assert!(!joins.is_empty(), "loop must materialize an ownership Join");
    for (target, incoming, replacement) in joins {
        let unique = incoming.iter().copied().collect::<BTreeSet<_>>();
        assert_eq!(
            unique.len(),
            incoming.len(),
            "Join at bb{target} must not duplicate an edge owner: {incoming:?}"
        );
        assert!(
            incoming.len() >= 2,
            "Join at bb{target} needs at least two distinct owner generations across the merging edges: {incoming:?}"
        );
        assert!(
            !incoming.contains(replacement),
            "Join at bb{target} must never list its successor as an input"
        );
        assert!(
            predecessors
                .get(&target)
                .is_some_and(|edges| edges.len() >= 2),
            "Join at bb{target} must correspond to a real CFG merge"
        );
    }
}

#[test]
fn branch_reassignments_join_before_terminal_aggregate_transfer() {
    let pipeline = pipeline(
        r#"
        type Plan { script: string; command: Vec<string>; }
        type Payload { program: string; args: Vec<string>; }

        fn render(plan: Plan) -> Payload {
            let args: Vec<string> = Vec.new();
            var program = "";
            if plan.script != "" {
                program = "sh";
                args.push(plan.script.clone());
            } else {
                program = plan.command[0].clone();
                for i in 1 .. plan.command.len() {
                    args.push(plan.command[i].clone());
                }
            }
            Payload { program: program, args: args }
        }

        fn direct(program: string) -> Payload {
            Payload { program: program, args: Vec.new() }
        }
        "#,
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );

    let raw = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "render")
        .expect("render Raw MIR");
    let joins = raw
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| {
            let hew_mir::Instr::OwnershipEvent(OwnershipEvent::Join {
                incoming,
                replacement,
                place,
                ..
            }) = instruction
            else {
                return None;
            };
            Some((incoming, replacement, place))
        })
        .collect::<Vec<_>>();
    let [(incoming, replacement, place)] = joins.as_slice() else {
        panic!("the two branch generations must produce one exact Join: {joins:?}");
    };
    assert_eq!(incoming.iter().copied().collect::<BTreeSet<_>>().len(), 2);
    assert!(incoming
        .iter()
        .all(|owner| owner.binding == replacement.binding));
    assert!(raw
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(|instruction| matches!(
            instruction,
            hew_mir::Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from,
                to_owner: None,
                ..
            }) if owner == *replacement && from == *place
        )));

    let direct = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "direct")
        .expect("direct Raw MIR");
    assert!(direct
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .all(|instruction| !matches!(
            instruction,
            hew_mir::Instr::OwnershipEvent(OwnershipEvent::Join { .. })
        )));
}
