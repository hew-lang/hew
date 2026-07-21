use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module, validate_outbound_actor_modes, Instr, MirCheck, SendAliasMode, Terminator,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn lower(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "{:?}", tco.errors);
    let hir = lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let pipeline = lower_hir_module(&hir.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:?}",
        pipeline.diagnostics
    );
    pipeline
}

fn send_modes(pipeline: &hew_mir::IrPipeline) -> Vec<Vec<SendAliasMode>> {
    pipeline
        .checked_mir
        .iter()
        .flat_map(|function| &function.blocks)
        .filter_map(|block| match &block.terminator {
            Terminator::Send { arg_modes, .. } => Some(arg_modes.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn snapshot_send_resolves_every_argument_independently() {
    let pipeline = lower(
        r#"
        type Boxed {
            payload: Vec<i64>,
        }

        actor Sink {
            receive fn take(n: i64, text: string, boxed: Boxed, last: string) {}
        }

        fn main() {
            let sink = spawn Sink;
            let n = 1;
            let text = "shared";
            let boxed = Boxed { payload: [2, 3] };
            let last = "last";
            sink.take(n, text, boxed, last);
            println(text);
            boxed.payload.push(4);
        }
        "#,
    );
    assert!(
        send_modes(&pipeline).iter().any(|modes| {
            modes
                == &[
                    SendAliasMode::SnapshotBitCopy,
                    SendAliasMode::SnapshotRetain,
                    SendAliasMode::SnapshotMaterialize,
                    SendAliasMode::TransferLastUse,
                ]
        }),
        "{:#?}",
        send_modes(&pipeline)
    );
    assert!(
        pipeline
            .raw_mir
            .iter()
            .flat_map(|function| &function.blocks)
            .any(|block| block
                .instructions
                .iter()
                .any(|instr| matches!(instr, Instr::NeutralizePayloadSlot { .. }))),
        "last-use transfer must neutralize the sender slot"
    );
}

#[test]
fn loop_back_edge_and_projection_force_snapshot() {
    let pipeline = lower(
        r"
        type Boxed {
            payload: Vec<i64>,
        }

        actor Sink {
            receive fn take(value: Vec<i64>) {}
        }

        fn main() {
            let sink = spawn Sink;
            let boxed = Boxed { payload: [1] };
            var i = 0;
            while i < 3 {
                sink.take(boxed.payload);
                i = i + 1;
            }
            boxed.payload.push(9);
        }
        ",
    );
    let modes = send_modes(&pipeline);
    assert!(
        modes
            .iter()
            .any(|modes| modes == &[SendAliasMode::SnapshotMaterialize]),
        "{modes:#?}"
    );
    assert!(
        pipeline
            .raw_mir
            .iter()
            .flat_map(|function| &function.blocks)
            .any(|block| block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::ValueSnapshotClone { dest, src, .. } if dest != src
                )
            })),
        "projection snapshots must clone into a fresh destination"
    );
}

#[test]
fn checked_mir_rejects_unresolved_outbound_modes() {
    let pipeline = lower(
        r#"
        actor Sink {
            receive fn take(value: string) {}
        }

        fn main() {
            let sink = spawn Sink;
            sink.take("payload");
        }
        "#,
    );
    let mut raw = pipeline
        .raw_mir
        .into_iter()
        .find(|function| {
            function
                .blocks
                .iter()
                .any(|block| matches!(block.terminator, Terminator::Send { .. }))
        })
        .expect("fixture must lower one send");
    for block in &mut raw.blocks {
        if let Terminator::Send { arg_modes, .. } = &mut block.terminator {
            arg_modes.clear();
        }
    }
    assert!(validate_outbound_actor_modes(&raw)
        .iter()
        .any(|check| matches!(check, MirCheck::OutboundModeUnresolved { .. })));
}
