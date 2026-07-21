//! `--explain-cow` renderer for MIR-authored actor outbound modes.

use hew_mir::{IrPipeline, SendAliasMode, SuspendKind, Terminator};

fn mode_label(mode: SendAliasMode) -> &'static str {
    match mode {
        SendAliasMode::SnapshotBitCopy => "SNAPSHOT_BIT_COPY",
        SendAliasMode::SnapshotRetain => "SNAPSHOT_RETAIN",
        SendAliasMode::SnapshotMaterialize => "SNAPSHOT_MATERIALIZE",
        SendAliasMode::TransferLastUse => "TRANSFER_LAST_USE",
    }
}

/// Render resolved send/ask modes in deterministic function/block order.
pub fn render_explain_cow(pipeline: &IrPipeline, filename: &str, out: &mut dyn std::io::Write) {
    for function in &pipeline.raw_mir {
        for block in &function.blocks {
            let modes = match &block.terminator {
                Terminator::Send { arg_modes, .. } | Terminator::Ask { arg_modes, .. } => {
                    Some(arg_modes)
                }
                Terminator::Suspend { .. } => {
                    function.suspend_kinds.get(&block.id).and_then(|kind| {
                        if let SuspendKind::Ask { arg_modes, .. } = kind {
                            Some(arg_modes)
                        } else {
                            None
                        }
                    })
                }
                _ => None,
            };
            let Some(modes) = modes else {
                continue;
            };
            let labels = modes
                .iter()
                .copied()
                .map(mode_label)
                .collect::<Vec<_>>()
                .join(",");
            let _ = writeln!(
                out,
                "{filename}:{}:bb{}: send - {labels}",
                function.name, block.id
            );
        }
    }
}
