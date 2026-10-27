//! Render verified ownership at mailbox and codec boundaries.

use std::collections::{BTreeMap, BTreeSet};

use hew_sir::{
    ActorOperation, BoundaryDecision, SemModule, SemOpKind, SemTerminator, SuspendKind, ValueId,
};
use hew_types::{CloneKind, ResolvedTy, TypeInstanceKey};

pub fn print(module: &SemModule, path: &str, source: &str) {
    let mut entries = BTreeSet::new();
    for function in &module.functions {
        let mut types = BTreeMap::new();
        let mut producers = BTreeMap::new();
        let forwarded = forwarded_values(function);
        for parameter in &function.params {
            types.insert(parameter.value, parameter.ty.clone());
        }
        for block in &function.blocks {
            if let SemTerminator::WireCodec { codec, plan, .. } = &block.terminator {
                if let Some(offset) = module.debug.site_offset(&block.terminator_provenance) {
                    let direction = if codec.is_serialize() {
                        "encode"
                    } else {
                        "decode"
                    };
                    entries.insert((
                        offset,
                        format!(
                            "codec - {direction} {:?} {}: borrow input ({} planned types)",
                            codec.format,
                            plan.root.user_facing(),
                            plan.plans.len(),
                        ),
                    ));
                    for planned in plan.plans.values() {
                        let table = match &planned.kind {
                            hew_sir::SemWireKind::Record {
                                table: Some(table), ..
                            }
                            | hew_sir::SemWireKind::Enum { table, .. } => table,
                            _ => continue,
                        };
                        for member in &table.members {
                            entries.insert((
                                offset,
                                format!(
                                    "codec plan - {}: key {:?}, tag {}, flags {}",
                                    planned.ty.user_facing(),
                                    member.key,
                                    member.tag,
                                    member.flags,
                                ),
                            ));
                        }
                    }
                }
            }
            for argument in &block.args {
                types.insert(argument.value, argument.ty.clone());
            }
            for operation in block.operations() {
                for result in &operation.results {
                    types.insert(result.id, result.ty.clone());
                    producers.insert(result.id, &operation.kind);
                }
            }
            block.terminator.visit_results(|result| {
                types.insert(result.id, result.ty.clone());
            });
        }
        for block in &function.blocks {
            let inputs = match &block.terminator {
                SemTerminator::Suspend {
                    kind: SuspendKind::Ask { .. },
                    inputs,
                    ..
                } => &inputs[1..],
                SemTerminator::ActorCall {
                    operation: ActorOperation::CallStart(_) | ActorOperation::StreamStart { .. },
                    args,
                    ..
                } => &args[1..],
                SemTerminator::ActorCall {
                    operation: ActorOperation::Submit { .. },
                    args,
                    ..
                } => args.as_slice(),
                _ => continue,
            };
            let Some(offset) = module.debug.site_offset(&block.terminator_provenance) else {
                continue;
            };
            // Report actual boundary payloads rather than reconstructing source
            // arguments: a submission carries a sealed request, while a stream
            // producer receives a tuple that also owns its sink.
            for (index, input) in inputs.iter().enumerate() {
                let value = input.operand.value;
                let ty = &types[&value];
                let action = match input.decision {
                    BoundaryDecision::Borrow => "borrow",
                    BoundaryDecision::BorrowMut => "borrow exclusively",
                    BoundaryDecision::Copy => "copy",
                    BoundaryDecision::Move => "move",
                    BoundaryDecision::Snapshot(hew_sir::SnapshotDecision::Share) => "share",
                    BoundaryDecision::Snapshot(hew_sir::SnapshotDecision::DeepCopy) => "deep copy",
                    BoundaryDecision::Snapshot(hew_sir::SnapshotDecision::Transfer) => "transfer",
                };
                let preparation = preparation(module, ty, &producers, &forwarded, value);
                entries.insert((
                    offset,
                    format!(
                        "send - payload {} {}: {action}{preparation}",
                        index + 1,
                        ty.user_facing()
                    ),
                ));
            }
        }
    }
    for (offset, description) in entries {
        let prefix = &source[..offset as usize];
        let line = prefix.bytes().filter(|byte| *byte == b'\n').count() + 1;
        let column = prefix.rsplit('\n').next().unwrap_or("").chars().count() + 1;
        println!("{path}:{line}:{column}: {description}");
    }
}

fn forwarded_values(function: &hew_sir::SemFunction) -> BTreeMap<ValueId, Option<ValueId>> {
    let mut forwarded = BTreeMap::new();
    let block_args = function
        .blocks
        .iter()
        .map(|block| (block.id, &block.args))
        .collect::<BTreeMap<_, _>>();
    for block in &function.blocks {
        block.terminator.visit_successors(|edge| {
            for (argument, incoming) in block_args[&edge.target].iter().zip(&edge.args) {
                forwarded
                    .entry(argument.value)
                    .and_modify(|previous| {
                        if *previous != Some(incoming.value) {
                            *previous = None;
                        }
                    })
                    .or_insert(Some(incoming.value));
            }
        });
    }
    forwarded
}

fn preparation(
    module: &SemModule,
    ty: &ResolvedTy,
    producers: &BTreeMap<ValueId, &SemOpKind>,
    forwarded: &BTreeMap<ValueId, Option<ValueId>>,
    mut value: ValueId,
) -> &'static str {
    let clone = module.type_facts[&TypeInstanceKey(ty.clone())].clone;
    let mut visited = BTreeSet::new();
    while visited.insert(value) {
        if let Some(Some(incoming)) = forwarded.get(&value) {
            value = *incoming;
            continue;
        }
        match producers.get(&value) {
            Some(SemOpKind::Move { source }) => value = source.value,
            Some(
                SemOpKind::LoadCopy { .. }
                | SemOpKind::CopyValue { .. }
                | SemOpKind::AggregateProjectCopy { .. }
                | SemOpKind::VariantProjectCopy { .. },
            ) => {
                return match clone {
                    CloneKind::Bits => " (bit copy)",
                    CloneKind::Retain => " (retain)",
                    CloneKind::DeepCopy => " (deep copy)",
                    CloneKind::FieldWise => " (field-wise copy)",
                    CloneKind::None => {
                        unreachable!("verified SIR does not copy non-copyable values")
                    }
                };
            }
            Some(SemOpKind::LoadTake { .. }) => return " (take from storage)",
            Some(SemOpKind::Fork { .. }) => return " (copy on write)",
            _ if clone == CloneKind::Bits => return " (bit copy)",
            _ => return "",
        }
    }
    ""
}
