//! Render the verified value operations feeding actor mailbox boundaries.

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
        for parameter in &function.params {
            types.insert(parameter.value, parameter.ty.clone());
        }
        for block in &function.blocks {
            for argument in &block.args {
                types.insert(argument.value, argument.ty.clone());
            }
            for operation in &block.ops {
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
                let preparation = preparation(module, ty, &producers, value);
                entries.insert((
                    offset,
                    index,
                    format!("{}: {action}{preparation}", ty.user_facing()),
                ));
            }
        }
    }
    for (offset, index, description) in entries {
        let prefix = &source[..offset as usize];
        let line = prefix.bytes().filter(|byte| *byte == b'\n').count() + 1;
        let column = prefix.rsplit('\n').next().unwrap_or("").chars().count() + 1;
        println!(
            "{path}:{line}:{column}: send - payload {} {description}",
            index + 1
        );
    }
}

fn preparation(
    module: &SemModule,
    ty: &ResolvedTy,
    producers: &BTreeMap<ValueId, &SemOpKind>,
    mut value: ValueId,
) -> &'static str {
    let clone = module.type_facts[&TypeInstanceKey(ty.clone())].clone;
    loop {
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
}
