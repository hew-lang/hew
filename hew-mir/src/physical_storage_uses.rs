use super::{ArgumentTransfer, BTreeSet, PhysicalOp, PhysicalTerminator, StorageId};

#[expect(
    clippy::too_many_lines,
    reason = "one exhaustive physical storage reference contract"
)]
pub(super) fn operation_storage(
    operation: &PhysicalOp,
    used: &mut BTreeSet<StorageId>,
    defined: &mut BTreeSet<StorageId>,
    locals: &mut BTreeSet<StorageId>,
) {
    match operation {
        PhysicalOp::TaskScopeEnter { duration, .. } => used.extend(duration),
        PhysicalOp::TaskScopeClose { .. } => {}
        PhysicalOp::StreamPipe { stream, sink, .. } => {
            defined.extend([*stream, *sink]);
        }
        PhysicalOp::GeneratorMake { dest, callable, .. }
        | PhysicalOp::TaskSpawn { dest, callable, .. } => {
            defined.insert(*dest);
            used.insert(*callable);
        }
        PhysicalOp::RegisterDefer { dependencies, .. } => used.extend(dependencies),
        PhysicalOp::StorageLive { storage } => {
            locals.insert(*storage);
        }
        PhysicalOp::StorageDead { storage, .. } => {
            used.insert(*storage);
        }
        PhysicalOp::FunctionMake { dest, .. } | PhysicalOp::Const { dest, .. } => {
            defined.insert(*dest);
        }
        PhysicalOp::Unary { dest, source, .. }
        | PhysicalOp::Cast { dest, source, .. }
        | PhysicalOp::CallableCoerce { dest, source }
        | PhysicalOp::GeneratorCoerce { dest, source }
        | PhysicalOp::DynMake { dest, source, .. }
        | PhysicalOp::Transfer { dest, source }
        | PhysicalOp::Clone { dest, source, .. }
        | PhysicalOp::Borrow { dest, source } => {
            defined.insert(*dest);
            used.insert(*source);
            used.insert(*dest);
        }
        PhysicalOp::Binary { dest, lhs, rhs, .. } => {
            defined.insert(*dest);
            used.extend([*lhs, *rhs]);
        }
        PhysicalOp::TupleGet { dest, tuple, .. } => {
            defined.insert(*dest);
            used.insert(*tuple);
        }
        PhysicalOp::TaskRace {
            dest,
            members: fields,
            ..
        }
        | PhysicalOp::TupleMake {
            dest,
            elements: fields,
        }
        | PhysicalOp::AggregateMake { dest, fields, .. }
        | PhysicalOp::ArrayMake { dest, fields, .. }
        | PhysicalOp::VariantMake { dest, fields, .. }
        | PhysicalOp::ClosureMake { dest, fields, .. } => {
            defined.insert(*dest);
            used.extend(fields);
        }
        PhysicalOp::ArrayRepeat { dest, seed, .. } => {
            defined.insert(*dest);
            used.insert(*seed);
        }
        PhysicalOp::AggregateProjectCopy {
            dest, aggregate, ..
        }
        | PhysicalOp::AggregateProjectBorrow {
            dest, aggregate, ..
        }
        | PhysicalOp::VariantIs {
            dest,
            source: aggregate,
            ..
        }
        | PhysicalOp::VariantProjectCopy {
            dest,
            source: aggregate,
            ..
        }
        | PhysicalOp::VariantProjectBorrow {
            dest,
            source: aggregate,
            ..
        } => {
            defined.insert(*dest);
            used.insert(*aggregate);
        }
        PhysicalOp::AggregateDestructure {
            aggregate, fields, ..
        }
        | PhysicalOp::VariantDestructure {
            source: aggregate,
            fields,
            ..
        } => {
            defined.extend(fields);
            used.insert(*aggregate);
        }
        PhysicalOp::Destroy { source, .. } | PhysicalOp::EndBorrow { source } => {
            used.insert(*source);
        }
        PhysicalOp::Assign { dest, source, .. } => {
            used.extend([*dest, *source]);
        }
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "one exhaustive physical storage reference contract"
)]
pub(super) fn terminator_storage(term: &PhysicalTerminator, used: &mut BTreeSet<StorageId>) {
    let source = |arg: &ArgumentTransfer| match arg {
        ArgumentTransfer::Borrow(id)
        | ArgumentTransfer::BorrowMut(id)
        | ArgumentTransfer::Move(id)
        | ArgumentTransfer::Clone { source: id, .. } => *id,
    };
    for edge in super::defer::edges(term) {
        used.extend(
            edge.transfers
                .iter()
                .chain(&edge.leaf_transfers)
                .flat_map(|(source, dest)| [*source, *dest]),
        );
    }
    match term {
        PhysicalTerminator::StreamNext { stream, result, .. }
        | PhysicalTerminator::GeneratorNext {
            generator: stream,
            result,
            ..
        } => {
            used.extend([source(stream), *result]);
        }
        PhysicalTerminator::StreamSend {
            sink,
            value,
            committed,
            ..
        } => {
            used.extend([source(sink), source(value)]);
            used.extend(committed);
        }
        PhysicalTerminator::ActorAsk { args, result, .. }
        | PhysicalTerminator::NativeIo { args, result, .. }
        | PhysicalTerminator::ValueCall { args, result, .. } => {
            used.extend(args.iter().map(source));
            used.insert(*result);
        }
        PhysicalTerminator::RemoteAsk {
            target,
            payload,
            timeout,
            result,
            ..
        } => {
            used.extend([*target, *payload, *timeout, *result]);
        }
        PhysicalTerminator::TaskSelect {
            sources,
            timeout,
            result,
            ..
        } => {
            used.extend(sources.iter().map(|input| source(&input.transfer())));
            used.extend(timeout);
            used.insert(*result);
        }
        PhysicalTerminator::GeneratorYield { value, .. } => {
            used.insert(source(value));
        }
        PhysicalTerminator::TaskAwait { task, result, .. } => {
            used.insert(source(task));
            used.extend(result);
        }
        PhysicalTerminator::Offload { args, result, .. }
        | PhysicalTerminator::ActorCall { args, result, .. }
        | PhysicalTerminator::ExternCall { args, result, .. }
        | PhysicalTerminator::RuntimeCall { args, result, .. } => {
            used.extend(args.iter().map(source));
            used.extend(result);
        }
        PhysicalTerminator::Sleep { duration, .. }
        | PhysicalTerminator::SleepUntil {
            deadline: duration, ..
        }
        | PhysicalTerminator::RecoverFault {
            result: duration, ..
        }
        | PhysicalTerminator::Branch {
            condition: duration,
            ..
        } => {
            used.insert(*duration);
        }
        PhysicalTerminator::CheckedBinary {
            lhs, rhs, result, ..
        } => {
            used.extend([*lhs, *rhs, *result]);
        }
        PhysicalTerminator::SwitchVariant {
            scrutinee, arms, ..
        } => {
            used.insert(*scrutinee);
            used.extend(arms.iter().flat_map(|arm| arm.fields.iter()).copied());
        }
        PhysicalTerminator::Call {
            args,
            result,
            handback,
            ..
        } => {
            used.extend(args.iter().map(source));
            used.extend(result);
            used.extend(handback);
        }
        PhysicalTerminator::WireCodec { input, result, .. } => {
            used.extend([source(input), *result]);
        }
        PhysicalTerminator::IndirectCall {
            callee,
            args,
            result,
            ..
        }
        | PhysicalTerminator::DynCall {
            receiver: callee,
            args,
            result,
            ..
        } => {
            used.insert(source(callee));
            used.extend(args.iter().map(source));
            used.extend(result);
        }
        PhysicalTerminator::Panic {
            message, assertion, ..
        } => {
            used.insert(source(message));
            if let Some(assertion) = assertion {
                used.extend(assertion.iter().map(source));
            }
        }
        PhysicalTerminator::Return { value } => {
            if let Some(value) = value {
                used.insert(match value {
                    super::ReturnTransfer::Borrow(id)
                    | super::ReturnTransfer::Move(id)
                    | super::ReturnTransfer::Clone { source: id, .. } => *id,
                });
            }
        }
        PhysicalTerminator::PropagateFault { handback } => {
            used.extend(handback);
        }
        PhysicalTerminator::TaskScopeJoin { .. }
        | PhysicalTerminator::EnterDefer { .. }
        | PhysicalTerminator::FinishDefer { .. }
        | PhysicalTerminator::CleanupDispatch { .. }
        | PhysicalTerminator::CheckedRaiseFault { .. }
        | PhysicalTerminator::Goto(_)
        | PhysicalTerminator::Trap(_)
        | PhysicalTerminator::Unreachable => {}
    }
}
