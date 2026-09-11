//! Independently verify deferred-region storage and optional fault carriers.

use std::collections::{BTreeMap, BTreeSet};

use super::{
    apply_edge, storage, ArgumentTransfer, BlockId, CallableId, DeferId, DeferScopeId, FaultParkId,
    FaultState, FlowState, InitState, OwnKind, PhysicalBlock, PhysicalEdge, PhysicalError,
    PhysicalFunction, PhysicalModule, PhysicalOp, PhysicalTerminator, StorageId, StorageOrigin,
    TrapKind,
};

pub(super) const ORDINARY: u8 = 1;
pub(super) const TRAP: u8 = 2;
pub(super) const CANCEL: u8 = 4;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Pending {
    defer: DeferId,
    scope: DeferScopeId,
    dependencies: Vec<StorageId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Active {
    defer: DeferId,
    scope: DeferScopeId,
    park: FaultParkId,
    entry: BlockId,
    pending_base: usize,
    fault: FaultState,
    exit: u8,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(super) struct State {
    pub pending: Vec<Pending>,
    pub active: Vec<Active>,
    invalid_join: bool,
}

impl State {
    pub fn same_phase(&self, other: &Self) -> bool {
        self.pending == other.pending
            && self.active.len() == other.active.len()
            && self.active.iter().zip(&other.active).all(|(a, b)| {
                (a.defer, a.scope, a.park, a.entry, a.pending_base)
                    == (b.defer, b.scope, b.park, b.entry, b.pending_base)
            })
    }
    pub fn same_faults(&self, other: &Self) -> bool {
        self.same_phase(other)
            && self
                .active
                .iter()
                .zip(&other.active)
                .all(|(a, b)| (a.fault, a.exit) == (b.fault, b.exit))
    }

    pub fn join(&mut self, other: &Self) -> bool {
        let before = self.clone();
        if self.same_phase(other) {
            for (a, b) in self.active.iter_mut().zip(&other.active) {
                if a.fault != b.fault {
                    a.fault = FaultState::MaybeActive;
                }
                a.exit |= b.exit;
            }
        } else {
            self.invalid_join = true;
        }
        self.invalid_join |= other.invalid_join;
        *self != before
    }

    pub fn register(
        &mut self,
        defer: DeferId,
        scope: DeferScopeId,
        dependencies: &[StorageId],
    ) -> Result<(), PhysicalError> {
        if self.pending.iter().any(|p| p.defer == defer)
            || self
                .active
                .iter()
                .any(|a| a.defer == defer || a.scope == scope)
        {
            return Err(PhysicalError::new(
                "physical defer registration is reused or not in a nested scope",
            ));
        }
        self.pending.push(Pending {
            defer,
            scope,
            dependencies: dependencies.to_vec(),
        });
        Ok(())
    }
}

pub(super) struct Region {
    defer: DeferId,
    blocks: BTreeSet<BlockId>,
    locals: BTreeSet<StorageId>,
    values: BTreeSet<StorageId>,
}

pub(super) type Plan = BTreeMap<BlockId, Region>;

#[expect(
    clippy::too_many_lines,
    reason = "one flat arm per terminator shape; splitting it would hide which \
              shapes carry which edges"
)]
pub(super) fn edges(term: &PhysicalTerminator) -> Vec<&PhysicalEdge> {
    match term {
        PhysicalTerminator::StreamSend {
            normal,
            closed,
            cancel,
            unwind,
            ..
        } => vec![normal, closed, cancel, unwind],
        PhysicalTerminator::GeneratorYield {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::StreamNext {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::ChannelRecv {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::ChannelSend {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::ActorAsk {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::GeneratorNext {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::NativeIo {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::Sleep {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::SleepUntil {
            normal,
            cancel,
            unwind,
            ..
        }
        | PhysicalTerminator::TaskSelect {
            normal,
            cancel,
            unwind,
            ..
        } => vec![normal, cancel, unwind],
        PhysicalTerminator::TaskAwait {
            normal,
            cancel,
            unwind,
            ..
        } => normal.iter().chain([cancel, unwind]).collect(),
        PhysicalTerminator::ValueClose { next: body, .. }
        | PhysicalTerminator::EnterDefer { body, .. }
        | PhysicalTerminator::FinishDefer { next: body, .. }
        | PhysicalTerminator::CheckedRaiseFault { cleanup: body, .. }
        | PhysicalTerminator::Panic { cleanup: body, .. }
        | PhysicalTerminator::Goto(body) => vec![body],
        PhysicalTerminator::RecoverFault {
            normal,
            unwind: fault,
            ..
        }
        | PhysicalTerminator::CleanupDispatch { normal, fault } => vec![normal, fault],
        PhysicalTerminator::Branch {
            then_target,
            else_target,
            ..
        } => vec![then_target, else_target],
        PhysicalTerminator::SwitchVariant { arms, .. } => {
            arms.iter().map(|arm| &arm.target).collect()
        }
        PhysicalTerminator::CheckedBinary {
            normal, failures, ..
        } => std::iter::once(normal)
            .chain(failures.iter().map(|f| &f.edge))
            .collect(),
        PhysicalTerminator::Call { normal, unwind, .. }
        | PhysicalTerminator::IndirectCall { normal, unwind, .. }
        | PhysicalTerminator::DynCall { normal, unwind, .. } => {
            normal.iter().chain(unwind).collect()
        }
        PhysicalTerminator::ActorCall { normal, unwind, .. } => {
            std::iter::once(normal).chain(unwind).collect()
        }
        PhysicalTerminator::TaskScopeJoin { normal, unwind, .. }
        | PhysicalTerminator::WireCodec { normal, unwind, .. }
        | PhysicalTerminator::ValueCall { normal, unwind, .. } => vec![normal, unwind],
        PhysicalTerminator::RuntimeCall {
            normal, failure, ..
        } => std::iter::once(normal).chain(failure).collect(),
        PhysicalTerminator::ExternCall { normal, .. } => vec![normal],
        PhysicalTerminator::Return { .. }
        | PhysicalTerminator::PropagateFault
        | PhysicalTerminator::Trap(_)
        | PhysicalTerminator::Unreachable => vec![],
    }
}

fn operation_storage(
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
        PhysicalOp::TupleMake { dest, elements } => {
            defined.insert(*dest);
            used.extend(elements);
        }
        PhysicalOp::TupleGet { dest, tuple, .. } => {
            defined.insert(*dest);
            used.insert(*tuple);
        }
        PhysicalOp::AggregateMake { dest, fields, .. }
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

fn terminator_storage(term: &PhysicalTerminator, used: &mut BTreeSet<StorageId>) {
    let source = |arg: &ArgumentTransfer| match arg {
        ArgumentTransfer::Borrow(id)
        | ArgumentTransfer::BorrowMut(id)
        | ArgumentTransfer::Move(id)
        | ArgumentTransfer::Clone { source: id, .. } => *id,
    };
    for edge in edges(term) {
        used.extend(
            edge.transfers
                .iter()
                .chain(&edge.leaf_transfers)
                .flat_map(|(source, dest)| [*source, *dest]),
        );
    }
    match term {
        PhysicalTerminator::Branch { condition, .. } => {
            used.insert(*condition);
        }
        PhysicalTerminator::CheckedBinary { lhs, rhs, .. } => {
            used.extend([*lhs, *rhs]);
        }
        PhysicalTerminator::SwitchVariant { scrutinee, .. } => {
            used.insert(*scrutinee);
        }
        PhysicalTerminator::NativeIo { args, .. }
        | PhysicalTerminator::Call { args, .. }
        | PhysicalTerminator::RuntimeCall { args, .. }
        | PhysicalTerminator::ValueCall { args, .. } => used.extend(args.iter().map(source)),
        PhysicalTerminator::WireCodec { input, .. } => {
            used.insert(source(input));
        }
        PhysicalTerminator::IndirectCall { callee, args, .. } => {
            used.insert(source(callee));
            used.extend(args.iter().map(source));
        }
        PhysicalTerminator::DynCall { receiver, args, .. } => {
            used.insert(source(receiver));
            used.extend(args.iter().map(source));
        }
        PhysicalTerminator::Panic { message, .. } => {
            used.insert(source(message));
        }
        _ => {}
    }
}

pub(super) fn verify_calls(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    plan: &Plan,
) -> Result<(), PhysicalError> {
    fn inspect(
        module: &PhysicalModule,
        term: &PhysicalTerminator,
        seen: &mut BTreeSet<CallableId>,
    ) -> Result<(), PhysicalError> {
        match term {
            PhysicalTerminator::Call { callee, .. } => {
                if !seen.insert(*callee) {
                    return Ok(());
                }
                let body = module
                    .functions
                    .iter()
                    .find(|f| f.callable == *callee)
                    .ok_or_else(|| {
                        PhysicalError::new("physical defer call has no proven non-suspending body")
                    })?;
                for block in &body.blocks {
                    inspect(module, &block.terminator, seen)?;
                }
            }
            PhysicalTerminator::Sleep { .. }
            | PhysicalTerminator::SleepUntil { .. }
            | PhysicalTerminator::TaskSelect { .. }
            | PhysicalTerminator::GeneratorYield { .. }
            | PhysicalTerminator::GeneratorNext { .. }
            | PhysicalTerminator::StreamNext { .. }
            | PhysicalTerminator::StreamSend { .. }
            | PhysicalTerminator::ChannelRecv { .. }
            | PhysicalTerminator::ChannelSend { .. }
            | PhysicalTerminator::TaskAwait { .. }
            | PhysicalTerminator::ActorAsk { .. }
            | PhysicalTerminator::TaskScopeJoin { .. }
            | PhysicalTerminator::IndirectCall { .. }
            | PhysicalTerminator::DynCall { .. }
            | PhysicalTerminator::ValueCall { .. } => {
                return Err(PhysicalError::new(
                    "physical defer call has unproven effects",
                ));
            }
            PhysicalTerminator::RuntimeCall { action, .. }
                if action
                    .family
                    .semantic_contract()
                    .is_none_or(hew_types::RuntimeSemanticContract::propagates_fault) =>
            {
                return Err(PhysicalError::new(
                    "physical defer call has unproven effects",
                ));
            }
            _ => {}
        }
        Ok(())
    }
    let blocks: BTreeSet<_> = plan
        .values()
        .flat_map(|r| r.blocks.iter().copied())
        .collect();
    let mut seen = BTreeSet::new();
    for block in &function.blocks {
        if blocks.contains(&block.id) {
            inspect(module, &block.terminator, &mut seen)?;
        }
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "keep physical region traversal and dependency validation together"
)]
pub(super) fn verify_regions(function: &PhysicalFunction) -> Result<Plan, PhysicalError> {
    let blocks: BTreeMap<_, _> = function.blocks.iter().map(|b| (b.id, b)).collect();
    let mut registrations = BTreeMap::new();
    for block in &function.blocks {
        for op in &block.ops {
            if let PhysicalOp::RegisterDefer {
                defer,
                scope,
                dependencies,
            } = op
            {
                let unique: BTreeSet<_> = dependencies.iter().copied().collect();
                if unique.len() != dependencies.len()
                    || registrations.insert(*defer, (*scope, unique)).is_some()
                {
                    return Err(PhysicalError::new(
                        "physical defer registration or dependency identity is duplicated",
                    ));
                }
            }
        }
    }
    let mut plan = Plan::new();
    let mut parks = BTreeMap::new();
    let mut entered = BTreeSet::new();
    for entry in &function.blocks {
        let PhysicalTerminator::EnterDefer { defer, park, body } = &entry.terminator else {
            continue;
        };
        let (scope, dependencies) = registrations
            .get(defer)
            .ok_or_else(|| PhysicalError::new("physical defer entry has no registration"))?;
        if parks.insert(*scope, *park).is_some_and(|old| old != *park) {
            return Err(PhysicalError::new(
                "physical cleanup scope does not reuse its fault park",
            ));
        }
        entered.insert(*defer);
        let mut region = Region {
            defer: *defer,
            blocks: BTreeSet::new(),
            locals: BTreeSet::new(),
            values: BTreeSet::new(),
        };
        let mut used = BTreeSet::new();
        let mut pending = vec![body.target];
        let mut finish = false;
        let mut diverged = false;
        while let Some(id) = pending.pop() {
            if !region.blocks.insert(id) {
                continue;
            }
            let block = blocks
                .get(&id)
                .ok_or_else(|| PhysicalError::new("physical defer body has an unknown block"))?;
            region.values.extend(&block.arguments);
            for op in &block.ops {
                operation_storage(op, &mut used, &mut region.values, &mut region.locals);
            }
            terminator_storage(&block.terminator, &mut used);
            if let PhysicalTerminator::Call { unwind, .. } = &block.terminator {
                if !unwind
                    .as_ref()
                    .is_some_and(|edge| drain_suffix(edge.target, &blocks, &mut BTreeSet::new()))
                {
                    return Err(PhysicalError::new(
                        "physical defer call failure bypasses bounded cleanup",
                    ));
                }
            }
            match &block.terminator {
                PhysicalTerminator::CheckedBinary { result, .. }
                | PhysicalTerminator::NativeIo { result, .. }
                | PhysicalTerminator::WireCodec { result, .. }
                | PhysicalTerminator::ValueCall { result, .. } => {
                    region.values.insert(*result);
                }
                PhysicalTerminator::Call { result, .. }
                | PhysicalTerminator::IndirectCall { result, .. }
                | PhysicalTerminator::DynCall { result, .. }
                | PhysicalTerminator::RuntimeCall { result, .. } => {
                    region.values.extend(result);
                }
                PhysicalTerminator::SwitchVariant { arms, .. } => {
                    for arm in arms {
                        region.values.extend(&arm.fields);
                    }
                }
                _ => {}
            }
            match &block.terminator {
                PhysicalTerminator::FinishDefer {
                    defer: done,
                    park: done_park,
                    next,
                } if done == defer => {
                    if done_park != park {
                        return Err(PhysicalError::new("physical defer finishes the wrong park"));
                    }
                    if !drain_suffix(next.target, &blocks, &mut BTreeSet::new()) {
                        return Err(PhysicalError::new(
                            "physical defer finish bypasses bounded cleanup advancement",
                        ));
                    }
                    finish = true;
                }
                // Divergence is not escape: a `Never`-typed call in the body
                // never returns control, so it owes no finish.
                PhysicalTerminator::Unreachable => diverged = true,
                PhysicalTerminator::Return { .. }
                | PhysicalTerminator::PropagateFault
                | PhysicalTerminator::Trap(_) => {
                    return Err(PhysicalError::new(
                        "physical defer body escapes its matching finish",
                    ));
                }
                _ => pending.extend(edges(&block.terminator).iter().map(|e| e.target)),
            }
        }
        if !finish && !diverged {
            return Err(PhysicalError::new("physical defer body has no finish"));
        }
        let is_place = |id| {
            storage(function, id).is_ok_and(|s| {
                matches!(
                    s.origin,
                    StorageOrigin::Local(_)
                        | StorageOrigin::Aggregate(_)
                        | StorageOrigin::Capture { .. }
                )
            })
        };
        let local = |id: StorageId| {
            let root = function.place_storage.get(&id).map_or(id, |p| p.root);
            region.locals.contains(&root) || (region.values.contains(&root) && !is_place(root))
        };
        used.retain(|id| is_place(*id) && !local(*id));
        // SIR's exact dependencies remain reserved after impossible fault
        // alternatives are erased. Physical verification must still cover
        // every remaining free place; it must not weaken that reservation.
        if !used.is_subset(dependencies) {
            return Err(PhysicalError::new(
                "physical defer dependencies omit free storage places",
            ));
        }
        region.values.retain(|id| !is_place(*id));
        plan.insert(entry.id, region);
    }
    if entered.len() != registrations.len() {
        return Err(PhysicalError::new(
            "physical registration has no action body",
        ));
    }
    for block in &function.blocks {
        if let PhysicalTerminator::CheckedRaiseFault { kind, .. } = block.terminator {
            if !raise_origin(block.id, kind, function, &mut BTreeSet::new()) {
                return Err(PhysicalError::new(
                    "physical checked raise differs from its producing failure edge",
                ));
            }
        }
    }
    Ok(plan)
}

fn pure(op: &PhysicalOp) -> bool {
    matches!(
        op,
        PhysicalOp::Destroy { .. } | PhysicalOp::StorageDead { .. } | PhysicalOp::EndBorrow { .. }
    )
}

fn drain_suffix(
    id: BlockId,
    blocks: &BTreeMap<BlockId, &PhysicalBlock>,
    visiting: &mut BTreeSet<BlockId>,
) -> bool {
    if !visiting.insert(id) {
        return false;
    }
    let Some(block) = blocks.get(&id) else {
        return false;
    };
    if !block.ops.iter().all(pure) {
        return false;
    }
    let valid = match &block.terminator {
        PhysicalTerminator::EnterDefer { .. }
        | PhysicalTerminator::FinishDefer { .. }
        | PhysicalTerminator::CleanupDispatch { .. }
        | PhysicalTerminator::RecoverFault { .. } => true,
        PhysicalTerminator::Goto(_)
        | PhysicalTerminator::Branch { .. }
        | PhysicalTerminator::ValueClose { .. } => edges(&block.terminator)
            .iter()
            .all(|e| drain_suffix(e.target, blocks, visiting)),
        _ => false,
    };
    visiting.remove(&id);
    valid
}

fn raise_origin(
    target: BlockId,
    kind: TrapKind,
    function: &PhysicalFunction,
    visiting: &mut BTreeSet<BlockId>,
) -> bool {
    if target == function.entry || !visiting.insert(target) {
        return false;
    }
    let mut found = false;
    for block in &function.blocks {
        for (slot, edge) in edges(&block.terminator).iter().enumerate() {
            if edge.target != target {
                continue;
            }
            found = true;
            let valid = match &block.terminator {
                PhysicalTerminator::CheckedBinary { failures, .. } => {
                    slot > 0 && failures.get(slot - 1).is_some_and(|f| f.kind == kind)
                }
                PhysicalTerminator::RuntimeCall {
                    action,
                    failure: Some(_),
                    ..
                } => {
                    slot == 1
                        && action.family.semantic_contract().is_some_and(|c| {
                            !c.failures.is_empty()
                                && c.failures
                                    .iter()
                                    .all(|f| hew_sir::runtime_failure_trap_kind(*f) == Some(kind))
                        })
                }
                PhysicalTerminator::Goto(_) | PhysicalTerminator::Branch { .. } => {
                    block.ops.iter().all(pure) && raise_origin(block.id, kind, function, visiting)
                }
                _ => false,
            };
            if !valid {
                return false;
            }
        }
    }
    visiting.remove(&target);
    found
}

pub(super) fn verify_entry_phase(
    plan: &Plan,
    block: BlockId,
    state: &FlowState,
) -> Result<(), PhysicalError> {
    if state.defers.invalid_join {
        return Err(PhysicalError::new(
            "physical CFG joins incompatible defer phases",
        ));
    }
    for region in plan.values() {
        if region.blocks.contains(&block)
            && !state.defers.active.iter().any(|f| f.defer == region.defer)
        {
            return Err(PhysicalError::new(
                "physical defer body bypasses its entry boundary",
            ));
        }
    }
    Ok(())
}

pub(super) fn require_unreserved(
    function: &PhysicalFunction,
    state: &FlowState,
    source: StorageId,
) -> Result<(), PhysicalError> {
    for pending in &state.defers.pending {
        for dependency in &pending.dependencies {
            let capture = storage(function, *dependency).is_ok_and(|s| matches!(s.origin, StorageOrigin::Capture { environment, .. } if environment == source));
            let overlaps = source == *dependency
                || capture
                || match (
                    function.place_storage.get(&source),
                    function.place_storage.get(dependency),
                ) {
                    (Some(a), Some(b)) => {
                        a.root == b.root
                            && a.leaves.iter().any(|leaf| {
                                b.leaves.iter().any(|other| leaf.storage == other.storage)
                            })
                    }
                    (None, Some(b)) => source == b.root,
                    _ => false,
                };
            if overlaps {
                return Err(PhysicalError::new(
                    "physical consume or end invalidates a pending defer dependency",
                ));
            }
        }
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "keep optional fault and defer phase transitions together"
)]
pub(super) fn successors(
    function: &PhysicalFunction,
    borrows: &super::BorrowDependents,
    plan: &Plan,
    terminator: &PhysicalTerminator,
    mut state: FlowState,
    block: BlockId,
) -> Result<Vec<(BlockId, FlowState)>, PhysicalError> {
    match terminator {
        PhysicalTerminator::EnterDefer { defer, park, body } => {
            let pending =
                state.defers.pending.last().ok_or_else(|| {
                    PhysicalError::new("physical defer entry has no pending action")
                })?;
            if pending.defer != *defer
                || state.defers.active.iter().any(|a| a.park == *park)
                || state
                    .defers
                    .active
                    .last()
                    .is_some_and(|a| state.defers.pending.len() <= a.pending_base)
            {
                return Err(PhysicalError::new(
                    "physical defer entry skips the top action or overwrites a live park",
                ));
            }
            let scope = pending.scope;
            state.defers.pending.pop();
            state.defers.active.push(Active {
                defer: *defer,
                scope,
                park: *park,
                entry: block,
                pending_base: state.defers.pending.len(),
                fault: state.fault,
                exit: state.exit,
            });
            state.fault = FaultState::None;
            Ok(vec![apply_edge(function, borrows, body, state, block)?])
        }
        PhysicalTerminator::FinishDefer { defer, park, next } => {
            let active = state
                .defers
                .active
                .last()
                .ok_or_else(|| PhysicalError::new("physical finish has no active defer"))?;
            if (active.defer, active.park) != (*defer, *park)
                || state.defers.pending.len() != active.pending_base
            {
                return Err(PhysicalError::new(
                    "physical defer finish mismatches its park or leaves nested actions",
                ));
            }
            let region = &plan[&active.entry];
            if region
                .locals
                .iter()
                .any(|id| state.active[id.0 as usize] != InitState::Uninitialized)
                || region.values.iter().any(|id| {
                    function.storage[id.0 as usize].own != OwnKind::None
                        && state.slots[id.0 as usize] != InitState::Uninitialized
                })
            {
                return Err(PhysicalError::new(
                    "physical defer finish leaves body-local storage, owners or loans live",
                ));
            }
            state.fault = match (active.fault, state.fault) {
                (FaultState::Active, _) | (_, FaultState::Active) => FaultState::Active,
                (FaultState::None, FaultState::None) => FaultState::None,
                _ => FaultState::MaybeActive,
            };
            state.exit = (active.exit | state.exit) & !ORDINARY
                | if active.exit & ORDINARY != 0 && state.exit & ORDINARY != 0 {
                    ORDINARY
                } else {
                    0
                };
            state.defers.active.pop();
            Ok(vec![apply_edge(function, borrows, next, state, block)?])
        }
        PhysicalTerminator::CleanupDispatch { normal, fault } => {
            let mut out = vec![];
            if state.fault != FaultState::Active {
                let mut success = state.clone();
                success.fault = FaultState::None;
                success.exit = success.defers.active.last().map_or(ORDINARY, |a| a.exit);
                out.push(apply_edge(function, borrows, normal, success, block)?);
            }
            if state.fault != FaultState::None {
                state.fault = FaultState::Active;
                state.exit = TRAP;
                out.push(apply_edge(function, borrows, fault, state, block)?);
            }
            Ok(out)
        }
        PhysicalTerminator::CheckedRaiseFault { cleanup, .. } => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "physical checked raise overwrites an active fault",
                ));
            }
            state.fault = FaultState::Active;
            state.exit = TRAP;
            Ok(vec![apply_edge(function, borrows, cleanup, state, block)?])
        }
        _ => unreachable!("caller selected a defer boundary"),
    }
}
