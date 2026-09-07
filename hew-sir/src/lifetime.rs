//! Path-sensitive availability of SSA obligations, local storage and loans.
//!
//! Guaranteed inputs can be read or explicitly copied, never consumed or
//! escaped. Local loans keep their immediate owner or parent loan live until
//! they end. Local activity is separate from content availability; capture
//! initialization follows the same paths as its environment.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use crate::{
    BlockId, BoundaryDecision, CallUnwind, Edge, OwnKind, OwnerRoot, PlaceBase, SemFunction,
    SemOpKind, SemTerminator, SnapshotDecision, ValueId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Violation {
    pub block: BlockId,
    pub value: Option<ValueId>,
    pub place: Option<crate::PlaceId>,
    pub reason: &'static str,
}

// A set of possible availability states. Joins union possibilities; a use
// requires LIVE alone, while a definition requires DEAD alone. Definitions
// reset availability, so a loop-local SSA definition creates a fresh dynamic
// obligation rather than inheriting the previous iteration's consumption.
const DEAD: u8 = 1;
const LIVE: u8 = 2;
#[derive(Clone)]
struct State {
    values: Vec<u8>,
    fault: u8,
    places: Vec<u8>,
    locals: Vec<u8>,
    exit: u8,
    defers: crate::defer::Schedule,
}

const ORDINARY: u8 = 1;
const TRAP: u8 = 2;
const CANCEL: u8 = 4;

/// Verified semantics of an implicit cleanup, independent of fault transport.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CleanupMode {
    /// Normal scope exit or replacement; a linear consume is still required.
    Ordinary,
    /// A trap-only cleanup reclaims representation without a linear consume.
    /// Resource close behaviour remains part of the type's drop contract.
    Trap,
}

/// Immutable output of the existing lifetime flow for one checked function.
/// Rebuild after changing its operations or control flow.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlaceLifetimes {
    operations: BTreeMap<crate::OpId, CleanupMode>,
    reachable: BTreeSet<BlockId>,
}

impl PlaceLifetimes {
    fn new() -> Self {
        Self {
            operations: BTreeMap::new(),
            reachable: BTreeSet::new(),
        }
    }

    /// Whether some verified fault/park alternative reaches this block.
    #[must_use]
    pub fn is_reachable(&self, block: BlockId) -> bool {
        self.reachable.contains(&block)
    }

    /// The checked disposition of a reachable end-lifetime or destroy operation.
    #[must_use]
    pub fn cleanup(&self, operation: crate::OpId) -> Option<CleanupMode> {
        self.operations.get(&operation).copied()
    }
}

pub(crate) struct Analysis {
    pub violations: Vec<Violation>,
    pub lifetimes: PlaceLifetimes,
}

#[allow(
    clippy::too_many_lines,
    reason = "keep availability convergence and final diagnostics together"
)]
pub(crate) fn verify(
    function: &SemFunction,
    projections: &crate::PlacePlan,
    facts: &crate::ownership::TypeFactTable,
) -> Analysis {
    let flow = Flow::new(function, projections, facts);
    let mut lifetimes = PlaceLifetimes::new();
    if !flow.blocks.contains_key(&function.entry) {
        return Analysis {
            violations: Vec::new(),
            lifetimes,
        };
    }
    let mut initial = State {
        values: vec![DEAD; flow.values.len()],
        fault: DEAD,
        exit: ORDINARY,
        defers: crate::defer::Schedule::default(),
        locals: vec![DEAD; flow.local_indices.len()],
        places: flow
            .places
            .iter()
            .map(|(id, _)| {
                if flow.projections.projection(*id).is_some() {
                    DEAD
                } else {
                    LIVE
                }
            })
            .collect(),
    };
    for param in &function.params {
        if let Some(&index) = flow.indices.get(&param.value) {
            initial.values[index] = LIVE;
        }
        for (index, (_, owner)) in flow.places.iter().enumerate() {
            if *owner == OwnerRoot::Value(param.value) {
                initial.places[index] = LIVE;
            }
        }
    }
    // Availability is joined only within one active/parked fault alternative.
    // Otherwise an absent saved return on the body-fault path would poison the
    // no-fault continuation, or a refinement could incorrectly invent a value.
    let mut incoming = BTreeMap::from([(function.entry, vec![initial])]);
    let mut queue = VecDeque::from([function.entry]);
    let mut queued = BTreeSet::from([function.entry]);
    while let Some(block) = queue.pop_front() {
        queued.remove(&block);
        for alternative in incoming[&block].clone() {
            let cleanup_exit = alternative.exit;
            for (target, mut state) in flow.block(
                block,
                alternative,
                cleanup_exit,
                &mut |_| {},
                &mut lifetimes,
            ) {
                let alternatives = incoming.entry(target).or_default();
                let incompatible = alternatives
                    .iter()
                    .any(|old| !old.defers.same_phase(&state.defers));
                let mut changed = false;
                if incompatible {
                    state.defers.invalid_join = true;
                    for old in alternatives.iter_mut() {
                        changed |= !old.defers.invalid_join;
                        old.defers.invalid_join = true;
                    }
                }
                if let Some(previous) = alternatives.iter_mut().find(|old| {
                    old.fault == state.fault
                        && old.exit == state.exit
                        && old.defers.same_faults(&state.defers)
                }) {
                    changed |= previous.defers.join(&state.defers);
                    for (before, after) in previous
                        .locals
                        .iter_mut()
                        .zip(state.locals)
                        .chain(previous.values.iter_mut().zip(state.values))
                        .chain(previous.places.iter_mut().zip(state.places))
                    {
                        let joined = *before | after;
                        changed |= joined != *before;
                        *before = joined;
                    }
                } else {
                    alternatives.push(state);
                    changed = true;
                }
                if changed && queued.insert(target) {
                    queue.push_back(target);
                }
            }
        }
    }
    // Diagnose the fixed point, not a transient partial predecessor set.
    let mut violations = Vec::new();
    lifetimes.operations.clear();
    lifetimes.reachable.extend(incoming.keys().copied());
    for (block, alternatives) in incoming {
        // Cleanup has one disposition per operation. Keep the exit-cause
        // certainty common to all alternatives without merging their owners.
        let cleanup_exit = alternatives
            .iter()
            .fold(0, |causes, state| causes | state.exit);
        for state in alternatives {
            flow.block(
                block,
                state,
                cleanup_exit,
                &mut |v| violations.push(v),
                &mut lifetimes,
            );
        }
    }
    Analysis {
        violations,
        lifetimes,
    }
}

fn combine_fault(primary: u8, secondary: u8) -> u8 {
    (if primary & LIVE != 0 || secondary & LIVE != 0 {
        LIVE
    } else {
        0
    }) | (if primary & DEAD != 0 && secondary & DEAD != 0 {
        DEAD
    } else {
        0
    })
}

fn combine_exit(primary: u8, secondary: u8) -> u8 {
    (primary | secondary) & !ORDINARY
        | if primary & ORDINARY != 0 && secondary & ORDINARY != 0 {
            ORDINARY
        } else {
            0
        }
}

fn mark_trap(state: &mut State) {
    state.exit = if state.exit == ORDINARY {
        TRAP
    } else {
        state.exit | TRAP
    };
}

fn is_cleanup(kind: &SemOpKind) -> bool {
    matches!(
        kind,
        SemOpKind::EndBorrow { .. }
            | SemOpKind::TaskScopeClose { .. }
            | SemOpKind::DestroyValue { .. }
            | SemOpKind::EndLifetime { .. }
    )
}

/// A least fixed point admits only cleanup suffixes with a finite trap exit.
/// A cycle cannot justify itself, and an ordinary operation cannot enter the
/// region merely because a later terminator happens to trap.
pub(crate) fn cleanup_suffixes(function: &SemFunction) -> BTreeMap<BlockId, usize> {
    let boundaries = crate::defer::plan(function).ok();
    let mut suffixes = BTreeMap::new();
    loop {
        let before = suffixes.len();
        for block in &function.blocks {
            if suffixes.contains_key(&block.id) {
                continue;
            }
            let terminal = match &block.terminator {
                SemTerminator::Trap { .. }
                | SemTerminator::ResumeUnwind
                | SemTerminator::RecoverFault { .. } => true,
                SemTerminator::Suspend {
                    kind: crate::SuspendKind::Join { cancel: true, .. },
                    resumes,
                    cancel,
                    unwind,
                    ..
                } => resumes
                    .iter()
                    .chain([cancel, unwind])
                    .all(|edge| suffixes.get(&edge.target) == Some(&0)),
                SemTerminator::CleanupDispatch { fault, .. } => {
                    suffixes.get(&fault.target) == Some(&0)
                }
                SemTerminator::EnterDefer { .. }
                | SemTerminator::FinishDefer { .. }
                | SemTerminator::CheckedRaiseFault { .. } => boundaries.is_some(),
                SemTerminator::Goto(edge) => suffixes.get(&edge.target) == Some(&0),
                SemTerminator::Suspend {
                    kind: crate::SuspendKind::ValueClose { .. },
                    resumes,
                    ..
                } => resumes
                    .iter()
                    .all(|edge| suffixes.get(&edge.target) == Some(&0)),
                SemTerminator::Branch {
                    then_target,
                    else_target,
                    ..
                } => {
                    suffixes.get(&then_target.target) == Some(&0)
                        && suffixes.get(&else_target.target) == Some(&0)
                }
                _ => false,
            };
            if terminal {
                let start = block
                    .ops
                    .iter()
                    .rposition(|op| !is_cleanup(&op.kind))
                    .map_or(0, |index| index + 1);
                suffixes.insert(block.id, start);
            }
        }
        if suffixes.len() == before {
            return suffixes;
        }
    }
}

struct Flow<'a> {
    defers: crate::defer::Plan,
    blocks: BTreeMap<BlockId, &'a crate::SemBlock>,
    indices: BTreeMap<ValueId, usize>,
    values: Vec<ValueId>,
    guaranteed: BTreeSet<ValueId>,
    local_borrows: BTreeSet<ValueId>,
    parents: BTreeMap<ValueId, PlaceBase>,
    // Derived lookup index over the same canonical dependency graph.
    dependents: BTreeMap<PlaceBase, Vec<PlaceBase>>,
    local_indices: BTreeMap<crate::PlaceId, usize>,
    linear_places: BTreeSet<crate::PlaceId>,
    linear_values: BTreeSet<ValueId>,
    cleanup_suffixes: BTreeMap<BlockId, usize>,
    places: Vec<(crate::PlaceId, OwnerRoot)>,
    projections: &'a crate::PlacePlan,
    place_indices: BTreeMap<crate::PlaceId, usize>,
}

impl<'a> Flow<'a> {
    #[allow(
        clippy::too_many_lines,
        reason = "constructs the derived value, place and dependency indices from one canonical function"
    )]
    fn new(
        function: &'a SemFunction,
        projections: &'a crate::PlacePlan,
        facts: &crate::ownership::TypeFactTable,
    ) -> Self {
        let mut values = BTreeSet::new();
        let mut guaranteed = BTreeSet::new();
        let mut linear_values = BTreeSet::new();
        let mut record = |value, own, ty: &hew_types::ResolvedTy| {
            if facts
                .get(&hew_types::TypeInstanceKey(ty.clone()))
                .is_some_and(|row| row.class == hew_types::ValueClass::Linear)
            {
                linear_values.insert(value);
            }
            if own == OwnKind::Owned {
                values.insert(value);
            } else if own == OwnKind::Guaranteed {
                guaranteed.insert(value);
            }
        };
        for param in &function.params {
            record(param.value, param.own, &param.ty);
        }
        for block in &function.blocks {
            for arg in &block.args {
                record(arg.value, arg.own, &arg.ty);
            }
            for op in &block.ops {
                for result in &op.results {
                    record(result.id, result.own, &result.ty);
                }
            }
            block
                .terminator
                .visit_results(|value| record(value.id, value.own, &value.ty));
        }
        let mut parents = BTreeMap::new();
        let mut local_borrows = BTreeSet::new();
        for op in function.blocks.iter().flat_map(|block| &block.ops) {
            if let Some(parent) = op.kind.borrow_parent() {
                for result in &op.results {
                    parents.insert(result.id, parent);
                    local_borrows.insert(result.id);
                }
            }
        }
        let mut dependents = BTreeMap::<_, Vec<_>>::new();
        for (&value, &base) in &parents {
            dependents
                .entry(base)
                .or_default()
                .push(PlaceBase::Value(value));
        }
        for place in &function.places {
            if let Some(base) = projections.base(place.id) {
                dependents
                    .entry(base)
                    .or_default()
                    .push(PlaceBase::Place(place.id));
            }
        }
        let local_indices = function
            .places
            .iter()
            .filter(|place| place.origin == crate::PlaceOrigin::Local)
            .enumerate()
            .map(|(index, place)| (place.id, index))
            .collect();
        let linear_places = function
            .places
            .iter()
            .filter(|place| {
                facts
                    .get(&hew_types::TypeInstanceKey(place.ty.clone()))
                    .is_some_and(|row| row.class == hew_types::ValueClass::Linear)
            })
            .map(|place| place.id)
            .collect();
        values.extend(&local_borrows);
        guaranteed.extend(&local_borrows);
        let values: Vec<_> = values.into_iter().collect();
        let places: Vec<_> = function
            .places
            .iter()
            .filter_map(|place| match place.origin {
                crate::PlaceOrigin::Capture { environment, .. }
                | crate::PlaceOrigin::ActorState {
                    state: environment, ..
                } => Some((place.id, OwnerRoot::Value(environment))),
                _ => None,
            })
            .chain(
                projections
                    .roots()
                    .flat_map(|(root, leaves)| leaves.iter().map(move |&place| (place, root))),
            )
            .collect();
        let place_indices = places
            .iter()
            .enumerate()
            .map(|(index, (place, _))| (*place, index))
            .collect();
        Self {
            defers: crate::defer::plan(function).unwrap_or_default(),
            blocks: function
                .blocks
                .iter()
                .map(|block| (block.id, block))
                .collect(),
            indices: values
                .iter()
                .enumerate()
                .map(|(index, &value)| (value, index))
                .collect(),
            values,
            guaranteed,
            local_borrows,
            parents,
            dependents,
            local_indices,
            linear_places,
            linear_values,
            cleanup_suffixes: cleanup_suffixes(function),
            places,
            place_indices,
            projections,
        }
    }

    fn access(
        &self,
        block: BlockId,
        value: ValueId,
        consume: bool,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        if consume && self.guaranteed.contains(&value) {
            emit(Violation {
                place: None,
                block,
                value: Some(value),
                reason: "guaranteed input cannot be consumed; copy it into an owned value first",
            });
            return;
        }
        let Some(&index) = self.indices.get(&value) else {
            return;
        };
        if state.values[index] != LIVE {
            emit(Violation {
                place: None,
                block,
                value: Some(value),
                reason: if self.local_borrows.contains(&value) {
                    "borrow is not live on every incoming path"
                } else {
                    "owned value is not live on every incoming path"
                },
            });
        }
        if consume {
            self.require_unreserved(block, PlaceBase::Value(value), state, emit);
            self.require_no_live_borrows(block, PlaceBase::Value(value), state, emit);
            state.values[index] = DEAD;
            for (index, (_, owner)) in self.places.iter().enumerate() {
                if *owner == OwnerRoot::Value(value) {
                    state.places[index] = DEAD;
                }
            }
        }
    }

    fn dependency_parent(&self, base: PlaceBase) -> Option<PlaceBase> {
        match base {
            PlaceBase::Value(value) => self.parents.get(&value).copied(),
            PlaceBase::Place(place) => self.projections.base(place),
        }
    }

    fn depends_on(&self, value: ValueId, ancestor: PlaceBase) -> bool {
        let mut current = PlaceBase::Value(value);
        let mut seen = BTreeSet::new();
        while seen.insert(current) {
            let Some(parent) = self.dependency_parent(current) else {
                return false;
            };
            if parent == ancestor {
                return true;
            }
            current = parent;
        }
        false
    }

    fn any_live_dependent(
        &self,
        base: PlaceBase,
        state: &State,
        accept: impl Fn(ValueId) -> bool,
    ) -> bool {
        let mut pending = vec![base];
        let mut seen = BTreeSet::new();
        while let Some(base) = pending.pop() {
            if !seen.insert(base) {
                continue;
            }
            for &child in self.dependents.get(&base).into_iter().flatten() {
                if let PlaceBase::Value(value) = child {
                    if self.local_borrows.contains(&value)
                        && state.values[self.indices[&value]] & LIVE != 0
                        && accept(value)
                    {
                        return true;
                    }
                }
                pending.push(child);
            }
        }
        false
    }

    fn require_no_live_borrows(
        &self,
        block: BlockId,
        base: PlaceBase,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        if self.any_live_dependent(base, state, |_| true) {
            emit(Violation {
                place: match base {
                    PlaceBase::Place(place) => Some(place),
                    PlaceBase::Value(_) => None,
                },
                block,
                value: match base {
                    PlaceBase::Value(value) => Some(value),
                    PlaceBase::Place(_) => None,
                },
                reason: "value cannot be consumed or ended while a dependent borrow is live",
            });
        }
    }

    fn end_borrow(
        &self,
        block: BlockId,
        value: ValueId,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        if !self.local_borrows.contains(&value) {
            emit(Violation {
                place: None,
                block,
                value: Some(value),
                reason: "end_borrow requires a local borrow producer",
            });
            return;
        }
        self.access(block, value, false, state, emit);
        self.require_no_live_borrows(block, PlaceBase::Value(value), state, emit);
        state.values[self.indices[&value]] = DEAD;
    }

    fn define(
        &self,
        block: BlockId,
        value: ValueId,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        let Some(&index) = self.indices.get(&value) else {
            return;
        };
        if state.values[index] != DEAD {
            emit(Violation {
                place: None,
                block,
                value: Some(value),
                reason: if self.local_borrows.contains(&value) {
                    "previous dynamic borrow remains live at definition"
                } else {
                    "previous dynamic owner remains live at definition"
                },
            });
        }
        state.values[index] = LIVE;
        for (index, (place, owner)) in self.places.iter().enumerate() {
            if *owner == OwnerRoot::Value(value) && self.projections.projection(*place).is_some() {
                state.places[index] = LIVE;
            }
        }
    }

    fn edge(
        &self,
        from: BlockId,
        edge: &Edge,
        mut state: State,
        emit: &mut impl FnMut(Violation),
    ) -> Option<(BlockId, State)> {
        let target = self.blocks.get(&edge.target)?;
        let before = state.places.clone();
        // Consume all sources and define all destinations before installing
        // leaf states. Loop edges may rename, reuse or permute root arguments.
        for argument in &edge.args {
            if self
                .projections
                .leaves(OwnerRoot::Value(argument.value))
                .is_none()
            {
                self.require_complete_environment(from, argument.value, &state, emit);
            }
            self.access(from, argument.value, true, &mut state, emit);
        }
        for argument in &target.args {
            self.define(edge.target, argument.value, &mut state, emit);
        }
        for (source, destination) in edge.args.iter().zip(&target.args) {
            let transfers = self
                .projections
                .transfer(source.value, destination.value)
                .expect("projection query verified every CFG root transfer");
            for (source, destination) in transfers {
                state.places[self.place_indices[&destination]] =
                    before[self.place_indices[&source]];
            }
        }
        Some((edge.target, state))
    }

    #[allow(
        clippy::too_many_lines,
        reason = "keep ownership and fault transitions in one exhaustive terminator match"
    )]
    fn block(
        &self,
        id: BlockId,
        mut state: State,
        cleanup_exit: u8,
        emit: &mut impl FnMut(Violation),
        lifetimes: &mut PlaceLifetimes,
    ) -> Vec<(BlockId, State)> {
        let block = self.blocks[&id];
        if state.defers.invalid_join {
            Self::defer_error(
                id,
                "incompatible defer phases or pending actions at CFG join",
                emit,
            );
        }
        for (entry, region) in &self.defers.regions {
            if region.blocks.contains(&id) && !state.defers.active.iter().any(|f| {
                matches!(self.blocks[entry].terminator, SemTerminator::EnterDefer { defer, .. } if defer == f.defer)
            }) {
                Self::defer_error(id, "defer body entered without its region boundary", emit);
            }
        }
        self.operations(id, &block.ops, &mut state, cleanup_exit, emit, lifetimes);
        self.boundary_inputs(id, &block.terminator, &mut state, emit);
        let mut successors = Vec::new();
        match &block.terminator {
            SemTerminator::EnterDefer { defer, park, body } => {
                if let Some(registration) = self.defers.registrations.get(defer) {
                    let frame = crate::defer::Frame {
                        defer: *defer,
                        scope: registration.scope,
                        park: *park,
                        entry: id,
                        pending_base: state.defers.pending.len().saturating_sub(1),
                        fault: state.fault,
                        exit: state.exit,
                    };
                    if let Err(reason) = state.defers.enter(frame) {
                        Self::defer_error(id, reason, emit);
                    }
                    state.fault = DEAD;
                    // Keep the exit cause while parked: a pre-existing trap
                    // still permits trap-only destruction of linear contents.
                    successors.extend(self.edge(id, body, state, emit));
                }
            }
            SemTerminator::FinishDefer { defer, park, next } => {
                if let Some(frame) = state.defers.active.last() {
                    if let Some(region) = self.defers.regions.get(&frame.entry) {
                        for place in &region.locals {
                            if self
                                .local_indices
                                .get(place)
                                .is_some_and(|i| state.locals[*i] & LIVE != 0)
                            {
                                Self::defer_error(
                                    id,
                                    "defer finish leaves body-local storage active",
                                    emit,
                                );
                            }
                        }
                        for value in &region.values {
                            if self
                                .indices
                                .get(value)
                                .is_some_and(|i| state.values[*i] & LIVE != 0)
                            {
                                Self::defer_error(
                                    id,
                                    "defer finish leaves a body-local owner or loan live",
                                    emit,
                                );
                            }
                        }
                    }
                }
                match state.defers.finish(*defer, *park) {
                    Ok(frame) => {
                        // The result is absent only if BOTH optional carriers
                        // are absent. A join must never forget a first fault.
                        state.fault = combine_fault(frame.fault, state.fault);
                        state.exit = combine_exit(frame.exit, state.exit);
                    }
                    Err(reason) => Self::defer_error(id, reason, emit),
                }
                successors.extend(self.edge(id, next, state, emit));
            }
            SemTerminator::RecoverFault {
                result,
                normal,
                unwind,
                ..
            } => {
                Self::require_fault(id, LIVE, &state, emit);
                let mut recovered = state.clone();
                recovered.fault = DEAD;
                recovered.exit = ORDINARY;
                self.define(id, result.id, &mut recovered, emit);
                successors.extend(self.edge(id, normal, recovered, emit));
                successors.extend(self.edge(id, unwind, state, emit));
            }
            SemTerminator::CleanupDispatch { normal, fault } => {
                if state.fault & DEAD != 0 {
                    let mut success = state.clone();
                    success.fault = DEAD;
                    // An enclosing parked fault retains the exit disposition
                    // of its body even though this nested boundary succeeded.
                    success.exit = success.defers.active.last().map_or(ORDINARY, |f| f.exit);
                    successors.extend(self.edge(id, normal, success, emit));
                }
                if state.fault & LIVE != 0 {
                    state.fault = LIVE;
                    mark_trap(&mut state);
                    successors.extend(self.edge(id, fault, state, emit));
                }
            }
            SemTerminator::CheckedRaiseFault { cleanup, .. }
            | SemTerminator::Panic { cleanup, .. } => {
                Self::require_fault(id, DEAD, &state, emit);
                state.fault = LIVE;
                mark_trap(&mut state);
                successors.extend(self.edge(id, cleanup, state, emit));
            }
            SemTerminator::Call { normal, unwind, .. }
            | SemTerminator::IndirectCall { normal, unwind, .. } => {
                Self::require_fault(id, DEAD, &state, emit);
                let mut returned = state.clone();
                block
                    .terminator
                    .visit_results(|result| self.define(id, result.id, &mut returned, emit));
                if let Some(normal) = normal {
                    successors.extend(self.edge(id, normal, returned, emit));
                }
                if let CallUnwind::Cleanup(edge) = unwind {
                    state.fault = LIVE;
                    mark_trap(&mut state);
                    successors.extend(self.edge(id, edge, state, emit));
                }
            }
            SemTerminator::ActorCall { normal, unwind, .. }
            | SemTerminator::RtCall { normal, unwind, .. }
            | SemTerminator::ValueCall { normal, unwind, .. } => {
                Self::require_fault(id, DEAD, &state, emit);
                let mut returned = state.clone();
                block
                    .terminator
                    .visit_results(|result| self.define(id, result.id, &mut returned, emit));
                successors.extend(self.edge(id, normal, returned, emit));
                if let CallUnwind::Cleanup(edge) = unwind {
                    let transfers_fault = match &block.terminator {
                        SemTerminator::ActorCall { .. }
                        | SemTerminator::Call { .. }
                        | SemTerminator::ValueCall { .. }
                        | SemTerminator::IndirectCall { .. } => true,
                        SemTerminator::RtCall { family, .. } => family
                            .semantic_contract()
                            .is_some_and(hew_types::RuntimeSemanticContract::propagates_fault),
                        _ => unreachable!("matched call terminator"),
                    };
                    if transfers_fault {
                        state.fault = LIVE;
                    }
                    mark_trap(&mut state);
                    successors.extend(self.edge(id, edge, state, emit));
                }
            }
            SemTerminator::CheckedBinary {
                lhs,
                rhs,
                result,
                normal,
                failures,
                ..
            } => successors
                .extend(self.checked_binary(id, lhs, rhs, result, normal, failures, state, emit)),
            SemTerminator::Goto(edge) => successors.extend(self.edge(id, edge, state, emit)),
            SemTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => {
                self.access(id, condition.value, false, &mut state, emit);
                successors.extend(self.edge(id, then_target, state.clone(), emit));
                successors.extend(self.edge(id, else_target, state, emit));
            }
            SemTerminator::SwitchVariant {
                scrutinee, arms, ..
            } => successors.extend(self.variant_switch(id, scrutinee, arms, &state, emit)),
            SemTerminator::Suspend {
                kind: crate::SuspendKind::ValueClose { place },
                resumes,
                ..
            } => {
                if let Some(place) = place {
                    let root = self
                        .projections
                        .projection(*place)
                        .map_or(OwnerRoot::Local(*place), |projection| projection.root);
                    let base = match root {
                        OwnerRoot::Local(root) => {
                            self.require_active(id, root, &state, emit);
                            PlaceBase::Place(root)
                        }
                        OwnerRoot::Value(root) => {
                            self.access(id, root, false, &mut state, emit);
                            PlaceBase::Value(root)
                        }
                    };
                    self.require_no_live_borrows(id, base, &state, emit);
                }
                state.fault = combine_fault(state.fault, DEAD | LIVE);
                state.exit |= TRAP;
                for edge in resumes {
                    successors.extend(self.edge(id, edge, state.clone(), emit));
                }
            }
            SemTerminator::Suspend {
                kind: crate::SuspendKind::Join { cancel: true, .. },
                resumes,
                cancel,
                unwind,
                ..
            } => {
                // A fault drain preserves its primary fault and exit kind;
                // child faults are appended by the physical join operation.
                Self::require_fault(id, LIVE, &state, emit);
                for edge in resumes.iter().chain([cancel, unwind]) {
                    successors.extend(self.edge(id, edge, state.clone(), emit));
                }
            }
            SemTerminator::Suspend {
                resumes,
                cancel,
                unwind,
                ..
            } => {
                Self::require_fault(id, DEAD, &state, emit);
                for (index, edge) in resumes.iter().enumerate() {
                    let mut resumed = state.clone();
                    if index == 0 {
                        block
                            .terminator
                            .visit_results(|result| self.define(id, result.id, &mut resumed, emit));
                    }
                    successors.extend(self.edge(id, edge, resumed, emit));
                }
                let mut failed = state.clone();
                failed.fault = LIVE;
                mark_trap(&mut failed);
                successors.extend(self.edge(id, unwind, failed, emit));
                state.exit = (state.exit & !ORDINARY) | CANCEL;
                state.fault = LIVE;
                successors.extend(self.edge(id, cancel, state, emit));
            }
            SemTerminator::Return { .. }
            | SemTerminator::ResumeUnwind
            | SemTerminator::Trap { .. }
            | SemTerminator::Unreachable => {
                self.exit(id, &block.terminator, &state, emit);
            }
        }
        successors
    }

    fn exit(
        &self,
        id: BlockId,
        terminator: &SemTerminator,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        if !state.defers.pending.is_empty() || !state.defers.active.is_empty() {
            Self::defer_error(id, "exit leaves pending actions or live fault parks", emit);
        }
        for (&place, &index) in &self.local_indices {
            if state.locals[index] & LIVE != 0 {
                emit(Violation {
                    block: id,
                    value: None,
                    place: Some(place),
                    reason: "local storage remains active at exit",
                });
            }
        }
        if state.exit & TRAP != 0
            && !matches!(
                terminator,
                SemTerminator::Trap { .. } | SemTerminator::ResumeUnwind
            )
        {
            emit(Violation {
                block: id,
                value: None,
                place: None,
                reason: "trap cleanup cannot resume ordinary or cancellation execution",
            });
        }
        let expected = if matches!(terminator, SemTerminator::ResumeUnwind) {
            LIVE
        } else {
            DEAD
        };
        Self::require_fault(id, expected, state, emit);
        for (index, &value) in self.values.iter().enumerate() {
            if state.values[index] & LIVE != 0 {
                emit(Violation {
                    place: None,
                    block: id,
                    value: Some(value),
                    reason: if self.local_borrows.contains(&value) {
                        "local borrow remains live at exit"
                    } else {
                        "owned value remains live at exit"
                    },
                });
            }
        }
    }

    fn operations(
        &self,
        id: BlockId,
        operations: &[crate::SemOp],
        state: &mut State,
        cleanup_exit: u8,
        emit: &mut impl FnMut(Violation),
        lifetimes: &mut PlaceLifetimes,
    ) {
        for (index, op) in operations.iter().enumerate() {
            if let SemOpKind::RegisterDefer {
                defer,
                scope,
                dependencies,
            } = &op.kind
            {
                Self::require_fault(id, DEAD, state, emit);
                for place in dependencies {
                    self.require_dependency(id, *place, state, emit);
                }
                if let Err(reason) = state.defers.register(*defer, *scope) {
                    Self::defer_error(id, reason, emit);
                }
            }
            if let SemOpKind::LoadTake { place } | SemOpKind::EndLifetime { place } = &op.kind {
                self.require_unreserved(id, PlaceBase::Place(*place), state, emit);
            }
            let cleanup = if self
                .cleanup_suffixes
                .get(&id)
                .is_some_and(|&start| index >= start)
                && ((cleanup_exit != 0 && cleanup_exit & ORDINARY == 0)
                    || (cleanup_exit == ORDINARY
                        && matches!(self.blocks[&id].terminator, SemTerminator::Trap { .. })))
            {
                CleanupMode::Trap
            } else {
                CleanupMode::Ordinary
            };
            if matches!(
                op.kind,
                SemOpKind::EndLifetime { .. } | SemOpKind::DestroyValue { .. }
            ) {
                lifetimes
                    .operations
                    .entry(op.id)
                    .and_modify(|mode| {
                        if cleanup == CleanupMode::Ordinary {
                            *mode = CleanupMode::Ordinary;
                        }
                    })
                    .or_insert(cleanup);
            }
            self.local_lifetime(id, &op.kind, cleanup, state, emit);
            if let SemOpKind::DestroyValue { value } = &op.kind {
                self.require_droppable_value(id, value.value, cleanup, state, emit);
            }
            if let SemOpKind::EndBorrow { borrow } = &op.kind {
                self.end_borrow(id, borrow.value, state, emit);
                continue;
            }
            self.projected_operation(id, &op.kind, state, emit);
            let consumes = operation_consumes_operands(&op.kind);
            op.visit_operands(|_, operand| {
                if !matches!(
                    op.kind,
                    SemOpKind::DestroyValue { .. } | SemOpKind::LoadBorrow { .. }
                ) {
                    self.require_complete_environment(id, operand.value, state, emit);
                }
                self.access(id, operand.value, consumes, state, emit);
            });
            for result in &op.results {
                self.define(id, result.id, state, emit);
            }
        }
    }

    fn defer_error(block: BlockId, reason: &'static str, emit: &mut impl FnMut(Violation)) {
        emit(Violation {
            block,
            value: None,
            place: None,
            reason,
        });
    }

    fn require_dependency(
        &self,
        block: BlockId,
        place: crate::PlaceId,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        if let Some(projection) = self.projections.projection(place) {
            match projection.root {
                OwnerRoot::Local(root) => self.require_active(block, root, state, emit),
                OwnerRoot::Value(root) => {
                    if self
                        .indices
                        .get(&root)
                        .is_some_and(|i| state.values[*i] != LIVE)
                    {
                        Self::defer_error(block, "defer dependency owner is unavailable", emit);
                    }
                }
            }
            if projection
                .leaves
                .iter()
                .any(|leaf| state.places[self.place_indices[leaf]] != LIVE)
            {
                Self::defer_error(
                    block,
                    "defer dependency is not initialized on every incoming path",
                    emit,
                );
            }
        } else if self
            .place_indices
            .get(&place)
            .is_none_or(|i| state.places[*i] != LIVE)
        {
            Self::defer_error(block, "defer dependency has no available typed place", emit);
        }
    }

    fn require_unreserved(
        &self,
        block: BlockId,
        base: PlaceBase,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        for defer in &state.defers.pending {
            let Some(registration) = self.defers.registrations.get(defer) else {
                continue;
            };
            for dependency in &registration.dependencies {
                let overlap = match base {
                    PlaceBase::Value(value) => {
                        self.places
                            .iter()
                            .any(|(p, root)| *p == *dependency && *root == OwnerRoot::Value(value))
                            || self
                                .projections
                                .projection(*dependency)
                                .is_some_and(|p| p.root == OwnerRoot::Value(value))
                    }
                    PlaceBase::Place(place) => {
                        place == *dependency
                            || match (
                                self.projections.projection(place),
                                self.projections.projection(*dependency),
                            ) {
                                (Some(a), Some(b)) => {
                                    a.root == b.root
                                        && a.leaves.iter().any(|leaf| b.leaves.contains(leaf))
                                }
                                _ => false,
                            }
                    }
                };
                if overlap {
                    Self::defer_error(
                        block,
                        "consume or end would invalidate a pending defer dependency",
                        emit,
                    );
                    return;
                }
            }
        }
    }

    fn require_complete_environment(
        &self,
        block: BlockId,
        value: ValueId,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        if self.places.iter().enumerate().any(|(index, (_, owner))| {
            *owner == OwnerRoot::Value(value) && state.places[index] != LIVE
        }) {
            emit(Violation {
                place: None,
                block,
                value: Some(value),
                reason: if self.projections.leaves(OwnerRoot::Value(value)).is_some() {
                    "partially consumed aggregate cannot be copied, borrowed, invoked or transferred"
                } else {
                    "partially consumed environment cannot be copied, invoked or transferred"
                },
            });
        }
    }

    fn require_active(
        &self,
        block: BlockId,
        place: crate::PlaceId,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        if state.locals[self.local_indices[&place]] != LIVE {
            emit(Violation {
                block,
                value: None,
                place: Some(place),
                reason: "local storage is not active on every incoming path",
            });
        }
    }

    fn require_droppable_places(
        &self,
        block: BlockId,
        place: crate::PlaceId,
        leaves: &[crate::PlaceId],
        mode: CleanupMode,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        if mode == CleanupMode::Ordinary
            && leaves.iter().any(|leaf| {
                self.linear_places.contains(leaf)
                    && state.places[self.place_indices[leaf]] & LIVE != 0
            })
        {
            emit(Violation {
                block,
                value: None,
                place: Some(place),
                reason:
                    "live linear contents require an explicit consume outside trap-only cleanup",
            });
        }
    }

    fn require_droppable_value(
        &self,
        block: BlockId,
        value: ValueId,
        mode: CleanupMode,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        let live_linear = if let Some(leaves) = self.projections.leaves(OwnerRoot::Value(value)) {
            leaves.iter().any(|leaf| {
                self.linear_places.contains(leaf)
                    && state.places[self.place_indices[leaf]] & LIVE != 0
            })
        } else {
            self.linear_values.contains(&value)
        };
        if mode == CleanupMode::Ordinary && live_linear {
            emit(Violation {
                block,
                value: Some(value),
                place: None,
                reason: "linear value requires an explicit consume outside trap-only cleanup",
            });
        }
    }

    fn local_lifetime(
        &self,
        block: BlockId,
        kind: &SemOpKind,
        mode: CleanupMode,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        let (SemOpKind::AllocPlace { place } | SemOpKind::EndLifetime { place }) = *kind else {
            return;
        };
        let Some(&index) = self.local_indices.get(&place) else {
            return;
        };
        let leaves = self
            .projections
            .leaves(OwnerRoot::Local(place))
            .expect("checked local content partition");
        if matches!(kind, SemOpKind::AllocPlace { .. }) {
            if state.locals[index] != DEAD {
                emit(Violation {
                    block,
                    value: None,
                    place: Some(place),
                    reason: "local storage is already active on an incoming path",
                });
            }
            state.locals[index] = LIVE;
        } else {
            self.require_active(block, place, state, emit);
            self.require_no_live_borrows(block, PlaceBase::Place(place), state, emit);
            self.require_droppable_places(block, place, leaves, mode, state, emit);
            state.locals[index] = DEAD;
        }
        for leaf in leaves {
            state.places[self.place_indices[leaf]] = DEAD;
        }
    }

    fn projected_operation(
        &self,
        block: BlockId,
        kind: &SemOpKind,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        let (place, changes) = match kind {
            SemOpKind::LoadCopy { place } | SemOpKind::LoadBorrow { place, .. } => (*place, false),
            SemOpKind::LoadTake { place }
            | SemOpKind::StoreAssign { place, .. }
            | SemOpKind::StoreInit { place, .. } => (*place, true),
            _ => return,
        };
        if let Some(projection) = self.projections.projection(place) {
            let root_value = match projection.root {
                OwnerRoot::Value(root) => {
                    self.access(block, root, false, state, emit);
                    if changes {
                        self.require_no_live_borrows(block, PlaceBase::Value(root), state, emit);
                    }
                    Some(root)
                }
                OwnerRoot::Local(root) => {
                    self.require_active(block, root, state, emit);
                    if changes {
                        self.require_no_live_borrows(block, PlaceBase::Place(root), state, emit);
                    }
                    None
                }
            };
            if matches!(kind, SemOpKind::StoreAssign { .. }) {
                self.require_droppable_places(
                    block,
                    place,
                    &projection.leaves,
                    CleanupMode::Ordinary,
                    state,
                    emit,
                );
            }
            let stores = matches!(
                kind,
                SemOpKind::StoreAssign { .. } | SemOpKind::StoreInit { .. }
            );
            for leaf in &projection.leaves {
                let index = self.place_indices[leaf];
                let expected = if matches!(kind, SemOpKind::StoreInit { .. }) {
                    Some(DEAD)
                } else if stores {
                    None
                } else {
                    Some(LIVE)
                };
                if expected.is_some_and(|expected| state.places[index] != expected) {
                    emit(Violation {
                        place: root_value.is_none().then_some(place),
                        block,
                        value: root_value,
                        reason: if stores {
                            "aggregate field initialization would overwrite a live value"
                        } else {
                            "aggregate field is not initialized on every incoming path"
                        },
                    });
                }
                if stores {
                    state.places[index] = LIVE;
                } else if matches!(kind, SemOpKind::LoadTake { .. }) {
                    state.places[index] = DEAD;
                }
            }
            return;
        }
        // Capture semantics remain unchanged: assignment requires a live
        // private mutable capture; it cannot restore a consumed capture.
        let Some(&index) = self.place_indices.get(&place) else {
            return;
        };
        let (_, OwnerRoot::Value(owner)) = self.places[index] else {
            unreachable!("only capture places have no local or aggregate selection")
        };
        self.access(block, owner, false, state, emit);
        if state.places[index] != LIVE {
            emit(Violation {
                place: None,
                block,
                value: Some(owner),
                reason: "capture field is not initialized on every incoming path",
            });
        }
        if changes {
            self.require_no_live_borrows(block, PlaceBase::Value(owner), state, emit);
        }
        if matches!(kind, SemOpKind::LoadTake { .. }) {
            state.places[index] = DEAD;
        }
    }

    fn borrow_root(&self, value: ValueId) -> PlaceBase {
        let mut current = PlaceBase::Value(value);
        let mut seen = BTreeSet::new();
        while seen.insert(current) {
            let Some(parent) = self.dependency_parent(current) else {
                break;
            };
            current = parent;
        }
        current
    }

    fn require_exclusive(
        &self,
        block: BlockId,
        value: ValueId,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        self.require_no_live_borrows(block, PlaceBase::Value(value), state, emit);
        let root = self.borrow_root(value);
        if self.any_live_dependent(root, state, |borrow| {
            borrow != value && !self.depends_on(value, PlaceBase::Value(borrow))
        }) {
            emit(Violation {
                place: None,
                block,
                value: Some(value),
                reason: "exclusive receiver has another live loan of its owner",
            });
        }
    }

    fn require_fault(
        block: BlockId,
        expected: u8,
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) {
        if state.fault != expected {
            emit(Violation {
                place: None,
                block,
                value: None,
                reason: if expected == LIVE {
                    "fault propagation requires an active fault on every incoming path"
                } else {
                    "active fault cannot be abandoned or overwritten"
                },
            });
        }
    }

    fn variant_switch(
        &self,
        id: BlockId,
        scrutinee: &crate::Operand,
        arms: &[crate::SemVariantArm],
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) -> Vec<(BlockId, State)> {
        let mut successors = Vec::with_capacity(arms.len());
        for arm in arms {
            let mut arm_state = state.clone();
            self.access(id, scrutinee.value, true, &mut arm_state, emit);
            for field in &arm.fields {
                self.define(id, field.id, &mut arm_state, emit);
            }
            successors.extend(self.edge(id, &arm.target, arm_state, emit));
        }
        successors
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "the transfer receives one closed checked-binary terminator shape"
    )]
    fn checked_binary(
        &self,
        id: BlockId,
        lhs: &crate::Operand,
        rhs: &crate::Operand,
        result: &crate::ValueDef,
        normal: &Edge,
        failures: &[crate::CheckedFailure],
        mut state: State,
        emit: &mut impl FnMut(Violation),
    ) -> Vec<(BlockId, State)> {
        self.access(id, lhs.value, false, &mut state, emit);
        self.access(id, rhs.value, false, &mut state, emit);
        let mut succeeded = state.clone();
        self.define(id, result.id, &mut succeeded, emit);
        let mut successors = Vec::new();
        successors.extend(self.edge(id, normal, succeeded, emit));
        mark_trap(&mut state);
        for failure in failures {
            successors.extend(self.edge(id, &failure.edge, state.clone(), emit));
        }
        successors
    }

    fn boundary_inputs(
        &self,
        id: BlockId,
        terminator: &SemTerminator,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        let mut roots = BTreeMap::<_, bool>::new();
        terminator.visit_boundary_operands(|_, operand| {
            let value = operand.operand.value;
            if matches!(
                terminator,
                SemTerminator::Suspend {
                    kind: crate::SuspendKind::ValueClose { .. },
                    ..
                }
            ) {
                // Closing visits only initialized captures and retains the
                // carrier until ordinary destruction. It never copies or
                // invokes the partially consumed environment.
                self.require_no_live_borrows(id, PlaceBase::Value(value), state, emit);
            } else {
                self.require_complete_environment(id, value, state, emit);
            }
            let exclusive = operand.decision == BoundaryDecision::BorrowMut;
            if exclusive {
                self.require_exclusive(id, value, state, emit);
            }
            let root = self.borrow_root(value);
            if roots
                .get(&root)
                .is_some_and(|previous| *previous || exclusive)
            {
                emit(Violation {
                    place: None,
                    block: id,
                    value: Some(value),
                    reason: "exclusive receiver aliases another call operand",
                });
            }
            roots
                .entry(root)
                .and_modify(|previous| *previous |= exclusive)
                .or_insert(exclusive);
            if self.guaranteed.contains(&value) {
                // Calls prove a non-retaining borrow. Selection retains its
                // own observation references before parking and releases them
                // on every exit; it never transfers the borrowed task result.
                // Native I/O copies submission inputs or proves producer
                // quiescence before its input-resource loans can end.
                let scoped_borrow = matches!(
                    terminator,
                    SemTerminator::Suspend {
                        kind: crate::SuspendKind::GeneratorNext,
                        ..
                    } | SemTerminator::ActorCall { .. }
                        | SemTerminator::Call { .. }
                        | SemTerminator::RtCall { .. }
                        | SemTerminator::ValueCall { .. }
                        | SemTerminator::IndirectCall { .. }
                        | SemTerminator::Panic { .. }
                        | SemTerminator::Suspend {
                            kind: crate::SuspendKind::Select { .. }
                                | crate::SuspendKind::NativeIo { .. },
                            ..
                        }
                ) && matches!(
                    operand.decision,
                    BoundaryDecision::Borrow | BoundaryDecision::BorrowMut
                );
                if !scoped_borrow {
                    emit(Violation {
                        place: None,
                        block: id,
                        value: Some(value),
                        reason: "guaranteed input requires an explicit owned copy at this boundary",
                    });
                }
                self.access(id, value, false, state, emit);
                return;
            }
            let consumes = matches!(
                operand.decision,
                BoundaryDecision::Move | BoundaryDecision::Snapshot(SnapshotDecision::Transfer)
            );
            self.access(id, value, consumes, state, emit);
        });
    }
}

fn operation_consumes_operands(kind: &SemOpKind) -> bool {
    matches!(
        kind,
        SemOpKind::GeneratorMake { .. }
            | SemOpKind::TaskSpawn { .. }
            | SemOpKind::ClosureMake { .. }
            | SemOpKind::CallableCoerce { .. }
            | SemOpKind::TupleMake { .. }
            | SemOpKind::AggregateMake { .. }
            | SemOpKind::VariantMake { .. }
            | SemOpKind::DestroyValue { .. }
            | SemOpKind::Move { .. }
            | SemOpKind::Fork { .. }
            | SemOpKind::Destructure { .. }
            | SemOpKind::StoreInit { .. }
            | SemOpKind::StoreAssign { .. }
    )
}

#[cfg(test)]
mod tests {
    fn verify(function: &crate::SemFunction) -> Vec<super::Violation> {
        super::verify(
            function,
            &crate::place_plan(function, &[], &std::collections::BTreeMap::default()).unwrap(),
            &crate::ownership::TypeFactTable::new(),
        )
        .violations
    }
    use crate::{
        BlockArg, BlockId, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind, CallableId,
        Edge, FunctionSourceOrigin, OpId, Operand, OwnKind, Provenance, SemBlock, SemFunction,
        SemOp, SemOpKind, SemTerminator, StringLiteralId, ValueDef, ValueId,
    };
    use hew_hir::ItemId;
    use hew_types::{DefId, ResolvedTy};

    fn operand(id: u32) -> Operand {
        Operand { value: ValueId(id) }
    }

    fn owned(id: u32) -> ValueDef {
        ValueDef {
            id: ValueId(id),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        }
    }

    fn boundary(id: u32) -> BoundaryOperand {
        BoundaryOperand {
            operand: operand(id),
            decision: BoundaryDecision::Move,
        }
    }

    fn edge(target: u32, args: &[u32]) -> Edge {
        Edge {
            target: BlockId(target),
            args: args.iter().copied().map(operand).collect(),
        }
    }

    fn op(id: u32, kind: SemOpKind, results: Vec<ValueDef>) -> SemOp {
        SemOp {
            id: OpId(id),
            kind,
            results,
            provenance: Provenance::Synthesized,
        }
    }

    fn destroy(id: u32, value: u32) -> SemOp {
        op(
            id,
            SemOpKind::DestroyValue {
                value: operand(value),
            },
            Vec::new(),
        )
    }

    fn block(id: u32, ops: Vec<SemOp>, terminator: SemTerminator) -> SemBlock {
        SemBlock {
            id: BlockId(id),
            args: Vec::new(),
            ops,
            terminator,
        }
    }

    fn function(blocks: Vec<SemBlock>) -> SemFunction {
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("lifetime"),
            name: "lifetime".into(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![
                BlockArg {
                    value: ValueId(0),
                    ty: ResolvedTy::String,
                    own: OwnKind::Owned,
                },
                BlockArg {
                    value: ValueId(99),
                    ty: ResolvedTy::Bool,
                    own: OwnKind::None,
                },
            ],
            return_ty: ResolvedTy::Tuple(Vec::new()),
            entry: BlockId(0),
            blocks,
            places: Vec::new(),
            bindings: Vec::new(),
        }
    }

    fn done() -> SemTerminator {
        SemTerminator::Return { value: None }
    }

    fn begin_borrow(id: u32, owner: u32, loan: u32) -> SemOp {
        op(
            id,
            SemOpKind::BeginBorrow {
                owner: operand(owner),
            },
            vec![ValueDef {
                own: OwnKind::Guaranteed,
                ..owned(loan)
            }],
        )
    }

    fn end_borrow(id: u32, loan: u32) -> SemOp {
        op(
            id,
            SemOpKind::EndBorrow {
                borrow: operand(loan),
            },
            vec![],
        )
    }

    fn cleanup_analysis(blocks: Vec<SemBlock>) -> super::Analysis {
        let mut f = function(blocks);
        let mut facts = hew_types::TypeFactService::new(
            hew_types::TypeFactContext::default(),
            std::collections::BTreeMap::new(),
        );
        facts.require(&ResolvedTy::String).unwrap();
        f.params.push(BlockArg {
            value: ValueId(98),
            ty: ResolvedTy::I64,
            own: OwnKind::None,
        });
        f.places = vec![crate::PlaceDecl {
            id: crate::PlaceId(0),
            ty: ResolvedTy::String,
            origin: crate::PlaceOrigin::Local,
        }];
        let mut entry = vec![
            op(
                20,
                SemOpKind::AllocPlace {
                    place: crate::PlaceId(0),
                },
                vec![],
            ),
            op(
                21,
                SemOpKind::StoreInit {
                    place: crate::PlaceId(0),
                    value: operand(0),
                },
                vec![],
            ),
        ];
        entry.append(&mut f.blocks[0].ops);
        f.blocks[0].ops = entry;
        let rows = facts.into_rows();
        let plan = crate::place_plan(&f, &[], &rows).unwrap();
        super::verify(&f, &plan, &rows)
    }

    fn local_end() -> SemOp {
        op(
            22,
            SemOpKind::EndLifetime {
                place: crate::PlaceId(0),
            },
            vec![],
        )
    }

    fn trap_endpoint() -> SemTerminator {
        SemTerminator::Trap {
            kind: crate::TrapKind::IntegerOverflow,
        }
    }

    fn checked_cleanup(normal: u32, failure: u32) -> SemTerminator {
        SemTerminator::CheckedBinary {
            id: OpId(23),
            op: hew_parser::ast::BinaryOp::Add,
            lhs: operand(98),
            rhs: operand(98),
            result: ValueDef {
                id: ValueId(2),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
            },
            normal: edge(normal, &[]),
            failures: vec![crate::CheckedFailure {
                kind: crate::TrapKind::IntegerOverflow,
                edge: edge(failure, &[]),
            }],
        }
    }

    #[test]
    fn cancellation_and_mixed_predecessors_never_certify_a_trap_cleanup() {
        // Cancellation owns a fault but does not certify trap-only disposal.
        // A mixed success/cancellation merge must preserve that distinction.
        for mixed in [false, true] {
            let analysis = cleanup_analysis(vec![
                block(
                    0,
                    vec![],
                    SemTerminator::Suspend {
                        kind: crate::SuspendKind::Await,
                        inputs: vec![],
                        result: CallResult::Unit,
                        resumes: vec![edge(if mixed { 1 } else { 2 }, &[])],
                        cancel: edge(1, &[]),
                        unwind: edge(3, &[]),
                    },
                ),
                block(
                    1,
                    vec![local_end()],
                    SemTerminator::CleanupDispatch {
                        normal: edge(4, &[]),
                        fault: edge(5, &[]),
                    },
                ),
                block(
                    2,
                    vec![op(
                        24,
                        SemOpKind::EndLifetime {
                            place: crate::PlaceId(0),
                        },
                        vec![],
                    )],
                    done(),
                ),
                block(
                    3,
                    vec![op(
                        25,
                        SemOpKind::EndLifetime {
                            place: crate::PlaceId(0),
                        },
                        vec![],
                    )],
                    SemTerminator::ResumeUnwind,
                ),
                block(4, vec![], done()),
                block(5, vec![], SemTerminator::ResumeUnwind),
            ]);
            assert!(analysis.violations.is_empty(), "{:?}", analysis.violations);
            assert_eq!(
                analysis.lifetimes.cleanup(OpId(22)),
                Some(super::CleanupMode::Ordinary)
            );
        }
        let analysis = cleanup_analysis(vec![
            block(0, vec![], checked_cleanup(1, 1)),
            block(1, vec![local_end()], trap_endpoint()),
        ]);
        assert_eq!(
            analysis.lifetimes.cleanup(OpId(22)),
            Some(super::CleanupMode::Ordinary)
        );
    }

    #[test]
    fn a_trap_cleanup_region_must_finish_without_normal_exit_or_escaping_cycle() {
        let prefix = || {
            vec![
                block(0, vec![], checked_cleanup(1, 2)),
                block(
                    1,
                    vec![op(
                        24,
                        SemOpKind::EndLifetime {
                            place: crate::PlaceId(0),
                        },
                        vec![],
                    )],
                    done(),
                ),
                block(2, vec![local_end()], SemTerminator::Goto(edge(3, &[]))),
                block(3, vec![], trap_endpoint()),
            ]
        };
        let analysis = cleanup_analysis(prefix());
        assert!(analysis.violations.is_empty(), "{:?}", analysis.violations);
        assert_eq!(
            analysis.lifetimes.cleanup(OpId(22)),
            Some(super::CleanupMode::Trap)
        );
        for terminal in [
            done(),
            SemTerminator::Unreachable,
            SemTerminator::Branch {
                condition: operand(99),
                then_target: edge(3, &[]),
                else_target: edge(4, &[]),
            },
        ] {
            let mut blocks = prefix();
            blocks[3].terminator = terminal;
            blocks.push(block(4, vec![], trap_endpoint()));
            let analysis = cleanup_analysis(blocks);
            assert_eq!(
                analysis.lifetimes.cleanup(OpId(22)),
                Some(super::CleanupMode::Ordinary)
            );
        }
    }

    #[test]
    fn a_local_loan_can_make_an_independent_owned_copy() {
        let f = function(vec![block(
            0,
            vec![
                begin_borrow(0, 0, 1),
                op(
                    1,
                    SemOpKind::CopyValue { source: operand(1) },
                    vec![owned(2)],
                ),
                end_borrow(2, 1),
                destroy(3, 0),
                destroy(4, 2),
            ],
            done(),
        )]);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
    }

    #[test]
    fn a_loop_creates_and_ends_each_dynamic_loan_before_its_backedge() {
        let f = function(vec![
            block(0, vec![], SemTerminator::Goto(edge(1, &[]))),
            block(
                1,
                vec![begin_borrow(0, 0, 1), end_borrow(1, 1)],
                SemTerminator::Branch {
                    condition: operand(99),
                    then_target: edge(1, &[]),
                    else_target: edge(2, &[]),
                },
            ),
            block(2, vec![destroy(2, 0)], done()),
        ]);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
        let mut missing_end = f;
        missing_end.blocks[1].ops.pop();
        assert!(verify(&missing_end).iter().any(|violation| {
            violation.reason == "previous dynamic borrow remains live at definition"
        }));
    }

    #[test]
    fn a_local_loan_cannot_escape_through_a_return_boundary() {
        for decision in [
            BoundaryDecision::Borrow,
            BoundaryDecision::BorrowMut,
            BoundaryDecision::Copy,
            BoundaryDecision::Move,
        ] {
            let f = function(vec![block(
                0,
                vec![begin_borrow(0, 0, 1)],
                SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: operand(1),
                        decision,
                    }),
                },
            )]);
            assert!(verify(&f).iter().any(|violation| {
                violation.value == Some(ValueId(1))
                    && violation.reason
                        == "guaranteed input requires an explicit owned copy at this boundary"
            }));
        }
    }

    #[test]
    fn a_parameter_is_not_a_local_loan_that_the_callee_can_end() {
        let mut f = function(vec![block(0, vec![end_borrow(0, 0)], done())]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).iter().any(|violation| {
            violation.reason == "end_borrow requires a local borrow producer"
        }));
    }

    #[test]
    fn indirect_receivers_preserve_or_transfer_ownership_on_both_outcomes() {
        for decision in [
            BoundaryDecision::Borrow,
            BoundaryDecision::BorrowMut,
            BoundaryDecision::Move,
        ] {
            let cleanup = if decision == BoundaryDecision::Move {
                vec![]
            } else {
                vec![destroy(0, 0)]
            };
            let mut f = function(vec![
                block(
                    0,
                    vec![],
                    SemTerminator::IndirectCall {
                        id: OpId(0),
                        callee: BoundaryOperand {
                            operand: operand(0),
                            decision,
                        },
                        signature: crate::SemSignature {
                            params: vec![],
                            return_ty: ResolvedTy::Unit,
                        },
                        args: vec![],
                        result: CallResult::Unit,
                        normal: Some(edge(1, &[])),
                        unwind: CallUnwind::Cleanup(edge(2, &[])),
                    },
                ),
                block(1, cleanup.clone(), done()),
                block(2, cleanup, SemTerminator::ResumeUnwind),
            ]);
            f.params[0].ty = ResolvedTy::Function {
                capabilities: hew_types::CallableCapabilities {
                    call: match decision {
                        BoundaryDecision::BorrowMut => hew_types::CallableCallMode::Var,
                        BoundaryDecision::Move => hew_types::CallableCallMode::Once,
                        _ => hew_types::CallableCallMode::Read,
                    },
                    clone: false,
                },
                params: vec![],
                ret: Box::new(ResolvedTy::Unit),
            };
            assert!(verify(&f).is_empty(), "{decision:?}: {:?}", verify(&f));
            if decision == BoundaryDecision::Move {
                for continuation in [1, 2] {
                    let mut reused = f.clone();
                    reused.blocks[continuation].ops.push(destroy(1, 0));
                    assert!(verify(&reused)
                        .iter()
                        .any(|violation| violation.value == Some(ValueId(0))));
                }
            } else {
                f.blocks[2].ops.clear();
                assert!(verify(&f)
                    .iter()
                    .any(|violation| violation.value == Some(ValueId(0))));
            }
        }
    }

    #[test]
    fn consumed_parameter_has_no_remaining_obligation() {
        assert!(verify(&function(vec![block(0, vec![destroy(0, 0)], done())])).is_empty());
    }

    #[test]
    fn borrowed_parameter_cannot_be_consumed() {
        let mut f = function(vec![block(0, vec![destroy(0, 0)], done())]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).iter().any(|v| v.value == Some(ValueId(0))));
    }

    #[test]
    fn borrowed_parameter_requires_explicit_copy_before_return() {
        for decision in [
            BoundaryDecision::Move,
            BoundaryDecision::Borrow,
            BoundaryDecision::BorrowMut,
            BoundaryDecision::Copy,
        ] {
            let mut f = function(vec![block(
                0,
                Vec::new(),
                SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: operand(0),
                        decision,
                    }),
                },
            )]);
            f.params[0].own = OwnKind::Guaranteed;
            assert!(
                verify(&f).iter().any(|v| v.value == Some(ValueId(0))),
                "{decision:?}"
            );
        }
    }

    #[test]
    fn borrowed_parameter_can_create_an_owned_copy() {
        let mut f = function(vec![block(
            0,
            vec![
                op(
                    0,
                    SemOpKind::CopyValue { source: operand(0) },
                    vec![owned(1)],
                ),
                destroy(1, 1),
            ],
            done(),
        )]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).is_empty());
    }

    #[test]
    fn borrowed_parameter_can_be_borrowed_by_a_call() {
        let mut f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Call {
                    id: OpId(0),
                    callee: CallableId(1),
                    args: vec![BoundaryOperand {
                        operand: operand(0),
                        decision: BoundaryDecision::Borrow,
                    }],
                    result: CallResult::Unit,
                    normal: Some(edge(1, &[])),
                    unwind: CallUnwind::Cleanup(edge(2, &[])),
                },
            ),
            block(1, Vec::new(), done()),
            block(2, Vec::new(), SemTerminator::ResumeUnwind),
        ]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).is_empty());
    }

    #[test]
    fn borrowed_parameter_cannot_be_transferred_into_an_owned_block_argument() {
        let mut continuation = block(1, vec![destroy(1, 1)], done());
        continuation.args.push(BlockArg {
            value: ValueId(1),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        });
        let mut f = function(vec![
            block(0, Vec::new(), SemTerminator::Goto(edge(1, &[0]))),
            continuation,
        ]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).iter().any(|v| v.value == Some(ValueId(0))));
    }

    #[test]
    fn return_transfers_the_owned_result() {
        let mut f = function(vec![block(
            0,
            Vec::new(),
            SemTerminator::Return {
                value: Some(boundary(0)),
            },
        )]);
        f.return_ty = ResolvedTy::String;
        assert!(verify(&f).is_empty());
    }

    #[test]
    fn nonterminating_loop_may_keep_its_owner_live() {
        let f = function(vec![
            block(0, Vec::new(), SemTerminator::Goto(edge(1, &[]))),
            block(
                1,
                vec![
                    op(
                        0,
                        SemOpKind::CopyValue { source: operand(0) },
                        vec![owned(1)],
                    ),
                    destroy(1, 1),
                ],
                SemTerminator::Goto(edge(1, &[])),
            ),
        ]);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
    }

    #[test]
    fn duplicate_owner_on_one_edge_is_rejected() {
        let mut exit = block(1, vec![destroy(0, 1), destroy(1, 2)], done());
        for value in [1, 2] {
            exit.args.push(BlockArg {
                value: ValueId(value),
                ty: ResolvedTy::String,
                own: OwnKind::Owned,
            });
        }
        let errors = verify(&function(vec![
            block(0, Vec::new(), SemTerminator::Goto(edge(1, &[0, 0]))),
            exit,
        ]));
        assert!(
            errors.iter().any(|e| e.value == Some(ValueId(0))
                && e.reason == "owned value is not live on every incoming path"),
            "{errors:?}"
        );
    }

    #[test]
    fn parameter_live_at_return_is_rejected() {
        let errors = verify(&function(vec![block(0, Vec::new(), done())]));
        assert!(
            errors
                .iter()
                .any(|e| e.value == Some(ValueId(0))
                    && e.reason == "owned value remains live at exit"),
            "{errors:?}"
        );
        let diagnostics = crate::verify_function(&function(vec![block(0, Vec::new(), done())]));
        assert!(
            diagnostics.iter().any(|diagnostic| matches!(
                diagnostic.kind,
                crate::SirDiagnosticKind::OwnershipLifetime {
                    value: ValueId(0),
                    ..
                }
            )),
            "{diagnostics:?}"
        );
    }

    #[test]
    fn repeated_consumption_is_rejected() {
        let errors = verify(&function(vec![block(
            0,
            vec![destroy(0, 0), destroy(1, 0)],
            done(),
        )]));
        assert!(
            errors
                .iter()
                .any(|e| e.reason == "owned value is not live on every incoming path"),
            "{errors:?}"
        );
    }

    #[test]
    fn each_branch_may_consume_the_same_incoming_owner() {
        let f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Branch {
                    condition: operand(99),
                    then_target: edge(1, &[]),
                    else_target: edge(2, &[]),
                },
            ),
            block(1, vec![destroy(0, 0)], done()),
            block(2, vec![destroy(1, 0)], done()),
        ]);
        assert!(verify(&f).is_empty());
    }

    #[test]
    fn consumed_on_one_predecessor_cannot_be_read_after_join() {
        let f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Branch {
                    condition: operand(99),
                    then_target: edge(1, &[]),
                    else_target: edge(2, &[]),
                },
            ),
            block(1, vec![destroy(0, 0)], SemTerminator::Goto(edge(3, &[]))),
            block(2, Vec::new(), SemTerminator::Goto(edge(3, &[]))),
            block(3, vec![destroy(1, 0)], done()),
        ]);
        let errors = verify(&f);
        assert!(
            errors
                .iter()
                .any(|e| e.block == BlockId(3) && e.value == Some(ValueId(0))),
            "{errors:?}"
        );
    }

    #[test]
    fn loop_local_definition_is_a_new_dynamic_owner() {
        let mut f = function(vec![
            block(0, vec![destroy(0, 0)], SemTerminator::Goto(edge(1, &[]))),
            block(
                1,
                vec![
                    op(1, SemOpKind::ConstStr(StringLiteralId(0)), vec![owned(1)]),
                    destroy(2, 1),
                ],
                SemTerminator::Branch {
                    condition: operand(99),
                    then_target: edge(1, &[]),
                    else_target: edge(2, &[]),
                },
            ),
            block(2, Vec::new(), done()),
        ]);
        assert!(verify(&f).is_empty());
        f.blocks[1].ops.pop();
        let errors = verify(&f);
        assert!(
            errors.iter().any(|e| e.value == Some(ValueId(1))
                && e.reason == "previous dynamic owner remains live at definition"),
            "{errors:?}"
        );
    }

    #[test]
    fn loop_argument_transfers_before_rebinding_itself() {
        let mut body = block(
            1,
            Vec::new(),
            SemTerminator::Branch {
                condition: operand(99),
                then_target: edge(1, &[1]),
                else_target: edge(2, &[1]),
            },
        );
        body.args.push(BlockArg {
            value: ValueId(1),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        });
        let mut exit = block(2, vec![destroy(0, 2)], done());
        exit.args.push(BlockArg {
            value: ValueId(2),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        });
        assert!(verify(&function(vec![
            block(0, Vec::new(), SemTerminator::Goto(edge(1, &[0]))),
            body,
            exit
        ]))
        .is_empty());
    }

    #[test]
    fn call_moves_apply_to_both_exits_but_results_only_to_normal_exit() {
        let f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Call {
                    id: OpId(0),
                    callee: CallableId(1),
                    args: vec![boundary(0)],
                    result: CallResult::Value(owned(1)),
                    normal: Some(edge(1, &[])),
                    unwind: CallUnwind::Cleanup(edge(2, &[])),
                },
            ),
            block(1, vec![destroy(1, 1)], done()),
            block(2, Vec::new(), SemTerminator::ResumeUnwind),
        ]);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
        let mut invalid = f;
        invalid.blocks[2].ops.push(destroy(2, 0));
        let errors = verify(&invalid);
        assert!(
            errors
                .iter()
                .any(|e| e.block == BlockId(2) && e.value == Some(ValueId(0))),
            "{errors:?}"
        );
    }

    fn scalar_call_fault_flow() -> SemFunction {
        let mut f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Call {
                    id: OpId(0),
                    callee: CallableId(1),
                    args: Vec::new(),
                    result: CallResult::Unit,
                    normal: Some(edge(1, &[])),
                    unwind: CallUnwind::Cleanup(edge(2, &[])),
                },
            ),
            block(1, Vec::new(), done()),
            block(2, Vec::new(), SemTerminator::ResumeUnwind),
        ]);
        f.params.clear();
        f
    }

    #[test]
    fn scalar_call_fault_must_be_propagated_exactly_once() {
        let f = scalar_call_fault_flow();
        assert!(verify(&f).is_empty());

        for terminal in [
            done(),
            SemTerminator::Unreachable,
            SemTerminator::Trap {
                kind: crate::TrapKind::IndexOutOfBounds,
            },
        ] {
            let mut invalid = f.clone();
            invalid.blocks[2].terminator = terminal;
            assert!(verify(&invalid).iter().any(|v| v.value.is_none()
                && v.reason == "active fault cannot be abandoned or overwritten"));
        }

        let mut invalid = f.clone();
        invalid.blocks[1].terminator = SemTerminator::ResumeUnwind;
        assert!(verify(&invalid).iter().any(|v| v.block == BlockId(1)
            && v.reason == "fault propagation requires an active fault on every incoming path"));

        let mut invalid = f.clone();
        invalid.blocks[2].terminator = f.blocks[0].terminator.clone();
        assert!(verify(&invalid).iter().any(|v| v.block == BlockId(2)
            && v.reason == "active fault cannot be abandoned or overwritten"));
    }

    #[test]
    fn joined_fault_state_requires_all_predecessors_to_own_a_fault() {
        let mut f = scalar_call_fault_flow();
        f.blocks[1].terminator = SemTerminator::Goto(edge(2, &[]));
        assert!(verify(&f).iter().any(|v| v.block == BlockId(2)
            && v.reason == "fault propagation requires an active fault on every incoming path"));
    }
    fn captured_field(function: &mut SemFunction) {
        function.places.push(crate::PlaceDecl {
            id: crate::PlaceId(0),
            ty: ResolvedTy::String,
            origin: crate::PlaceOrigin::Capture {
                environment: ValueId(0),
                field: 0,
            },
        });
    }

    fn take_capture(id: u32, value: u32) -> SemOp {
        op(
            id,
            SemOpKind::LoadTake {
                place: crate::PlaceId(0),
            },
            vec![owned(value)],
        )
    }

    #[test]
    fn consuming_a_capture_on_one_branch_still_allows_environment_cleanup() {
        let mut f = function(vec![
            block(
                0,
                vec![],
                SemTerminator::Branch {
                    condition: operand(99),
                    then_target: edge(1, &[]),
                    else_target: edge(2, &[]),
                },
            ),
            block(
                1,
                vec![take_capture(0, 1), destroy(1, 1)],
                SemTerminator::Goto(edge(3, &[])),
            ),
            block(2, vec![], SemTerminator::Goto(edge(3, &[]))),
            block(3, vec![destroy(2, 0)], done()),
        ]);
        captured_field(&mut f);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
        let mut read_after_join = f.clone();
        read_after_join.blocks[3].ops.insert(0, take_capture(3, 2));
        read_after_join.blocks[3].ops.push(destroy(4, 2));
        assert!(verify(&read_after_join)
            .iter()
            .any(|v| v.reason == "capture field is not initialized on every incoming path"));
        let mut copy_after_join = f;
        copy_after_join.blocks[3].ops.insert(
            0,
            op(
                3,
                SemOpKind::CopyValue { source: operand(0) },
                vec![owned(2)],
            ),
        );
        copy_after_join.blocks[3].ops.push(destroy(4, 2));
        assert!(verify(&copy_after_join).iter().any(|v| v.reason
            == "partially consumed environment cannot be copied, invoked or transferred"));
    }

    #[test]
    fn captured_loan_protects_its_field_until_it_ends() {
        let loan = op(
            0,
            SemOpKind::LoadBorrow {
                place: crate::PlaceId(0),
            },
            vec![ValueDef {
                own: OwnKind::Guaranteed,
                ..owned(1)
            }],
        );
        let mut f = function(vec![block(
            0,
            vec![
                loan,
                end_borrow(1, 1),
                take_capture(2, 2),
                destroy(3, 2),
                destroy(4, 0),
            ],
            done(),
        )]);
        captured_field(&mut f);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
        f.blocks[0].ops.swap(1, 2);
        assert!(verify(&f)
            .iter()
            .any(|v| v.reason
                == "value cannot be consumed or ended while a dependent borrow is live"));
    }

    #[test]
    fn exclusive_indirect_receiver_cannot_overlap_a_second_loan_or_argument() {
        let call = SemTerminator::IndirectCall {
            id: OpId(2),
            callee: BoundaryOperand {
                operand: operand(1),
                decision: BoundaryDecision::BorrowMut,
            },
            signature: crate::SemSignature {
                params: vec![],
                return_ty: ResolvedTy::Unit,
            },
            args: vec![],
            result: CallResult::Unit,
            normal: Some(edge(1, &[])),
            unwind: CallUnwind::Cleanup(edge(2, &[])),
        };
        let cleanup = vec![end_borrow(3, 1), destroy(4, 0)];
        let f = function(vec![
            block(0, vec![begin_borrow(0, 0, 1)], call),
            block(1, cleanup.clone(), done()),
            block(2, cleanup, SemTerminator::ResumeUnwind),
        ]);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
        let mut second_loan = f.clone();
        second_loan.blocks[0].ops.push(begin_borrow(1, 0, 2));
        for index in [1, 2] {
            second_loan.blocks[index].ops.insert(0, end_borrow(5, 2));
        }
        assert!(verify(&second_loan)
            .iter()
            .any(|v| v.reason == "exclusive receiver has another live loan of its owner"));
        let mut argument_alias = f;
        if let SemTerminator::IndirectCall { args, .. } = &mut argument_alias.blocks[0].terminator {
            args.push(BoundaryOperand {
                operand: operand(0),
                decision: BoundaryDecision::Borrow,
            });
        }
        assert!(verify(&argument_alias)
            .iter()
            .any(|v| v.reason == "exclusive receiver aliases another call operand"));
    }
}
