//! Structured execution of inline deferred bodies. These are scheduling and
//! fault-carrier facts; ordinary lifetime flow remains the ownership authority.

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    BlockId, DeferId, DeferScopeId, FaultParkId, PlaceId, SemFunction, SemOpKind, SemTerminator,
    ValueId,
};

#[derive(Debug)]
pub(crate) struct Registration {
    pub scope: DeferScopeId,
    pub dependencies: BTreeSet<PlaceId>,
}

#[derive(Debug, Default)]
pub(crate) struct Region {
    pub blocks: BTreeSet<BlockId>,
    pub locals: BTreeSet<PlaceId>,
    pub values: BTreeSet<ValueId>,
}

#[derive(Debug, Default)]
pub(crate) struct Plan {
    pub registrations: BTreeMap<DeferId, Registration>,
    /// Indexed by the `EnterDefer` block, allowing a source action to have
    /// separately elaborated drains for different lexical exit paths.
    pub regions: BTreeMap<BlockId, Region>,
}

/// Inspect inline body boundaries and their exact free-place dependencies.
/// Cyclic bodies are permitted; completion and failure cleanup are separate
/// finite suffixes ending at a verified boundary.
#[allow(
    clippy::too_many_lines,
    reason = "keep region boundaries and exact free-place validation together"
)]
pub(crate) fn plan(function: &SemFunction) -> Result<Plan, &'static str> {
    let blocks: BTreeMap<_, _> = function.blocks.iter().map(|b| (b.id, b)).collect();
    let mut plan = Plan::default();
    for block in &function.blocks {
        for op in &block.ops {
            if let SemOpKind::RegisterDefer {
                defer,
                scope,
                dependencies,
            } = &op.kind
            {
                let roots: BTreeSet<_> = dependencies.iter().copied().collect();
                if roots.len() != dependencies.len() {
                    return Err("defer registration repeats a free-place dependency");
                }
                if plan
                    .registrations
                    .insert(
                        *defer,
                        Registration {
                            scope: *scope,
                            dependencies: roots,
                        },
                    )
                    .is_some()
                {
                    return Err("defer identity has more than one registration operation");
                }
            }
        }
    }
    let mut parks = BTreeMap::new();
    let mut entered = BTreeSet::new();
    for block in &function.blocks {
        let SemTerminator::EnterDefer { defer, park, body } = &block.terminator else {
            continue;
        };
        let registration = plan
            .registrations
            .get(defer)
            .ok_or("defer entry has no registration")?;
        if parks
            .insert(registration.scope, *park)
            .is_some_and(|old| old != *park)
        {
            return Err("one cleanup scope must reuse one fault park");
        }
        entered.insert(*defer);
        let mut region = Region::default();
        let mut used = BTreeSet::new();
        let mut pending = vec![body.target];
        let mut finished = false;
        while let Some(id) = pending.pop() {
            if !region.blocks.insert(id) {
                continue;
            }
            let block = blocks
                .get(&id)
                .ok_or("defer body targets an unknown block")?;
            for arg in &block.args {
                region.values.insert(arg.value);
            }
            for op in &block.ops {
                if let SemOpKind::AllocPlace { place } = op.kind {
                    region.locals.insert(place);
                }
                op.kind.visit_places(|place| {
                    used.insert(place);
                });
                region.values.extend(op.results.iter().map(|v| v.id));
            }
            if let SemTerminator::Call { unwind, .. } = &block.terminator {
                if !matches!(unwind, crate::CallUnwind::Cleanup(edge) if crate::verify::defer_drain_suffix(edge.target, &blocks, &mut BTreeSet::new()))
                {
                    return Err("defer call failure must leave through bounded cleanup");
                }
            }
            block.terminator.visit_results(|value| {
                region.values.insert(value.id);
            });
            match &block.terminator {
                SemTerminator::FinishDefer {
                    defer: done,
                    park: finished_park,
                    ..
                } if done == defer => {
                    if finished_park != park {
                        return Err("defer finish uses the wrong fault park");
                    }
                    finished = true;
                }
                SemTerminator::Return { .. }
                | SemTerminator::ResumeUnwind
                | SemTerminator::Trap { .. }
                | SemTerminator::Unreachable
                | SemTerminator::Suspend { .. } => {
                    return Err("defer body escapes without its matching finish");
                }
                _ => block
                    .terminator
                    .visit_successors(|edge| pending.push(edge.target)),
            }
        }
        if !finished {
            return Err("defer body has no matching finish");
        }
        // A body-local aggregate's field places are local too. The origin
        // chain, rather than spelling or a second owner graph, identifies it.
        let local_place = |mut place: PlaceId| {
            let mut seen = BTreeSet::new();
            while seen.insert(place) {
                if region.locals.contains(&place) {
                    return true;
                }
                match function
                    .places
                    .iter()
                    .find(|p| p.id == place)
                    .map(|p| &p.origin)
                {
                    Some(crate::PlaceOrigin::Aggregate {
                        base: crate::PlaceBase::Place(parent),
                        ..
                    }) => place = *parent,
                    Some(crate::PlaceOrigin::Aggregate {
                        base: crate::PlaceBase::Value(value),
                        ..
                    }) => return region.values.contains(value),
                    _ => return false,
                }
            }
            false
        };
        used.retain(|place| !local_place(*place));
        if used != registration.dependencies {
            return Err("defer dependencies do not equal the body's free places");
        }
        plan.regions.insert(block.id, region);
    }
    if entered.len() != plan.registrations.len() {
        return Err("registered defer has no body entry");
    }
    Ok(plan)
}

/// The phase carried by the existing availability flow. It contains only
/// action identities and optional parked fault/exit states, never value owners.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Schedule {
    pub pending: Vec<DeferId>,
    pub active: Vec<Frame>,
    pub invalid_join: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Frame {
    pub defer: DeferId,
    pub scope: DeferScopeId,
    pub park: FaultParkId,
    pub entry: BlockId,
    pub pending_base: usize,
    pub fault: u8,
    pub exit: u8,
}

impl Schedule {
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
                a.fault |= b.fault;
                a.exit |= b.exit;
            }
        } else {
            self.invalid_join = true;
        }
        self.invalid_join |= other.invalid_join;
        *self != before
    }

    pub fn register(&mut self, defer: DeferId, scope: DeferScopeId) -> Result<(), &'static str> {
        if self.pending.contains(&defer) || self.active.iter().any(|f| f.defer == defer) {
            return Err("defer registration is already pending or active");
        }
        if self.active.iter().any(|f| f.scope == scope) {
            return Err("defer registration must use a nested scope while a body is active");
        }
        self.pending.push(defer);
        Ok(())
    }

    pub fn enter(&mut self, frame: Frame) -> Result<(), &'static str> {
        if self.pending.last() != Some(&frame.defer) {
            return Err("defer entry must consume the pending top action");
        }
        if self.active.iter().any(|f| f.park == frame.park) {
            return Err("defer entry would overwrite a live fault park");
        }
        if self
            .active
            .last()
            .is_some_and(|f| self.pending.len() <= f.pending_base)
        {
            return Err("defer body cannot enter an older action before finishing");
        }
        self.pending.pop();
        self.active.push(frame);
        Ok(())
    }

    pub fn finish(&mut self, defer: DeferId, park: FaultParkId) -> Result<Frame, &'static str> {
        let frame = self
            .active
            .last()
            .ok_or("defer finish has no active body")?;
        if (frame.defer, frame.park) != (defer, park) {
            return Err("defer finish does not match the active body and park");
        }
        if self.pending.len() != frame.pending_base {
            return Err("defer finish leaves nested actions pending");
        }
        Ok(self.active.pop().expect("active frame checked"))
    }
}

/// A direct call may recurse, but every reachable callee must have a body
/// whose effects are known. Unknown callable and callback effects fail closed.
pub(crate) fn verify_calls(
    module: &crate::SemModule,
    function: &SemFunction,
) -> Result<(), &'static str> {
    fn inspect(
        module: &crate::SemModule,
        term: &SemTerminator,
        seen: &mut BTreeSet<crate::CallableId>,
    ) -> Result<(), &'static str> {
        match term {
            SemTerminator::Call { callee, .. } => {
                if !seen.insert(*callee) {
                    return Ok(());
                }
                let body = module
                    .functions
                    .iter()
                    .find(|f| f.callable == *callee)
                    .ok_or("defer call has no proven non-suspending body")?;
                for block in &body.blocks {
                    inspect(module, &block.terminator, seen)?;
                }
            }
            SemTerminator::Suspend { .. }
            | SemTerminator::IndirectCall { .. }
            | SemTerminator::DynCall { .. }
            | SemTerminator::ValueCall { .. } => {
                return Err("defer call has unproven or suspending effects");
            }
            SemTerminator::RtCall { family, .. }
                if family.is_async_suspending().is_some()
                    || family
                        .semantic_contract()
                        .is_none_or(hew_types::RuntimeSemanticContract::propagates_fault) =>
            {
                return Err("defer call has unproven or suspending effects");
            }
            _ => {}
        }
        Ok(())
    }
    let plan = plan(function)?;
    let body_blocks: BTreeSet<_> = plan
        .regions
        .values()
        .flat_map(|r| r.blocks.iter().copied())
        .collect();
    let mut seen = BTreeSet::new();
    for block in &function.blocks {
        if body_blocks.contains(&block.id) {
            inspect(module, &block.terminator, &mut seen)?;
        }
    }
    Ok(())
}
