//! Whole-function owner-state replay, and the memo that keeps it once-per-body.
//!
//! Two lattices answer "which owner generations are live here, and what does
//! each own": an EXACT one (live on every path into the point) and a
//! POSSIBLY-live one (live on at least one). Between them the ownership passes
//! separate a value that must be dropped from one that may already be gone.
//! Both are pure functions of the ownership operations in `blocks`, which is
//! what lets [`exact_owner_states`] hand the same answer to the whole pass
//! sequence instead of replaying it for each caller.
use super::{BasicBlock, BindingId, HashMap, HashSet, Instr, Place, ENTRY_BLOCK_ID};
#[cfg(test)]
use super::{ResolvedTy, Terminator};

/// Exact owner generations live at a program point, keyed to the place each
/// one currently owns.
pub(in crate::lower) type ExactOwnerState = HashMap<crate::model::OwnerId, Place>;
pub(in crate::lower) type MaybeOwnerState = HashSet<(crate::model::OwnerId, Place)>;
pub(in crate::lower) type MustBindingOwnerState = HashMap<BindingId, Place>;

#[derive(Clone, PartialEq)]
enum OwnerStateOperation {
    Mint {
        owner: crate::model::OwnerId,
        place: Place,
    },
    Transfer {
        owner: crate::model::OwnerId,
        successor: Option<(crate::model::OwnerId, Place)>,
    },
    RelocateOwner {
        owner: crate::model::OwnerId,
        to: Place,
    },
    End {
        owner: crate::model::OwnerId,
    },
    Reset {
        previous: crate::model::OwnerId,
        replacement: crate::model::OwnerId,
        place: Place,
    },
    Rearm {
        previous: crate::model::OwnerId,
        replacement: crate::model::OwnerId,
        place: Place,
    },
    Join {
        incoming: Vec<crate::model::OwnerId>,
        replacement: crate::model::OwnerId,
        place: Place,
    },
    None,
}

#[allow(
    clippy::match_same_arms,
    reason = "explicit physical-copy and unclassified-operation arms document distinct ownership semantics"
)]
fn owner_state_operation(instruction: &Instr) -> OwnerStateOperation {
    use crate::model::OwnershipEvent;

    match instruction {
        // Physical copies are backend mechanics. They never mutate ownership
        // state implicitly: lowering must publish an exact `Relocate` or
        // `Transfer` event at the same program point. This distinction also
        // permits borrowed ABI copies without accidentally moving the owner.
        Instr::Move { .. } | Instr::WitnessMove { .. } => OwnerStateOperation::None,
        Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, .. }) => {
            OwnerStateOperation::Mint {
                owner: *owner,
                place: *place,
            }
        }
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner,
            to,
            to_owner,
            ..
        }) => OwnerStateOperation::Transfer {
            owner: *owner,
            successor: to_owner.zip(*to),
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, to, .. }) => {
            OwnerStateOperation::RelocateOwner {
                owner: *owner,
                to: *to,
            }
        }
        Instr::OwnershipEvent(
            OwnershipEvent::Release { owner, .. }
            | OwnershipEvent::GuardedRelease { owner, .. }
            | OwnershipEvent::DemoteToAlias { owner, .. },
        ) => OwnerStateOperation::End { owner: *owner },
        Instr::OwnershipEvent(OwnershipEvent::Reset {
            previous,
            replacement,
            place,
            ..
        }) => OwnerStateOperation::Reset {
            previous: *previous,
            replacement: *replacement,
            place: *place,
        },
        Instr::OwnershipEvent(OwnershipEvent::Rearm {
            previous,
            replacement,
            place,
            ..
        }) => OwnerStateOperation::Rearm {
            previous: *previous,
            replacement: *replacement,
            place: *place,
        },
        Instr::OwnershipEvent(OwnershipEvent::Join {
            incoming,
            replacement,
            place,
            ..
        }) => OwnerStateOperation::Join {
            incoming: incoming.clone(),
            replacement: *replacement,
            place: *place,
        },
        _ => OwnerStateOperation::None,
    }
}

pub(in crate::lower) fn apply_exact_owner_ops(instructions: &[Instr], live: &mut ExactOwnerState) {
    for instruction in instructions {
        match owner_state_operation(instruction) {
            OwnerStateOperation::Mint { owner, place } => {
                live.insert(owner, place);
            }
            OwnerStateOperation::Transfer { owner, successor } => {
                live.remove(&owner);
                if let Some((next, destination)) = successor {
                    live.insert(next, destination);
                }
            }
            OwnerStateOperation::RelocateOwner { owner, to } => {
                if let Some(place) = live.get_mut(&owner) {
                    *place = to;
                }
            }
            OwnerStateOperation::End { owner } => {
                live.remove(&owner);
            }
            OwnerStateOperation::Reset {
                previous,
                replacement,
                place,
            }
            | OwnerStateOperation::Rearm {
                previous,
                replacement,
                place,
            } => {
                live.remove(&previous);
                live.insert(replacement, place);
            }
            OwnerStateOperation::Join {
                incoming: _,
                replacement,
                place,
            } => {
                live.retain(|owner, _| owner.binding != replacement.binding);
                live.insert(replacement, place);
            }
            OwnerStateOperation::None => {}
        }
    }
}

pub(in crate::lower) fn apply_maybe_owner_ops(instructions: &[Instr], live: &mut MaybeOwnerState) {
    for instruction in instructions {
        match owner_state_operation(instruction) {
            OwnerStateOperation::Mint { owner, place } => {
                live.insert((owner, place));
            }
            OwnerStateOperation::Transfer { owner, successor } => {
                live.retain(|(candidate, _)| *candidate != owner);
                if let Some(next) = successor {
                    live.insert(next);
                }
            }
            OwnerStateOperation::RelocateOwner { owner, to } => {
                let was_live = live.iter().any(|(candidate, _)| *candidate == owner);
                live.retain(|(candidate, _)| *candidate != owner);
                if was_live {
                    live.insert((owner, to));
                }
            }
            OwnerStateOperation::End { owner } => {
                live.retain(|(candidate, _)| *candidate != owner);
            }
            OwnerStateOperation::Reset {
                previous,
                replacement,
                place,
            } => {
                live.retain(|(candidate, _)| *candidate != previous);
                live.insert((replacement, place));
            }
            OwnerStateOperation::Rearm {
                previous,
                replacement,
                place,
            } => {
                live.retain(|(candidate, _)| *candidate != previous);
                live.insert((replacement, place));
            }
            OwnerStateOperation::Join {
                incoming: _,
                replacement,
                place,
            } => {
                live.retain(|(owner, _)| owner.binding != replacement.binding);
                live.insert((replacement, place));
            }
            OwnerStateOperation::None => {}
        }
    }
}

/// Whether the per-block cache checkers run.
///
/// Never in release: `cfg!` is a compile-time constant there, so every checker
/// call folds away. In a debug build, always for the crate's own unit tests,
/// and otherwise only when `HEW_DEBUG_OWNER_STATE_CACHES` is set - the checker
/// costs two whole-function replays per block visit, which is worth paying to
/// prove the dirty-flag discipline on a real module and not worth paying on
/// every debug compile.
fn owner_state_caches_checked() -> bool {
    static CHECKED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    if !cfg!(debug_assertions) {
        return false;
    }
    if cfg!(test) {
        return true;
    }
    *CHECKED.get_or_init(|| std::env::var_os("HEW_DEBUG_OWNER_STATE_CACHES").is_some())
}

/// Debug-only: check that a cached exact-owner replay still describes `blocks`.
///
/// Four passes hold the replay across a per-block loop and re-derive it only
/// when a flag they set at their own mutation sites says the blocks changed.
/// A missed mutation site is silent in release - a later block simply reads
/// owner state describing the pre-rewrite program, and the damage surfaces as
/// an obligation imbalance or a runtime leak somewhere else entirely. That is
/// the defect `prepare_owned_call_carriers` shipped with, found by reading
/// rather than by anything in the tree. Every debug build and every unit test
/// now re-derives and compares, so the flag discipline has a checker.
///
/// Compiled out of release entirely: `cfg!` is a compile-time constant, so the
/// whole body folds away and the call costs nothing on the shipping path.
///
/// In a debug build it is opt-in, because it is not free there either: it
/// re-derives on every visit of a per-block loop, which is quadratic in block
/// count where the pass itself is linear. Measured on the `emit_document`
/// fixture, that is 30 s debug against 0.6 s release, and on a real module
/// (goobers `workflow_machine.hew`) 160 s against 2 s. Unit tests run it
/// unconditionally - their bodies are a handful of blocks - and a debug
/// compiler runs it when [`owner_state_caches_checked`] says to.
pub(in crate::lower) fn debug_assert_exact_entries_current(
    blocks: &[BasicBlock],
    cached: &HashMap<u32, ExactOwnerState>,
    pass: &'static str,
) {
    if !owner_state_caches_checked() {
        return;
    }
    let (fresh, _) = exact_owner_states_unattributed(blocks);
    assert!(
        *cached == fresh,
        "{pass} is reading a stale exact-owner replay: it rewrote `blocks` at a \
         site that does not mark the cache dirty"
    );
}

/// Debug-only twin of [`debug_assert_exact_entries_current`] for the
/// possibly-live lattice.
pub(in crate::lower) fn debug_assert_maybe_entries_current(
    blocks: &[BasicBlock],
    cached: &HashMap<u32, MaybeOwnerState>,
    pass: &'static str,
) {
    if !owner_state_caches_checked() {
        return;
    }
    let (fresh, _) = maybe_owner_states_unattributed(blocks);
    assert!(
        *cached == fresh,
        "{pass} is reading a stale maybe-owner replay: it rewrote `blocks` at a \
         site that does not mark the cache dirty"
    );
}

/// Position of every block in `blocks`, keyed by its id.
///
/// The owner-state replays pop block ids off a worklist and revisit a block
/// once per predecessor generation change. Resolving each pop by scanning
/// `blocks` made a replay quadratic in block count on its own, which is the
/// wrong cost for an analysis whose real work is linear in instructions.
fn block_index_by_id(blocks: &[BasicBlock]) -> HashMap<u32, usize> {
    blocks
        .iter()
        .enumerate()
        .map(|(position, block)| (block.id, position))
        .collect()
}

/// The call site of a whole-function derivation, in builds that can name it.
///
/// `#[track_caller]` is not free on the shipping path: it puts a hidden
/// location argument on every call of an analysis the lowering runs tens of
/// thousands of times per module, and on a 1 444-function module that measured
/// about 0.6 s of a 29 s check. `make bench-mir` only needs the totals, so the
/// release compiler counts totals and a debug build carries the per-site table
/// that says WHICH pass grew.
#[cfg_attr(debug_assertions, track_caller)]
#[allow(
    clippy::unnecessary_wraps,
    reason = "clippy sees only the debug_assertions arm; the release arm returns None"
)]
fn derivation_site() -> Option<&'static std::panic::Location<'static>> {
    #[cfg(debug_assertions)]
    {
        Some(std::panic::Location::caller())
    }
    #[cfg(not(debug_assertions))]
    {
        None
    }
}

/// Everything the two owner-state replays read out of `blocks`.
///
/// Both replays consume exactly three things: each block's id, its successor
/// ids, and the [`OwnerStateOperation`] of each of its instructions. Nothing
/// else in a `BasicBlock` can change their answer, so two `blocks` with equal
/// keys have equal owner state, whatever else the pass sequence rewrote in
/// between.
struct OwnerStateKey {
    /// Per block, in order: its id, its successors, and how many of `ops`
    /// belong to it. The count is what keeps the flattened `ops` segmented, so
    /// moving an instruction across a block boundary cannot compare equal.
    blocks: Vec<(u32, Vec<u32>, usize)>,
    /// Every block's operations, concatenated in block order.
    ops: Vec<OwnerStateOperation>,
}

fn owner_state_key(blocks: &[BasicBlock]) -> OwnerStateKey {
    let mut ops = Vec::with_capacity(blocks.iter().map(|block| block.instructions.len()).sum());
    let key_blocks = blocks
        .iter()
        .map(|block| {
            ops.extend(block.instructions.iter().map(owner_state_operation));
            (block.id, block.successors(), block.instructions.len())
        })
        .collect();
    OwnerStateKey {
        blocks: key_blocks,
        ops,
    }
}

/// Whether `blocks` still projects to `key`, without building a second key.
///
/// The comparison is the hot path — it runs on every owner-state query, and
/// nearly all of them match — so it streams and short-circuits rather than
/// materialising a key to compare against.
fn owner_state_key_matches(blocks: &[BasicBlock], key: &OwnerStateKey) -> bool {
    if key.blocks.len() != blocks.len() {
        return false;
    }
    let mut cursor = 0usize;
    for (block, (id, successors, count)) in blocks.iter().zip(&key.blocks) {
        if block.id != *id || block.instructions.len() != *count {
            return false;
        }
        if block.successors() != *successors {
            return false;
        }
        let Some(ops) = key.ops.get(cursor..cursor + *count) else {
            return false;
        };
        if block
            .instructions
            .iter()
            .zip(ops)
            .any(|(instruction, op)| owner_state_operation(instruction) != *op)
        {
            return false;
        }
        cursor += count;
    }
    cursor == key.ops.len()
}

/// Entry and exit owner state for every block, as the replays produce it.
pub(in crate::lower) type ExactOwnerStates =
    (HashMap<u32, ExactOwnerState>, HashMap<u32, ExactOwnerState>);
/// The possibly-live twin of [`ExactOwnerStates`].
pub(in crate::lower) type MaybeOwnerStates =
    (HashMap<u32, MaybeOwnerState>, HashMap<u32, MaybeOwnerState>);

/// The last owner state derived, with the exact input it was derived from.
///
/// MIR lowering runs about fifty passes that each ask for whole-function owner
/// state, and nearly every consecutive pair of them asks about the same
/// program: the pass in between either rewrote nothing or rewrote something no
/// replay reads. Re-deriving for each is the compile-time cost F5 exists to
/// remove, and cloning the answer out for each is the rest of it, so the
/// derivation is handed out behind an `Rc` and the callers read it in place.
///
/// This is a *verified*-input cache, not a dirty flag. It never claims a
/// rewrite left the answer alone; it re-reads the whole projection the replays
/// consume and compares it. A pass that changed the ownership operations gets a
/// fresh derivation because its key differs, and no pass has to remember to say
/// so. Equal key, equal answer: the replays are pure functions of the key and
/// of nothing else.
///
/// Deliberately NOT used by the four passes that hold a replay across their own
/// per-block rewrite loop (`canonicalize_terminal_transfer_owner_ids` and
/// friends). Those would re-read the key on every block visit, which is the
/// same quadratic shape their dirty flags exist to avoid; they keep the flags,
/// and [`debug_assert_exact_entries_current`] keeps the flags honest.
#[derive(Default)]
struct OwnerStateMemo {
    key: Option<OwnerStateKey>,
    exact: Option<std::rc::Rc<ExactOwnerStates>>,
    maybe: Option<std::rc::Rc<MaybeOwnerStates>>,
}

thread_local! {
    static OWNER_STATE_MEMO: std::cell::RefCell<OwnerStateMemo> =
        std::cell::RefCell::new(OwnerStateMemo::default());
}

/// Adopt `blocks` as the memo's subject, discarding anything derived from
/// another program. Returns whether the memo already held this one.
fn owner_state_memo_admit(blocks: &[BasicBlock]) -> bool {
    OWNER_STATE_MEMO.with(|memo| {
        let mut memo = memo.borrow_mut();
        if memo
            .key
            .as_ref()
            .is_some_and(|key| owner_state_key_matches(blocks, key))
        {
            return true;
        }
        *memo = OwnerStateMemo {
            key: Some(owner_state_key(blocks)),
            exact: None,
            maybe: None,
        };
        false
    })
}

#[cfg_attr(debug_assertions, track_caller)]
pub(in crate::lower) fn exact_owner_states(blocks: &[BasicBlock]) -> std::rc::Rc<ExactOwnerStates> {
    let _timing = crate::timing::stage("exact_owner_states");
    crate::timing::derivation("exact_owner_states", derivation_site());
    if owner_state_memo_admit(blocks) {
        if let Some(cached) = OWNER_STATE_MEMO.with(|memo| memo.borrow().exact.clone()) {
            return cached;
        }
    }
    crate::timing::replay("exact_owner_states");
    let derived = std::rc::Rc::new(exact_owner_states_unattributed(blocks));
    OWNER_STATE_MEMO.with(|memo| memo.borrow_mut().exact = Some(std::rc::Rc::clone(&derived)));
    derived
}

/// The replay itself, without the timing and call-site accounting.
///
/// [`debug_assert_exact_entries_current`] re-derives on every visit of a
/// caching pass's per-block loop, which would otherwise dominate the derivation
/// table it exists to keep honest.
fn exact_owner_states_unattributed(blocks: &[BasicBlock]) -> ExactOwnerStates {
    let index = block_index_by_id(blocks);
    let mut entries = HashMap::from([(ENTRY_BLOCK_ID, ExactOwnerState::new())]);
    let mut exits = HashMap::new();
    let mut queue = std::collections::VecDeque::from([ENTRY_BLOCK_ID]);
    while let Some(block_id) = queue.pop_front() {
        let Some(block) = index.get(&block_id).map(|position| &blocks[*position]) else {
            continue;
        };
        let mut outgoing = entries.get(&block_id).cloned().unwrap_or_default();
        apply_exact_owner_ops(&block.instructions, &mut outgoing);
        exits.insert(block_id, outgoing.clone());
        for successor in block.successors() {
            let changed = if let Some(existing) = entries.get_mut(&successor) {
                let joined: ExactOwnerState = existing
                    .iter()
                    .filter_map(|(owner, place)| {
                        (outgoing.get(owner) == Some(place)).then_some((*owner, *place))
                    })
                    .collect();
                if *existing == joined {
                    false
                } else {
                    *existing = joined;
                    true
                }
            } else {
                entries.insert(successor, outgoing.clone());
                true
            };
            if changed {
                queue.push_back(successor);
            }
        }
    }
    (entries, exits)
}

#[cfg_attr(debug_assertions, track_caller)]
pub(in crate::lower) fn maybe_owner_states(blocks: &[BasicBlock]) -> std::rc::Rc<MaybeOwnerStates> {
    let _timing = crate::timing::stage("maybe_owner_states");
    crate::timing::derivation("maybe_owner_states", derivation_site());
    if owner_state_memo_admit(blocks) {
        if let Some(cached) = OWNER_STATE_MEMO.with(|memo| memo.borrow().maybe.clone()) {
            return cached;
        }
    }
    crate::timing::replay("maybe_owner_states");
    let derived = std::rc::Rc::new(maybe_owner_states_unattributed(blocks));
    OWNER_STATE_MEMO.with(|memo| memo.borrow_mut().maybe = Some(std::rc::Rc::clone(&derived)));
    derived
}

/// The replay itself, without the timing and call-site accounting. See
/// [`exact_owner_states_unattributed`].
fn maybe_owner_states_unattributed(blocks: &[BasicBlock]) -> MaybeOwnerStates {
    let index = block_index_by_id(blocks);
    let mut entries = HashMap::from([(ENTRY_BLOCK_ID, MaybeOwnerState::new())]);
    let mut exits = HashMap::new();
    let mut queue = std::collections::VecDeque::from([ENTRY_BLOCK_ID]);
    while let Some(block_id) = queue.pop_front() {
        let Some(block) = index.get(&block_id).map(|position| &blocks[*position]) else {
            continue;
        };
        let mut outgoing = entries.get(&block_id).cloned().unwrap_or_default();
        apply_maybe_owner_ops(&block.instructions, &mut outgoing);
        exits.insert(block_id, outgoing.clone());
        for successor in block.successors() {
            let changed = if let Some(existing) = entries.get_mut(&successor) {
                let before = existing.len();
                existing.extend(outgoing.iter().copied());
                existing.len() != before
            } else {
                entries.insert(successor, outgoing.clone());
                true
            };
            if changed {
                queue.push_back(successor);
            }
        }
    }
    (entries, exits)
}

fn apply_must_binding_owner_ops(instructions: &[Instr], live: &mut MustBindingOwnerState) {
    for instruction in instructions {
        match owner_state_operation(instruction) {
            OwnerStateOperation::Mint { owner, place } => {
                live.insert(owner.binding, place);
            }
            OwnerStateOperation::Transfer { owner, successor } => {
                live.remove(&owner.binding);
                if let Some((next, destination)) = successor {
                    live.insert(next.binding, destination);
                }
            }
            OwnerStateOperation::RelocateOwner { owner, to } => {
                if let Some(place) = live.get_mut(&owner.binding) {
                    *place = to;
                }
            }
            OwnerStateOperation::End { owner } => {
                live.remove(&owner.binding);
            }
            OwnerStateOperation::Reset {
                previous,
                replacement,
                place,
            }
            | OwnerStateOperation::Rearm {
                previous,
                replacement,
                place,
            } => {
                live.remove(&previous.binding);
                live.insert(replacement.binding, place);
            }
            OwnerStateOperation::Join {
                incoming,
                replacement,
                place,
            } => {
                for owner in incoming {
                    live.remove(&owner.binding);
                }
                live.insert(replacement.binding, place);
            }
            OwnerStateOperation::None => {}
        }
    }
}

/// Generation-erased must-own state used only to justify an ownership-SSA
/// Join. Unlike exact `OwnerId` intersection, this lattice preserves a binding
/// when every predecessor owns it in the same physical place even if their
/// generations differ. It is derived solely from explicit ownership events.
pub(in crate::lower) fn must_binding_owner_states(
    blocks: &[BasicBlock],
) -> (
    HashMap<u32, MustBindingOwnerState>,
    HashMap<u32, MustBindingOwnerState>,
) {
    let mut entries = HashMap::from([(ENTRY_BLOCK_ID, MustBindingOwnerState::new())]);
    let mut exits = HashMap::new();
    let mut queue = std::collections::VecDeque::from([ENTRY_BLOCK_ID]);
    while let Some(block_id) = queue.pop_front() {
        let Some(block) = blocks.iter().find(|block| block.id == block_id) else {
            continue;
        };
        let mut outgoing = entries.get(&block_id).cloned().unwrap_or_default();
        apply_must_binding_owner_ops(&block.instructions, &mut outgoing);
        exits.insert(block_id, outgoing.clone());
        for successor in block.successors() {
            let changed = if let Some(existing) = entries.get_mut(&successor) {
                let joined = existing
                    .iter()
                    .filter_map(|(binding, place)| {
                        (outgoing.get(binding) == Some(place)).then_some((*binding, *place))
                    })
                    .collect::<MustBindingOwnerState>();
                if *existing == joined {
                    false
                } else {
                    *existing = joined;
                    true
                }
            } else {
                entries.insert(successor, outgoing.clone());
                true
            };
            if changed {
                queue.push_back(successor);
            }
        }
    }
    (entries, exits)
}

/// Generations whose ownership was consumed (transferred, released, or
/// demoted) on at least one path. Paired with [`maybe_owner_states`] this
/// separates an owner that is *conditionally consumed* — live on one incoming
/// path and ended on another, so no static cleanup is admissible — from one
/// that is merely conditionally minted, whose absent generation owes nothing
/// on the paths that never created it. A `Reset`/`Rearm` predecessor or a
/// `Join` input is renamed into its successor generation, not consumed: the
/// generation-lineage rules own those, so they are not recorded here.
pub(in crate::lower) type MaybeEndedOwnerState = HashSet<crate::model::OwnerId>;

fn apply_maybe_ended_owner_ops(instructions: &[Instr], ended: &mut MaybeEndedOwnerState) {
    for instruction in instructions {
        match owner_state_operation(instruction) {
            OwnerStateOperation::Transfer { owner, .. } | OwnerStateOperation::End { owner } => {
                ended.insert(owner);
            }
            OwnerStateOperation::Reset { .. }
            | OwnerStateOperation::Rearm { .. }
            | OwnerStateOperation::Join { .. }
            | OwnerStateOperation::Mint { .. }
            | OwnerStateOperation::RelocateOwner { .. }
            | OwnerStateOperation::None => {}
        }
    }
}

pub(in crate::lower) fn maybe_ended_owner_states(
    blocks: &[BasicBlock],
) -> (
    HashMap<u32, MaybeEndedOwnerState>,
    HashMap<u32, MaybeEndedOwnerState>,
) {
    let mut entries = HashMap::from([(ENTRY_BLOCK_ID, MaybeEndedOwnerState::new())]);
    let mut exits = HashMap::new();
    let mut queue = std::collections::VecDeque::from([ENTRY_BLOCK_ID]);
    while let Some(block_id) = queue.pop_front() {
        let Some(block) = blocks.iter().find(|block| block.id == block_id) else {
            continue;
        };
        let mut outgoing = entries.get(&block_id).cloned().unwrap_or_default();
        apply_maybe_ended_owner_ops(&block.instructions, &mut outgoing);
        exits.insert(block_id, outgoing.clone());
        for successor in block.successors() {
            let changed = if let Some(existing) = entries.get_mut(&successor) {
                let before = existing.len();
                existing.extend(outgoing.iter().copied());
                existing.len() != before
            } else {
                entries.insert(successor, outgoing.clone());
                true
            };
            if changed {
                queue.push_back(successor);
            }
        }
    }
    (entries, exits)
}

#[cfg(test)]
mod owner_state_cache_tests {
    use super::{
        debug_assert_exact_entries_current, debug_assert_maybe_entries_current, exact_owner_states,
        maybe_owner_states, BasicBlock, BindingId, Instr, Place, ResolvedTy, Terminator,
        ENTRY_BLOCK_ID,
    };
    use crate::model::{OwnerId, OwnershipEvent};

    fn owner(binding: u32) -> OwnerId {
        OwnerId {
            binding: BindingId(binding),
            generation: 0,
        }
    }

    fn mint(binding: u32, local: u32) -> Instr {
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: owner(binding),
            place: Place::Local(local),
            ty: ResolvedTy::String,
        })
    }

    /// Two blocks so a caching pass has a second visit to read a stale answer on.
    fn two_blocks(entry: Vec<Instr>) -> Vec<BasicBlock> {
        vec![
            BasicBlock {
                id: ENTRY_BLOCK_ID,
                statements: Vec::new(),
                instructions: entry,
                terminator: Terminator::Goto { target: 1 },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ]
    }

    /// The checker must accept the answer the passes actually hold: derived from
    /// these blocks, with no rewrite since. A checker that rejects this fires on
    /// every debug compile and gets deleted rather than fixed.
    #[test]
    fn a_replay_derived_from_the_current_blocks_is_accepted() {
        let blocks = two_blocks(vec![mint(1, 0)]);
        let exact_states = exact_owner_states(&blocks);
        let exact = &exact_states.0;
        let maybe_states = maybe_owner_states(&blocks);
        let maybe = &maybe_states.0;

        debug_assert_exact_entries_current(&blocks, exact, "test");
        debug_assert_maybe_entries_current(&blocks, maybe, "test");
    }

    /// The defect the checker exists for: a pass rewrites `blocks` at a site that
    /// does not mark its cache dirty, and a later block reads owner state
    /// describing the program before the rewrite.
    #[test]
    #[should_panic(expected = "stale exact-owner replay")]
    fn a_replay_from_before_an_unmarked_rewrite_is_rejected() {
        let mut blocks = two_blocks(vec![mint(1, 0)]);
        let exact_states = exact_owner_states(&blocks);
        let exact = &exact_states.0;

        blocks[0].instructions.push(mint(2, 1));

        debug_assert_exact_entries_current(&blocks, exact, "test");
    }

    #[test]
    #[should_panic(expected = "stale maybe-owner replay")]
    fn a_possibly_live_replay_from_before_an_unmarked_rewrite_is_rejected() {
        let mut blocks = two_blocks(vec![mint(1, 0)]);
        let maybe_states = maybe_owner_states(&blocks);
        let maybe = &maybe_states.0;

        blocks[0].instructions.push(mint(2, 1));

        debug_assert_maybe_entries_current(&blocks, maybe, "test");
    }

    /// A rewrite that does not change what the replay computes must not fire the
    /// checker; the invariant is "the cached ANSWER is current", not "nothing was
    /// touched". A checker keyed on an edit counter instead of the answer would
    /// fail here and make the four caching passes unfixable.
    #[test]
    fn a_rewrite_the_replay_cannot_see_is_accepted() {
        let mut blocks = two_blocks(vec![mint(1, 0)]);
        let exact_states = exact_owner_states(&blocks);
        let exact = &exact_states.0;

        blocks[0].instructions.push(Instr::ConstI64 {
            dest: Place::Local(9),
            value: 7,
        });

        debug_assert_exact_entries_current(&blocks, exact, "test");
    }
}
