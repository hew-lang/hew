//! Proven dependencies of strict local actor turns.
//!
//! Direct nested invocations retain their actor turn identity. Independent task
//! invocations do not: executing a forked ask alone does not block its actor.
//! A readiness wake invalidates the outgoing dependency before scheduling the
//! actor, so a satisfied wait cannot participate in a later false cycle.

use crate::coro_state::HewCoroState;
use crate::lifetime::{live_actors, local_handles};
use crate::util::MutexExt;
use live_actors::ActorIncarnation;
use rustc_hash::FxHashMap;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, LazyLock, Mutex, OnceLock, Weak};

const WAIT_SHARDS: usize = 64;
const STATE_DISARMED: u8 = 0;
const STATE_ARMED: u8 = 1;
const STATE_PENDING: u8 = 2;

#[derive(Debug)]
pub struct HewActorWaitEdge {
    owner: ActorIncarnation,
    /// Inline first target avoids a heap `Vec` on single-target asks and waits.
    primary_target: Option<ActorIncarnation>,
    /// Additional targets for multi-arm `select` registrations.
    extra_targets: Box<[ActorIncarnation]>,
    external_progress: bool,
    operation: i32,
    state: AtomicU8,
    replaced: AtomicBool,
    diagnostic: OnceLock<String>,
}

impl HewActorWaitEdge {
    fn targets(&self) -> impl Iterator<Item = ActorIncarnation> + '_ {
        self.primary_target
            .into_iter()
            .chain(self.extra_targets.iter().copied())
    }

    fn can_block(&self) -> bool {
        !self.external_progress && self.primary_target.is_some()
    }
}

#[cfg_attr(target_arch = "aarch64", repr(align(128)))]
#[cfg_attr(not(target_arch = "aarch64"), repr(align(64)))]
struct WaitShard {
    len: AtomicUsize,
    entries: Mutex<FxHashMap<ActorIncarnation, Weak<HewActorWaitEdge>>>,
}

impl WaitShard {
    fn new() -> Self {
        Self {
            len: AtomicUsize::new(0),
            entries: Mutex::new(FxHashMap::default()),
        }
    }
}

#[inline]
const fn shard_index(owner: ActorIncarnation) -> usize {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "masked to WAIT_SHARDS - 1 (0..64)"
    )]
    {
        (owner.actor_id() as usize) & (WAIT_SHARDS - 1)
    }
}

static WAITS: LazyLock<[WaitShard; WAIT_SHARDS]> =
    LazyLock::new(|| std::array::from_fn(|_| WaitShard::new()));

/// Serializes rare potential-cycle graph snapshots so two actors closing the
/// same knot simultaneously do not race diagnostic publication.
static CYCLE_CHECK_LOCK: Mutex<()> = Mutex::new(());

fn is_target_pending(target: ActorIncarnation) -> bool {
    let shard = &WAITS[shard_index(target)];
    if shard.len.load(Ordering::SeqCst) == 0 {
        return false;
    }
    let entries = shard.entries.lock_or_recover();
    entries
        .get(&target)
        .and_then(Weak::upgrade)
        .is_some_and(|edge| edge.state.load(Ordering::SeqCst) == STATE_PENDING)
}

impl HewActorWaitEdge {
    fn register(owner: ActorIncarnation, target: ActorIncarnation, operation: i32) -> Arc<Self> {
        let edge = Arc::new(Self {
            owner,
            primary_target: Some(target),
            extra_targets: Box::default(),
            external_progress: false,
            operation,
            state: AtomicU8::new(STATE_DISARMED),
            replaced: AtomicBool::new(false),
            diagnostic: OnceLock::new(),
        });
        Self::install(owner, &edge);
        edge
    }

    fn register_alternatives(
        owner: ActorIncarnation,
        targets: Vec<ActorIncarnation>,
        external_progress: bool,
        operation: i32,
    ) -> Arc<Self> {
        let mut iter = targets.into_iter();
        let primary_target = iter.next();
        let extra_targets: Box<[ActorIncarnation]> = iter.collect();
        let edge = Arc::new(Self {
            owner,
            primary_target,
            extra_targets,
            external_progress,
            operation,
            state: AtomicU8::new(STATE_DISARMED),
            replaced: AtomicBool::new(false),
            diagnostic: OnceLock::new(),
        });
        Self::install(owner, &edge);
        edge
    }

    fn install(owner: ActorIncarnation, edge: &Arc<Self>) {
        let shard = &WAITS[shard_index(owner)];
        let mut entries = shard.entries.lock_or_recover();
        if let Some(previous) = entries.insert(owner, Arc::downgrade(edge)) {
            if let Some(previous_edge) = previous.upgrade() {
                previous_edge.replaced.store(true, Ordering::Release);
                previous_edge.state.store(STATE_DISARMED, Ordering::SeqCst);
            }
        } else {
            shard.len.fetch_add(1, Ordering::SeqCst);
        }
    }
}

/// Invalidate before readiness makes the actor runnable. The operation arms
/// its candidate again before the next poll, then confirms only a pending poll.
pub(crate) fn ready(owner: ActorIncarnation) {
    let shard = &WAITS[shard_index(owner)];
    if shard.len.load(Ordering::SeqCst) == 0 {
        return;
    }
    let entries = shard.entries.lock_or_recover();
    if let Some(edge) = entries.get(&owner).and_then(Weak::upgrade) {
        edge.state.store(STATE_DISARMED, Ordering::SeqCst);
    }
}

pub(crate) fn resolve_target(target: local_handles::HewLocalPidId) -> Option<ActorIncarnation> {
    local_handles::resolve_current_actor(target).and_then(|id| {
        live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: this guard pins the exact destination during capture.
            unsafe { ActorIncarnation::of(actor) }
        })
    })
}

/// Select dependencies are alternatives. A timer or an independently driven
/// source is a possible escape, so that turn cannot prove a closed actor knot.
/// This registration replaces the owner's single strict dependency atomically.
pub(crate) fn select_alternatives(
    owner: ActorIncarnation,
    targets: Vec<ActorIncarnation>,
    external_progress: bool,
) -> *const HewActorWaitEdge {
    if owner.is_none() {
        return std::ptr::null();
    }
    Arc::into_raw(HewActorWaitEdge::register_alternatives(
        owner,
        targets,
        external_progress,
        3,
    ))
}

/// Create a candidate for an exact local actor dependency; null means the
/// invocation does not hold a proven actor turn or the destination has retired.
///
/// # Safety
/// `state` is the live invocation state for this operation.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_new(
    state: *const HewCoroState,
    target: local_handles::HewLocalPidId,
    operation: i32,
) -> *const HewActorWaitEdge {
    // SAFETY: the caller holds its invocation state throughout construction.
    let owner = unsafe { &*state }.execution().actor_turn;
    if owner.is_none() {
        return std::ptr::null();
    }
    let target = resolve_target(target);
    target.map_or(std::ptr::null(), |target| {
        Arc::into_raw(HewActorWaitEdge::register(owner, target, operation))
    })
}

/// Register a dependency on the target incarnation already captured by a
/// local actor call during submission, avoiding a redundant handle and
/// live-actor registry lookup.
///
/// # Safety
/// `state` and `call` remain live through this registration.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_new_for_call(
    state: *const HewCoroState,
    call: *const crate::actor_call_native::HewActorCall,
    operation: i32,
) -> *const HewActorWaitEdge {
    // SAFETY: the caller retains both pointers for this call.
    let owner = unsafe { &*state }.execution().actor_turn;
    if owner.is_none() || call.is_null() {
        return std::ptr::null();
    }
    // SAFETY: the caller exclusively owns this live call operation.
    let Some(target) = unsafe { &*call }.target else {
        return std::ptr::null();
    };
    Arc::into_raw(HewActorWaitEdge::register(owner, target, operation))
}

/// Register a dependency on the incarnation selected by a terminal observer.
/// A role's owner token is a supervisor, so resolving that token independently
/// cannot prove an actor wait cycle and can select a different child after a
/// restart.
///
/// # Safety
/// `state` and `wait` remain live through this registration.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_new_for_wait(
    state: *const HewCoroState,
    wait: *const crate::actor_native::HewNativeActorWait,
    operation: i32,
) -> *const HewActorWaitEdge {
    // SAFETY: the caller retains both pointers for this call.
    let owner = unsafe { &*state }.execution().actor_turn;
    // SAFETY: the same caller retains this wait until registration returns.
    let target = unsafe { &*wait }.target;
    if owner.is_none() || target.is_none() {
        return std::ptr::null();
    }
    Arc::into_raw(HewActorWaitEdge::register(owner, target, operation))
}

/// Arm before checking the operation predicate, closing registration/wake races.
///
/// # Safety
/// The pointer is null or one live operation-owned edge.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_prepare(edge: *const HewActorWaitEdge) {
    // SAFETY: the caller retains this optional immutable edge.
    let Some(edge) = (unsafe { edge.as_ref() }) else {
        return;
    };
    if !edge.replaced.load(Ordering::Acquire) {
        edge.state.store(STATE_ARMED, Ordering::SeqCst);
    }
}

/// Confirm a pending predicate and return 1 only when it closes a local cycle.
/// Wakes and confirmations synchronize through the edge's `SeqCst` state so
/// neither can resurrect a wake that happened between predicate inspection and
/// this confirmation, and two actors closing a cycle concurrently cannot both
/// miss each other's pending state.
///
/// # Safety
/// The pointer is null or a live edge prepared before the last pending poll.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_pending(edge: *const HewActorWaitEdge) -> i32 {
    // SAFETY: the operation holds its edge for this confirmation.
    let Some(edge) = (unsafe { edge.as_ref() }) else {
        return 0;
    };
    if edge.replaced.load(Ordering::Acquire) {
        return 0;
    }
    if edge
        .state
        .compare_exchange(
            STATE_ARMED,
            STATE_PENDING,
            Ordering::SeqCst,
            Ordering::Acquire,
        )
        .is_err()
    {
        return 0;
    }
    std::sync::atomic::fence(Ordering::SeqCst);

    // An independent or non-pending alternative rules out this edge before
    // the graph walk. Keep the edge pending so a later confirmation can close
    // a cycle through it.
    if !edge.can_block() || edge.targets().any(|target| !is_target_pending(target)) {
        return 0;
    }

    detect_cycle_slow(edge)
}

#[cold]
#[inline(never)]
fn detect_cycle_slow(edge: &HewActorWaitEdge) -> i32 {
    let _cycle_guard = CYCLE_CHECK_LOCK.lock_or_recover();
    if edge.replaced.load(Ordering::Acquire) || edge.state.load(Ordering::SeqCst) != STATE_PENDING {
        return 0;
    }

    // A turn is proven blocked only when every alternative is another proven
    // blocked turn. Remove escapes to readiness, independent sources, timers
    // and unregistered actors until the remaining closed set stops shrinking.
    let mut active: HashMap<ActorIncarnation, Arc<HewActorWaitEdge>> = HashMap::new();
    for shard in WAITS.iter() {
        if shard.len.load(Ordering::SeqCst) == 0 {
            continue;
        }
        let entries = shard.entries.lock_or_recover();
        for (owner, weak) in entries.iter() {
            if let Some(candidate) = weak.upgrade() {
                if candidate.state.load(Ordering::SeqCst) == STATE_PENDING {
                    active.insert(*owner, candidate);
                }
            }
        }
    }
    let mut blocked: HashSet<_> = active
        .iter()
        .filter(|(_, candidate)| candidate.can_block())
        .map(|(owner, _)| *owner)
        .collect();
    loop {
        let escaped: Vec<_> = blocked
            .iter()
            .filter(|owner| {
                active[owner]
                    .targets()
                    .any(|target| !blocked.contains(&target))
            })
            .copied()
            .collect();
        if escaped.is_empty() {
            break;
        }
        for owner in escaped {
            blocked.remove(&owner);
        }
    }
    if !blocked.contains(&edge.owner) {
        return 0;
    }
    let mut path = Vec::new();
    let mut next = edge.owner;
    let mut visited = HashSet::new();
    while visited.insert(next) {
        path.push(next.actor_id());
        next = active[&next].primary_target.expect("blocked edge target");
    }
    path.push(next.actor_id());
    let operation = match edge.operation {
        0 => "ask",
        1 => "mailbox wait",
        2 => "actor termination wait",
        _ => "select",
    };
    let path = path
        .iter()
        .map(u64::to_string)
        .collect::<Vec<_>>()
        .join(" -> ");
    let _ = edge
        .diagnostic
        .set(format!("local actor wait cycle at {operation}: {path}"));
    edge.state.store(STATE_DISARMED, Ordering::SeqCst);
    1
}

/// Materialize the diagnosed cycle as an ordinary owned logical fault.
///
/// # Safety
/// `edge` is live and its pending confirmation reported a cycle.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_fault(
    edge: *const HewActorWaitEdge,
) -> *mut crate::fault::HewFault {
    // SAFETY: the operation retains the diagnosed edge until this copy returns.
    let edge = unsafe { &*edge };
    Box::into_raw(Box::new(crate::fault::HewFault::with_message(
        crate::internal::types::HEW_TRAP_USER_PANIC,
        edge.diagnostic
            .get()
            .expect("confirmed actor cycle")
            .clone(),
    )))
}

/// Release the registration before the enclosing invocation can finish.
///
/// # Safety
/// The pointer is null or the unique Arc reference returned at construction.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_free(edge: *const HewActorWaitEdge) {
    if edge.is_null() {
        return;
    }
    // SAFETY: this call consumes the operation's one raw Arc reference.
    let edge = unsafe { Arc::from_raw(edge) };
    edge.state.store(STATE_DISARMED, Ordering::SeqCst);
    let shard = &WAITS[shard_index(edge.owner)];
    let mut entries = shard.entries.lock_or_recover();
    if entries
        .get(&edge.owner)
        .is_some_and(|weak| weak.ptr_eq(&Arc::downgrade(&edge)))
    {
        entries.remove(&edge.owner);
        shard.len.fetch_sub(1, Ordering::SeqCst);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn select_requires_every_alternative_to_close_the_actor_knot() {
        for (external_progress, escaping_actor) in [(false, false), (true, false), (false, true)] {
            let a = ActorIncarnation::from_parts(900_001_001, 1);
            let b = ActorIncarnation::from_parts(900_001_002, 2);
            let c = ActorIncarnation::from_parts(900_001_003, 3);
            let outside = ActorIncarnation::from_parts(900_001_004, 4);
            let select = select_alternatives(a, vec![b, c], external_progress);
            let first = Arc::into_raw(HewActorWaitEdge::register(b, a, 0));
            let second = Arc::into_raw(HewActorWaitEdge::register(
                c,
                if escaping_actor { outside } else { a },
                0,
            ));
            // SAFETY: the fixture exclusively drives each live registration.
            unsafe {
                hew_actor_wait_edge_prepare(first);
                assert_eq!(hew_actor_wait_edge_pending(first), 0);
                hew_actor_wait_edge_prepare(second);
                assert_eq!(hew_actor_wait_edge_pending(second), 0);
                hew_actor_wait_edge_prepare(select);
                let cycle = hew_actor_wait_edge_pending(select);
                assert_eq!(cycle != 0, !external_progress && !escaping_actor);
                if cycle != 0 {
                    let fault = hew_actor_wait_edge_fault(select);
                    crate::fault::hew_fault_drop(fault);
                }
                hew_actor_wait_edge_free(select);
                hew_actor_wait_edge_free(first);
                hew_actor_wait_edge_free(second);
            }
        }
    }

    #[test]
    fn readiness_breaks_old_dependencies_before_actor_resumption() {
        let a = ActorIncarnation::from_parts(900_000_001, 1);
        let b = ActorIncarnation::from_parts(900_000_002, 2);
        let first = Arc::into_raw(HewActorWaitEdge::register(a, b, 0));
        let second = Arc::into_raw(HewActorWaitEdge::register(b, a, 2));
        // SAFETY: each pointer is an exclusively driven, live registration.
        unsafe {
            hew_actor_wait_edge_prepare(first);
            assert_eq!(hew_actor_wait_edge_pending(first), 0);
            ready(a);
            hew_actor_wait_edge_prepare(second);
            assert_eq!(hew_actor_wait_edge_pending(second), 0);
            hew_actor_wait_edge_prepare(first);
            assert_eq!(hew_actor_wait_edge_pending(first), 1);
            let fault = hew_actor_wait_edge_fault(first);
            crate::fault::hew_fault_drop(fault);
            hew_actor_wait_edge_free(first);
            hew_actor_wait_edge_free(second);
            let replacement = ActorIncarnation::from_parts(a.actor_id(), 3);
            let first = Arc::into_raw(HewActorWaitEdge::register(a, b, 1));
            let second = Arc::into_raw(HewActorWaitEdge::register(b, replacement, 1));
            hew_actor_wait_edge_prepare(first);
            assert_eq!(hew_actor_wait_edge_pending(first), 0);
            hew_actor_wait_edge_prepare(second);
            assert_eq!(hew_actor_wait_edge_pending(second), 0);
            hew_actor_wait_edge_free(first);
            hew_actor_wait_edge_free(second);
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn loom_concurrent_two_actor_cycle_is_never_missed() {
        use loom::sync::atomic::{
            fence as loom_fence, AtomicU8 as LoomAtomicU8, Ordering as LoomOrdering,
        };
        use loom::sync::Arc as LoomArc;

        loom::model(|| {
            let state_a = LoomArc::new(LoomAtomicU8::new(STATE_ARMED));
            let state_b = LoomArc::new(LoomAtomicU8::new(STATE_ARMED));

            let a_for_b = LoomArc::clone(&state_a);
            let b_for_b = LoomArc::clone(&state_b);
            let tb = loom::thread::spawn(move || {
                b_for_b
                    .compare_exchange(
                        STATE_ARMED,
                        STATE_PENDING,
                        LoomOrdering::SeqCst,
                        LoomOrdering::Acquire,
                    )
                    .expect("B transitions armed -> pending");
                loom_fence(LoomOrdering::SeqCst);
                a_for_b.load(LoomOrdering::SeqCst) == STATE_PENDING
            });

            state_a
                .compare_exchange(
                    STATE_ARMED,
                    STATE_PENDING,
                    LoomOrdering::SeqCst,
                    LoomOrdering::Acquire,
                )
                .expect("A transitions armed -> pending");
            loom_fence(LoomOrdering::SeqCst);
            let a_saw_b = state_b.load(LoomOrdering::SeqCst) == STATE_PENDING;
            let b_saw_a = tb.join().expect("thread B");

            assert!(
                a_saw_b || b_saw_a,
                "concurrent 2-cycle must be observed by at least one participant"
            );
        });
    }
}
