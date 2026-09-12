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
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, LazyLock, Mutex, OnceLock, Weak};

#[derive(Debug)]
pub struct HewActorWaitEdge {
    owner: ActorIncarnation,
    targets: Vec<ActorIncarnation>,
    external_progress: bool,
    operation: i32,
    diagnostic: OnceLock<String>,
}

struct Registration {
    edge: Weak<HewActorWaitEdge>,
    armed: bool,
    pending: bool,
}

static WAITS: LazyLock<Mutex<HashMap<ActorIncarnation, Registration>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

impl HewActorWaitEdge {
    fn register(owner: ActorIncarnation, target: ActorIncarnation, operation: i32) -> Arc<Self> {
        Self::register_alternatives(owner, vec![target], false, operation)
    }

    fn register_alternatives(
        owner: ActorIncarnation,
        targets: Vec<ActorIncarnation>,
        external_progress: bool,
        operation: i32,
    ) -> Arc<Self> {
        let edge = Arc::new(Self {
            owner,
            targets,
            external_progress,
            operation,
            diagnostic: OnceLock::new(),
        });
        WAITS.lock_or_recover().insert(
            owner,
            Registration {
                edge: Arc::downgrade(&edge),
                armed: false,
                pending: false,
            },
        );
        edge
    }
}

/// Invalidate before readiness makes the actor runnable. The operation arms
/// its candidate again before the next poll, then confirms only a pending poll.
pub(crate) fn ready(owner: ActorIncarnation) {
    if let Some(registration) = WAITS.lock_or_recover().get_mut(&owner) {
        registration.armed = false;
        registration.pending = false;
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
    let owner = unsafe { &*state }.actor_turn;
    if owner.is_none() {
        return std::ptr::null();
    }
    let target = resolve_target(target);
    target.map_or(std::ptr::null(), |target| {
        Arc::into_raw(HewActorWaitEdge::register(owner, target, operation))
    })
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
    if let Some(registration) = WAITS
        .lock_or_recover()
        .get_mut(&edge.owner)
        .filter(|registration| std::ptr::eq(registration.edge.as_ptr(), edge))
    {
        registration.armed = true;
        registration.pending = false;
    }
}

/// Confirm a pending predicate and return 1 only when it closes a local cycle.
/// Wakes and confirmations share the graph lock; neither can resurrect a wake
/// that happened between predicate inspection and this confirmation.
///
/// # Safety
/// The pointer is null or a live edge prepared before the last pending poll.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_pending(edge: *const HewActorWaitEdge) -> i32 {
    // SAFETY: the operation holds its edge for this confirmation.
    let Some(edge) = (unsafe { edge.as_ref() }) else {
        return 0;
    };
    let mut graph = WAITS.lock_or_recover();
    let Some(registration) = graph
        .get_mut(&edge.owner)
        .filter(|registration| std::ptr::eq(registration.edge.as_ptr(), edge))
    else {
        return 0;
    };
    if !registration.armed {
        return 0;
    }
    registration.pending = true;
    // A turn is proven blocked only when every alternative is another proven
    // blocked turn. Remove escapes to readiness, independent sources, timers
    // and unregistered actors until the remaining closed set stops shrinking.
    let active: HashMap<_, _> = graph
        .iter()
        .filter(|(_, registration)| registration.armed && registration.pending)
        .filter_map(|(owner, registration)| registration.edge.upgrade().map(|edge| (*owner, edge)))
        .collect();
    let mut blocked: HashSet<_> = active
        .iter()
        .filter(|(_, edge)| !edge.external_progress && !edge.targets.is_empty())
        .map(|(owner, _)| *owner)
        .collect();
    loop {
        let escaped: Vec<_> = blocked
            .iter()
            .filter(|owner| {
                active[owner]
                    .targets
                    .iter()
                    .any(|target| !blocked.contains(target))
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
        next = active[&next].targets[0];
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
    graph.get_mut(&edge.owner).unwrap().pending = false;
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
    let mut graph = WAITS.lock_or_recover();
    if graph
        .get(&edge.owner)
        .is_some_and(|r| r.edge.ptr_eq(&Arc::downgrade(&edge)))
    {
        graph.remove(&edge.owner);
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
}
