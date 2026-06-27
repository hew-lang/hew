//! Actor links implementation for fail-together semantics.
//!
//! In Erlang-style actor systems, links are bidirectional: when one linked
//! actor crashes, all linked actors also crash (unless they are trapping exits).
//! This module implements the link table and crash propagation logic.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::LazyLock;
#[cfg(test)]
use std::sync::{Arc, Barrier, Mutex};

use crate::actor::HewActor;
use crate::internal::types::HewActorState;
use crate::lifetime::PoisonSafeRw;
use crate::mailbox;
use crate::supervisor::SYS_MSG_EXIT;

/// Number of shards for link table to reduce contention.
const LINK_SHARDS: usize = 16;

/// Entry in the link table mapping `actor_id` -> linked actors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LinkedActorEntry {
    /// Actor ID for O(1) liveness lookup in propagation paths.
    linked_actor_id: u64,
    /// Linked actor address retained for pointer confirmation.
    linked_actor: usize,
}

/// Entry in the link table mapping `actor_id` -> linked actors.
#[derive(Debug)]
struct LinkShard {
    /// Maps `actor_id` to Vec of actors linked to that actor.
    /// Using usize instead of *mut `HewActor` for thread safety.
    links: HashMap<u64, Vec<LinkedActorEntry>>,
    /// Tracks actors whose terminal EXIT propagation has already completed.
    terminal_exits: HashMap<u64, i32>,
}

/// Global sharded link table.
/// We use `PoisonSafeRw` (instead of a raw `RwLock`) intentionally: link state
/// must remain recoverable even if a panic poisons a lock while mutating a shard.
/// The runtime's poison-recovery discipline is to continue operating with
/// conservatively recovered state rather than cascading a poisoning failure,
/// consistent with the rationale documented in `poison_safe.rs`.
/// We use usize to store actor pointers to make it Send+Sync safe.
/// The runtime guarantees actors remain valid while linked.
// native-only: LINK_TABLE state does not exist in the single-threaded WASM model
static LINK_TABLE: LazyLock<[PoisonSafeRw<LinkShard>; LINK_SHARDS]> = LazyLock::new(|| {
    std::array::from_fn(|_| {
        PoisonSafeRw::new(LinkShard {
            links: HashMap::new(),
            terminal_exits: HashMap::new(),
        })
    })
});

/// Get shard index for an actor ID.
fn get_shard_index(actor_id: u64) -> usize {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "shard index is bounded by LINK_SHARDS (16)"
    )]
    {
        (actor_id as usize) % LINK_SHARDS
    }
}

/// Create a bidirectional link between two actors.
///
/// # Safety
///
/// Both `a` and `b` must be valid pointers to [`HewActor`] structs.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_link(a: *mut HewActor, b: *mut HewActor) {
    if a.is_null() || b.is_null() || a == b {
        return;
    }

    // SAFETY: Caller guarantees `a` is a valid pointer.
    let actor_a = unsafe { &*a };
    // SAFETY: Caller guarantees `b` is a valid pointer.
    let actor_b = unsafe { &*b };

    let id_a = actor_a.id;
    let id_b = actor_b.id;
    let shard_index_a = get_shard_index(id_a);
    let shard_index_b = get_shard_index(id_b);

    let (a_terminal_reason, b_terminal_reason) = match shard_index_a.cmp(&shard_index_b) {
        std::cmp::Ordering::Equal => LINK_TABLE[shard_index_a].access(|shard| {
            let a_terminal_reason = terminal_exit_reason(shard, id_a, actor_a);
            let b_terminal_reason = terminal_exit_reason(shard, id_b, actor_b);

            if a_terminal_reason.is_none() && b_terminal_reason.is_none() {
                add_link_locked(shard, id_a, id_b, b);
                add_link_locked(shard, id_b, id_a, a);
            }

            (a_terminal_reason, b_terminal_reason)
        }),
        std::cmp::Ordering::Less => LINK_TABLE[shard_index_a].access(|shard_a| {
            LINK_TABLE[shard_index_b].access(|shard_b| {
                let a_terminal_reason = terminal_exit_reason(shard_a, id_a, actor_a);
                let b_terminal_reason = terminal_exit_reason(shard_b, id_b, actor_b);

                if a_terminal_reason.is_none() && b_terminal_reason.is_none() {
                    add_link_locked(shard_a, id_a, id_b, b);
                    add_link_locked(shard_b, id_b, id_a, a);
                }

                (a_terminal_reason, b_terminal_reason)
            })
        }),
        std::cmp::Ordering::Greater => LINK_TABLE[shard_index_b].access(|shard_b| {
            LINK_TABLE[shard_index_a].access(|shard_a| {
                let a_terminal_reason = terminal_exit_reason(shard_a, id_a, actor_a);
                let b_terminal_reason = terminal_exit_reason(shard_b, id_b, actor_b);

                if a_terminal_reason.is_none() && b_terminal_reason.is_none() {
                    add_link_locked(shard_a, id_a, id_b, b);
                    add_link_locked(shard_b, id_b, id_a, a);
                }

                (a_terminal_reason, b_terminal_reason)
            })
        }),
    };

    if let (Some(reason), None) = (a_terminal_reason, b_terminal_reason) {
        send_exit_signal(id_b, b, id_a, reason);
    } else if let (None, Some(reason)) = (a_terminal_reason, b_terminal_reason) {
        send_exit_signal(id_a, a, id_b, reason);
    }
}

/// Remove a bidirectional link between two actors.
///
/// # Safety
///
/// Both `a` and `b` must be valid pointers to [`HewActor`] structs.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_unlink(a: *mut HewActor, b: *mut HewActor) {
    if a.is_null() || b.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `a` is a valid pointer.
    let actor_a = unsafe { &*a };
    // SAFETY: Caller guarantees `b` is a valid pointer.
    let actor_b = unsafe { &*b };

    let id_a = actor_a.id;
    let id_b = actor_b.id;

    // Remove bidirectional links: A -/-> B and B -/-> A
    remove_link(id_a, b);
    remove_link(id_b, a);
}

fn add_link_locked(shard: &mut LinkShard, from_id: u64, to_actor_id: u64, to_actor: *mut HewActor) {
    shard
        .links
        .entry(from_id)
        .or_default()
        .push(LinkedActorEntry {
            linked_actor_id: to_actor_id,
            linked_actor: to_actor as usize,
        });
}

fn terminal_exit_reason(shard: &LinkShard, actor_id: u64, actor: &HewActor) -> Option<i32> {
    if let Some(&reason) = shard.terminal_exits.get(&actor_id) {
        return Some(reason);
    }

    let actor_state = actor.actor_state.load(std::sync::atomic::Ordering::Acquire);
    if actor_state == HewActorState::Stopped as i32 || actor_state == HewActorState::Crashed as i32
    {
        Some(actor.error_code.load(std::sync::atomic::Ordering::Acquire))
    } else {
        None
    }
}

fn send_exit_signal(
    linked_actor_id: u64,
    linked_actor: *mut HewActor,
    crashed_actor_id: u64,
    reason: i32,
) {
    if linked_actor.is_null() {
        return;
    }

    crate::actor::with_live_actor_by_id(linked_actor_id, linked_actor, |linked_actor_ref| {
        let mailbox = linked_actor_ref.mailbox.cast::<mailbox::HewMailbox>();
        if !mailbox.is_null() {
            let exit_data = ExitMessage {
                crashed_actor_id,
                reason,
                // M-7 substrate: precompute the CrashKind tag (the M-6
                // projection) so the future typed `#[on(exit)]` hook reads
                // `CrashNotification.kind` directly.
                crash_kind: crate::internal::types::CrashKind::tag_from_error_code(reason),
            };

            let data_ptr = (&raw const exit_data).cast::<c_void>();
            let data_size = std::mem::size_of::<ExitMessage>();

            // SAFETY: LIVE_ACTORS keeps the linked actor and mailbox live.
            unsafe {
                mailbox::hew_mailbox_send_sys(
                    mailbox,
                    SYS_MSG_EXIT,
                    data_ptr.cast_mut(),
                    data_size,
                );
            }

            if linked_actor_ref
                .actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Runnable as i32,
                    std::sync::atomic::Ordering::AcqRel,
                    std::sync::atomic::Ordering::Acquire,
                )
                .is_ok()
            {
                linked_actor_ref
                    .idle_count
                    .store(0, std::sync::atomic::Ordering::Relaxed);
                linked_actor_ref
                    .hibernating
                    .store(0, std::sync::atomic::Ordering::Relaxed);
                crate::scheduler::sched_enqueue(linked_actor);
            }
        }

        #[cfg(test)]
        run_propagate_exit_hook();
    });
}

/// Remove a unidirectional link: `from_id` -/-> `to_actor`.
fn remove_link(from_id: u64, to_actor: *mut HewActor) {
    let shard_index = get_shard_index(from_id);
    LINK_TABLE[shard_index].access(|shard| {
        if let Some(linked_actors) = shard.links.get_mut(&from_id) {
            let target_addr = to_actor as usize;
            linked_actors.retain(|entry| entry.linked_actor != target_addr);
            if linked_actors.is_empty() {
                shard.links.remove(&from_id);
            }
        }
    });
}

/// Propagate exit signal to all linked actors when an actor crashes.
///
/// This function is called from `hew_actor_trap` after the actor has
/// transitioned to a terminal state. It removes all links for the
/// crashing actor to prevent infinite propagation loops, then sends
/// EXIT messages to all linked actors.
pub(crate) fn propagate_exit_to_links(actor_id: u64, reason: i32) {
    let shard_index = get_shard_index(actor_id);

    // Take all linked actors for this actor ID to prevent re-entrancy.
    let linked_actors = LINK_TABLE[shard_index].access(|shard| {
        let linked_actors = shard.links.remove(&actor_id).unwrap_or_default();
        shard.terminal_exits.insert(actor_id, reason);
        linked_actors
    });

    // Send EXIT messages to all linked actors.
    for linked_actor_entry in linked_actors {
        if linked_actor_entry.linked_actor == 0 {
            continue;
        }

        let linked_actor = linked_actor_entry.linked_actor as *mut HewActor;
        let linked_id = linked_actor_entry.linked_actor_id;

        // Remove the reverse link: linked_actor -/-> crashing_actor
        remove_link_by_target(linked_id, actor_id);
        send_exit_signal(linked_id, linked_actor, actor_id, reason);
    }
}

/// `PartitionPolicy::CrashLinked` discriminant (mirrors
/// `std/link_monitor.hew::PartitionPolicy` / `execution_context::PartitionPolicy`,
/// where `CrashLinked == 3`). A cross-node link with this policy crashes the
/// LOCAL linked actor when its remote peer dies; any other policy is non-fatal.
pub(crate) const POLICY_TAG_CRASH_LINKED: u8 = 3;

/// Fire a cross-node link-down into a LOCAL linked actor (DIST-9).
///
/// This is the cross-node mirror of [`send_exit_signal`]: when a remote linked
/// peer reaches a terminal state, a `CrashLinked` link must synthesize a
/// `SYS_MSG_EXIT` into the LOCAL linked actor's MAILBOX and crash it — the OTP
/// fail-together semantic. Unlike the local link table (keyed by `*mut HewActor`
/// pointers), the cross-node entry stores only the local actor id, so we resolve
/// the live pointer by id before reusing the same EXIT-synthesis path.
///
/// `crashed_remote_serial` is the dead remote peer's serial (carried into the
/// `ExitMessage` for the typed `#[on(exit)]` consumer). `reason` is the terminal
/// reason (a `HewActorState` value, or a partition sentinel).
///
/// Non-`CrashLinked` policies are non-fatal: the link is recorded but the
/// remote's death does not crash the local actor (it survives). This is the
/// policy discriminator that the negative two-process fixture pins.
///
/// Returns true if a fatal EXIT was synthesized (`CrashLinked` + a live local
/// actor); false if the policy was non-fatal or the local actor is already gone.
pub(crate) fn deliver_cross_node_link_exit(
    local_actor_id: u64,
    crashed_remote_serial: u64,
    reason: i32,
    policy_tag: u8,
) -> bool {
    if policy_tag != POLICY_TAG_CRASH_LINKED {
        // Non-CrashLinked link: the remote's death is non-fatal to the local
        // actor (it survives). Recorded for completeness; no EXIT synthesized.
        return false;
    }
    // Resolve the live local actor by id; if it is already terminal/freed, there
    // is nothing to crash (idempotent / fail-closed — no fabricated EXIT).
    let Some(actor_ptr) = crate::lifetime::live_actors::get_actor_ptr_by_id(local_actor_id) else {
        return false;
    };
    // Reuse the proven local EXIT-synthesis path (same SYS_MSG_EXIT + CrashKind
    // projection + Idle→Runnable wake), so supervision / exit-trapping behaves
    // identically to a local link EXIT. `with_live_actor_by_id` inside
    // `send_exit_signal` re-validates liveness under the registry lock.
    send_exit_signal(local_actor_id, actor_ptr, crashed_remote_serial, reason);
    true
}

#[cfg(test)]
type PropagateExitHook = Option<(Arc<Barrier>, Arc<Barrier>)>;

#[cfg(test)]
static PROPAGATE_EXIT_HOOK: LazyLock<Mutex<PropagateExitHook>> = LazyLock::new(|| Mutex::new(None));

#[cfg(test)]
fn run_propagate_exit_hook() {
    let hook = {
        let guard = PROPAGATE_EXIT_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        guard.clone()
    };

    if let Some((entered, release)) = hook {
        entered.wait();
        release.wait();
    }
}

#[cfg(test)]
struct PropagateExitHookGuard;

#[cfg(test)]
impl PropagateExitHookGuard {
    fn install(entered: Arc<Barrier>, release: Arc<Barrier>) -> Self {
        let mut guard = PROPAGATE_EXIT_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard = Some((entered, release));
        Self
    }
}

#[cfg(test)]
impl Drop for PropagateExitHookGuard {
    fn drop(&mut self) {
        let mut guard = PROPAGATE_EXIT_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard = None;
    }
}

/// Remove all links where the target is the specified actor ID.
/// This is used to clean up reverse links when an actor exits.
fn remove_link_by_target(from_id: u64, target_id: u64) {
    let shard_index = get_shard_index(from_id);
    LINK_TABLE[shard_index].access(|shard| {
        if let Some(linked_actors) = shard.links.get_mut(&from_id) {
            linked_actors.retain(|entry| {
                if entry.linked_actor == 0 {
                    return false;
                }
                entry.linked_actor_id != target_id
            });

            if linked_actors.is_empty() {
                shard.links.remove(&from_id);
            }
        }
    });
}

/// Remove all link entries for a given actor (by ID) and purge its address
/// from every other actor's link list across all shards.
///
/// Called from `hew_actor_free` before deallocation to prevent link
/// propagation from dereferencing freed memory.
pub(crate) fn remove_all_links_for_actor(actor_id: u64, actor_addr: *mut HewActor) {
    let actor_usize = actor_addr as usize;

    // Remove the actor's own link-list entry from its shard.
    let own_shard = get_shard_index(actor_id);
    LINK_TABLE[own_shard].access(|shard| {
        shard.terminal_exits.remove(&actor_id);
        shard.links.remove(&actor_id);
    });

    // Scan all shards and remove this actor's address from other actors'
    // link lists. This is O(shards × entries) but actors rarely have many
    // links, and this only runs at free time.
    for shard_rw in LINK_TABLE.iter() {
        shard_rw.access(|shard| {
            shard.links.retain(|_id, linked_actors| {
                linked_actors.retain(|entry| entry.linked_actor != actor_usize);
                !linked_actors.is_empty()
            });
        });
    }
}

/// Message data for EXIT system messages.
///
/// M-7 typed-payload substrate: this is the runtime mirror of the
/// `std/failure.hew::CrashNotification { actor_id, kind }` a linked actor's
/// exit hook will receive once the `#[on(exit)]` user surface is ratified
/// (M-7-R). `crash_kind` is the M-6 `ExitReason -> CrashKind` projection of
/// `reason`, precomputed at the delivery boundary so the (future) typed hook
/// reads a `CrashKind` tag directly without re-deriving the projection. The
/// raw `reason` is retained for the existing integer-tag consumers.
#[repr(C)]
#[derive(Debug)]
struct ExitMessage {
    /// ID of the actor that crashed and caused this exit signal. Maps to
    /// `CrashNotification.actor_id`.
    crashed_actor_id: u64,
    /// Reason code (`error_code` from `hew_actor_trap`).
    reason: i32,
    /// The M-6 `CrashKind` tag projected from `reason` (Crashed=0,
    /// HeapExceeded=1, PartitionDetected=2). Maps to `CrashNotification.kind`.
    crash_kind: i32,
}

// ── Test-only crash ledger + probe FFI (DIST-9 link cascade observability) ────
//
// The cross-node link fixtures need to confirm a LOCAL linked actor actually
// CRASHED (terminal Crashed) after a cross-node link-down — proving the link-down
// landed in the mailbox as a SYS_MSG_EXIT and crashed the actor, not in a monitor
// recv slot. A crashed actor is freed, so the death must be recorded somewhere it
// survives the free. This ledger does that, gated by HEW_LINK_PROBE so production
// pays nothing (the env is read once into a LazyLock).

/// Sentinel returned by `hew_link_probe_terminal_state` when the actor has not
/// (yet) reached a terminal state recorded in the probe ledger.
const LINK_PROBE_NOT_TERMINAL: i64 = -999;

/// Whether the probe ledger is active (`HEW_LINK_PROBE` set). Read once.
static LINK_PROBE_ENABLED: LazyLock<bool> =
    LazyLock::new(|| std::env::var_os("HEW_LINK_PROBE").is_some());

/// `actor_id -> terminal reason`, populated by `propagate_exit_to_links` when the
/// probe is enabled. Survives the actor's free so a fixture can poll it.
static LINK_PROBE_LEDGER: LazyLock<std::sync::Mutex<HashMap<u64, i32>>> =
    LazyLock::new(|| std::sync::Mutex::new(HashMap::new()));

/// Record an actor's TERMINAL STATE (`HewActorState`: Crashed == 5, Stopped ==
/// 6) in the probe ledger (no-op unless enabled). Called from `hew_actor_trap`
/// with the terminal state — distinct from the link EXIT reason — so a fixture
/// can assert the local linked actor reached `Crashed`, proving the mailbox-EXIT
/// cascade drove a controlled crash (not the exhaustiveness `llvm.trap`).
pub(crate) fn record_link_probe_terminal(actor_id: u64, terminal_state: i32) {
    if !*LINK_PROBE_ENABLED {
        return;
    }
    let mut ledger = LINK_PROBE_LEDGER
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ledger.insert(actor_id, terminal_state);
}

/// Test-introspection probe: report the recorded terminal reason for `actor_id`,
/// or [`LINK_PROBE_NOT_TERMINAL`] if it has not reached a terminal state. Used by
/// the two-process link fixtures to assert the LOCAL linked actor crashed
/// (reason == `HewActorState::Crashed` == 5). Not user-callable; the compiler emits
/// no calls to this symbol. Callable from `.hew` via
/// `extern "C" { fn hew_link_probe_terminal_state(actor_id: i64) -> i64; }`.
#[no_mangle]
pub extern "C" fn hew_link_probe_terminal_state(actor_id: i64) -> i64 {
    #[expect(
        clippy::cast_sign_loss,
        reason = "actor_id originates from a u64 actor counter reinterpreted as i64 on the Hew side"
    )]
    let actor_id = actor_id as u64;
    let ledger = LINK_PROBE_LEDGER
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ledger
        .get(&actor_id)
        .map_or(LINK_PROBE_NOT_TERMINAL, |&reason| i64::from(reason))
}

/// Returns true if any link entries reference the given actor address.
#[cfg(test)]
pub(crate) fn has_links_for_actor(actor_id: u64, actor_addr: *mut HewActor) -> bool {
    let actor_usize = actor_addr as usize;
    let own_shard = get_shard_index(actor_id);
    if LINK_TABLE[own_shard].read_access(|shard| shard.links.contains_key(&actor_id)) {
        return true;
    }
    // Check if this actor appears as a target in any other actor's link list.
    for shard_rw in LINK_TABLE.iter() {
        let hit = shard_rw.read_access(|shard| {
            shard
                .links
                .values()
                .any(|linked| linked.iter().any(|entry| entry.linked_actor == actor_usize))
        });
        if hit {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64};
    use std::time::Duration;

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    fn create_test_actor(id: u64) -> HewActor {
        HewActor {
            sched_link_next: AtomicPtr::new(std::ptr::null_mut()),
            id,
            state: std::ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: std::ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Idle as i32),
            budget: AtomicI32::new(0),
            init_state: std::ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: std::ptr::null_mut(),
            supervisor_child_index: 0,
            priority: AtomicI32::new(1),
            reductions: AtomicI32::new(0),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            arena: std::ptr::null_mut(),
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: std::ptr::null(),
            send_pin_count: std::sync::atomic::AtomicU32::new(0),
        }
    }

    #[test]
    fn test_link_creation_and_removal() {
        let mut actor_a = create_test_actor(100);
        let mut actor_b = create_test_actor(200);

        let a_ptr = &raw mut actor_a;
        let b_ptr = &raw mut actor_b;

        // Create bidirectional link
        // SAFETY: a_ptr and b_ptr are valid pointers to stack-allocated test actors.
        unsafe {
            hew_actor_link(a_ptr, b_ptr);
        }

        // Verify links exist
        let shard_a = get_shard_index(100);
        let shard_b = get_shard_index(200);

        LINK_TABLE[shard_a].read_access(|table_a| {
            assert!(table_a
                .links
                .get(&100)
                .is_some_and(|v| v.contains(&LinkedActorEntry {
                    linked_actor_id: 200,
                    linked_actor: b_ptr as usize,
                })));
        });
        LINK_TABLE[shard_b].read_access(|table_b| {
            assert!(table_b
                .links
                .get(&200)
                .is_some_and(|v| v.contains(&LinkedActorEntry {
                    linked_actor_id: 100,
                    linked_actor: a_ptr as usize,
                })));
        });

        // Remove link
        // SAFETY: a_ptr and b_ptr are valid pointers to stack-allocated test actors.
        unsafe {
            hew_actor_unlink(a_ptr, b_ptr);
        }

        // Verify links are removed
        LINK_TABLE[shard_a].read_access(|table_a| {
            assert!(!table_a
                .links
                .get(&100)
                .is_some_and(|v| v.iter().any(|entry| entry.linked_actor == b_ptr as usize)));
        });
        LINK_TABLE[shard_b].read_access(|table_b| {
            assert!(!table_b
                .links
                .get(&200)
                .is_some_and(|v| v.iter().any(|entry| entry.linked_actor == a_ptr as usize)));
        });
    }

    #[test]
    fn test_null_actor_handling() {
        let mut actor = create_test_actor(300);
        let actor_ptr = &raw mut actor;

        // These should not panic
        // SAFETY: Testing null pointer handling; functions handle null gracefully.
        unsafe {
            hew_actor_link(std::ptr::null_mut(), actor_ptr);
            hew_actor_link(actor_ptr, std::ptr::null_mut());
            hew_actor_link(std::ptr::null_mut(), std::ptr::null_mut());

            hew_actor_unlink(std::ptr::null_mut(), actor_ptr);
            hew_actor_unlink(actor_ptr, std::ptr::null_mut());
            hew_actor_unlink(std::ptr::null_mut(), std::ptr::null_mut());
        }

        // Self-linking should be ignored
        // SAFETY: actor_ptr is a valid pointer; self-link is a no-op.
        unsafe {
            hew_actor_link(actor_ptr, actor_ptr);
        }

        let shard = get_shard_index(300);
        LINK_TABLE[shard].read_access(|table| assert!(!table.links.contains_key(&300)));
    }

    /// Link operations survive a poisoned `RwLock` shard.
    #[test]
    fn link_survives_poisoned_shard() {
        // Poison a standalone RwLock to prove the pattern works.
        let lock = std::sync::RwLock::new(42);
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _guard = lock.write().unwrap();
            panic!("intentional poison");
        }));
        assert!(lock.is_poisoned());

        // The global LINK_TABLE wraps each shard in PoisonSafeRw, so
        // link/unlink must not panic even if another thread poisoned a shard.
        let mut actor_a = create_test_actor(900);
        let mut actor_b = create_test_actor(901);

        let a_ptr = &raw mut actor_a;
        let b_ptr = &raw mut actor_b;

        // SAFETY: a_ptr and b_ptr are valid pointers to stack-allocated test actors.
        unsafe {
            hew_actor_link(a_ptr, b_ptr);
            hew_actor_unlink(a_ptr, b_ptr);
        }
    }

    #[test]
    fn remove_all_links_clears_both_directions() {
        // Use unique IDs (30xxx) to avoid collisions with parallel tests.
        let mut actor_a = create_test_actor(30_100);
        let mut actor_b = create_test_actor(30_200);
        let a_ptr = &raw mut actor_a;
        let b_ptr = &raw mut actor_b;

        // SAFETY: Both are valid stack-allocated test actors.
        unsafe { hew_actor_link(a_ptr, b_ptr) };

        // Precondition: both directions exist.
        assert!(has_links_for_actor(30_100, a_ptr));
        assert!(has_links_for_actor(30_200, b_ptr));

        // Act: remove all links for actor A (simulating hew_actor_free).
        remove_all_links_for_actor(30_100, a_ptr);

        // Actor A's own entry and its address in B's list should both be gone.
        assert!(
            !has_links_for_actor(30_100, a_ptr),
            "actor A should have no link entries after cleanup"
        );

        // Actor B's own entry that pointed to A should also be gone.
        let shard_b = get_shard_index(30_200);
        LINK_TABLE[shard_b].read_access(|table_b| {
            let b_links = table_b.links.get(&30_200);
            assert!(
                b_links.is_none()
                    || !b_links
                        .unwrap()
                        .iter()
                        .any(|entry| entry.linked_actor == a_ptr as usize),
                "actor B's link list should no longer reference actor A"
            );
        });
    }

    #[test]
    fn remove_all_links_no_links_does_not_panic() {
        // An actor with no links — cleanup should be a no-op.
        let actor = create_test_actor(30_300);
        let ptr = (&raw const actor).cast_mut();
        remove_all_links_for_actor(30_300, ptr);
        assert!(!has_links_for_actor(30_300, ptr));
    }

    #[test]
    fn late_link_to_terminal_actor_skips_stale_entries_and_cleans_tombstone() {
        // `propagate_exit_to_links` probes the runtime-owned live-actor registry.
        let _rt = crate::runtime_test_guard();
        let mut survivor = create_test_actor(30_400);
        let mut terminal = create_test_actor(30_401);
        let survivor_ptr = &raw mut survivor;
        let terminal_ptr = &raw mut terminal;

        terminal.actor_state.store(
            HewActorState::Crashed as i32,
            std::sync::atomic::Ordering::Release,
        );
        terminal
            .error_code
            .store(77, std::sync::atomic::Ordering::Release);
        propagate_exit_to_links(30_401, 77);

        // SAFETY: Both are valid stack-allocated test actors.
        unsafe { hew_actor_link(survivor_ptr, terminal_ptr) };

        assert!(
            !has_links_for_actor(30_400, survivor_ptr),
            "late link should not create survivor-side stale entries"
        );
        assert!(
            !has_links_for_actor(30_401, terminal_ptr),
            "late link should not recreate terminal-side stale entries"
        );

        let terminal_shard = get_shard_index(30_401);
        LINK_TABLE[terminal_shard].read_access(|shard| {
            assert_eq!(shard.terminal_exits.get(&30_401), Some(&77));
        });

        remove_all_links_for_actor(30_401, terminal_ptr);

        LINK_TABLE[terminal_shard].read_access(|shard| {
            assert!(
                !shard.terminal_exits.contains_key(&30_401),
                "actor cleanup must clear terminal EXIT tombstones"
            );
        });
    }

    #[test]
    fn propagate_exit_keeps_actor_live_through_sys_send() {
        let _guard = crate::runtime_test_guard();
        let entered = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));
        let _hook = PropagateExitHookGuard::install(Arc::clone(&entered), Arc::clone(&release));

        // SAFETY: spawned actors are owned by this test and freed before return.
        unsafe {
            let linked_actor =
                crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
            let target =
                crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!linked_actor.is_null());
            assert!(!target.is_null());

            (*linked_actor).actor_state.store(
                HewActorState::Runnable as i32,
                std::sync::atomic::Ordering::Release,
            );

            hew_actor_link(linked_actor, target);
            let target_id = (*target).id;

            let propagate = std::thread::spawn(move || propagate_exit_to_links(target_id, 91));
            entered.wait();

            // The EXIT message has now been delivered: `propagate` is parked
            // in the hook still holding the LIVE_ACTORS lock, so the
            // `hew_mailbox_send_sys` call already completed. Verify delivery
            // from the main thread *before* spawning the free thread. The
            // thread spawn below is a happens-before edge, so this read +
            // node free is ordered ahead of the free thread's later mailbox
            // teardown and the two never race. (Reading the mailbox
            // concurrently with a pending free instead would race its drain,
            // ordered only by the std `Barrier` the runtime cannot expose to
            // ThreadSanitizer.)
            let mailbox = (*linked_actor).mailbox.cast::<mailbox::HewMailbox>();
            let node = mailbox::hew_mailbox_try_recv_sys(mailbox);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, SYS_MSG_EXIT);
            let payload = &*((*node).data.cast::<ExitMessage>());
            assert_eq!(payload.crashed_actor_id, target_id);
            assert_eq!(payload.reason, 91);
            // M-7 substrate: the delivered EXIT message carries the projected
            // CrashKind tag. reason 91 is not a known trap code → Signal(91) →
            // CrashKind::Crashed (tag 0). Exact-value (wire-contract-presence).
            assert_eq!(
                payload.crash_kind,
                crate::internal::types::CrashKind::Crashed.tag(),
                "EXIT message must carry the projected CrashKind tag (Crashed for a generic signal)"
            );
            mailbox::hew_msg_node_free(node);

            (*linked_actor).actor_state.store(
                HewActorState::Idle as i32,
                std::sync::atomic::Ordering::Release,
            );

            let free_started = Arc::new(AtomicBool::new(false));
            let free_done = Arc::new(AtomicBool::new(false));
            let linked_actor_addr = linked_actor as usize;
            let free_started_thread = Arc::clone(&free_started);
            let free_done_thread = Arc::clone(&free_done);
            let free_handle = std::thread::spawn(move || {
                free_started_thread.store(true, std::sync::atomic::Ordering::Release);
                let rc = crate::actor::hew_actor_free(linked_actor_addr as *mut HewActor);
                assert_eq!(rc, 0);
                free_done_thread.store(true, std::sync::atomic::Ordering::Release);
            });

            while !free_started.load(std::sync::atomic::Ordering::Acquire) {
                std::thread::yield_now();
            }

            std::thread::sleep(Duration::from_millis(50));
            assert!(
                !free_done.load(std::sync::atomic::Ordering::Acquire),
                "hew_actor_free must wait until propagate_exit_to_links releases LIVE_ACTORS"
            );

            release.wait();
            propagate.join().unwrap();
            free_handle.join().unwrap();

            assert_eq!(crate::actor::hew_actor_free(target), 0);
        }
    }
}
