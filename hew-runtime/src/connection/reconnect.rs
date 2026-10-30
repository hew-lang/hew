//! Reconnect policy, backoff and the reconnect worker.

use std::ffi::{c_int, CStr};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use rand::rng;
use rand::RngExt;

use crate::peer_binding::{ClaimState, ConnectionOrigin};
use crate::set_last_error;
use crate::transport::HEW_CONN_INVALID;

use super::admission::{add_connection, Admitted};
use super::handshake::close_transport_conn;
use super::identity_claim::AdmissionStart;
use super::manager::{apply_reconnect, hew_connmgr_expect_peer};
use super::{
    HewConnMgr, ReconnectOwner, ReconnectPlan, SendConnMgr, RECONNECT_DEFAULT_MAX_RETRIES,
    RECONNECT_INITIAL_BACKOFF_MS, RECONNECT_JITTER_MAX_PERCENT, RECONNECT_JITTER_MIN_PERCENT,
    RECONNECT_MAX_BACKOFF_MS, RECONNECT_SLEEP_SLICE_MS,
};
use crate::util::MutexExt;

pub(super) fn normalize_max_retries(max_retries: c_int) -> u32 {
    if max_retries <= 0 {
        RECONNECT_DEFAULT_MAX_RETRIES
    } else {
        u32::try_from(max_retries).unwrap_or(RECONNECT_DEFAULT_MAX_RETRIES)
    }
}

fn jittered_backoff_ms(base_ms: u64) -> u64 {
    let mut rng = rng();
    let jitter_pct = rng.random_range(RECONNECT_JITTER_MIN_PERCENT..=RECONNECT_JITTER_MAX_PERCENT);
    let jittered = base_ms.saturating_mul(jitter_pct) / 100;
    jittered.clamp(1, RECONNECT_MAX_BACKOFF_MS)
}

fn sleep_until_retry(shutdown: &AtomicBool, delay_ms: u64) -> bool {
    let deadline = crate::request_deadline::RequestDeadline::after(
        i64::try_from(delay_ms.saturating_mul(1_000_000)).unwrap_or(i64::MAX),
    );
    loop {
        if shutdown.load(Ordering::Acquire) {
            return false;
        }
        let remaining_ns = deadline.remaining().unwrap_or(0);
        if remaining_ns <= 0 {
            return true;
        }
        let remaining = u64::try_from(remaining_ns).unwrap_or(0);
        thread::sleep(Duration::from_nanos(
            remaining.min(RECONNECT_SLEEP_SLICE_MS * 1_000_000),
        ));
    }
}

pub(super) fn collect_finished_reconnect_workers(mgr: &HewConnMgr) {
    mgr.reconnect_workers.access(|workers| {
        let mut idx = 0usize;
        while idx < workers.len() {
            if workers[idx].is_finished() {
                let handle = workers.swap_remove(idx);
                crate::util::report_join_panic("connection reconnect worker", handle.join());
            } else {
                idx += 1;
            }
        }
    });
}

/// Run `work` on a manager-tracked worker; `hew_connmgr_free` joins it.
pub(super) fn track_worker(
    manager: &HewConnMgr,
    name: &str,
    work: impl FnOnce() + Send + 'static,
) -> bool {
    collect_finished_reconnect_workers(manager);
    match thread::Builder::new().name(name.into()).spawn(work) {
        Ok(worker) => {
            manager
                .reconnect_workers
                .access(|workers| workers.push(worker));
            true
        }
        Err(error) => {
            set_last_error(format!("{name} worker: {error}"));
            false
        }
    }
}

/// Issue the next publication token, or `None` once the token space is
/// exhausted.
///
/// The token is the discriminator that makes `(conn_id, publication_token)` an
/// exact identity for one admission, and transport `conn_id`s are recycled. A
/// wrapping counter would eventually reissue a token that a paused teardown or
/// supersede is still holding for a long-dead connection, and that stale
/// operation would then match — and close, or retire — an unrelated successor
/// on the same recycled id. So the counter is checked rather than wrapping: it
/// saturates at `u64::MAX` and issues nothing further, which turns an
/// impossible-in-practice ABA match into a refused admission. No value is ever
/// issued twice, and zero is never issued at all.
pub(super) fn next_publication_token(mgr: &HewConnMgr) -> Option<u64> {
    mgr.next_publication_token
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |issued| {
            issued.checked_add(1)
        })
        .ok()
}

pub(super) fn reconnect_plan(mgr: &HewConnMgr, conn_id: c_int) -> Option<ReconnectPlan> {
    if !mgr.reconnect_enabled.load(Ordering::Acquire)
        || mgr.reconnect_shutdown.load(Ordering::Acquire)
    {
        return None;
    }
    mgr.connections.access(|conns| {
        let conn = conns.iter().find(|c| c.conn_id == conn_id)?;
        if conn.peer_identity.is_some_and(|identity| {
            mgr.seed_identities
                .access(|identities| identities.contains(&identity))
        }) {
            return None;
        }
        let reconnect = conn.reconnect.as_ref()?;
        Some(ReconnectPlan {
            predecessor: ReconnectOwner {
                node_id: conn.peer_identity?,
                session_incarnation: conn.peer_session_incarnation,
                publication_token: conn.publication_token,
            },
            target_addr: reconnect.target_addr.clone(),
            max_retries: reconnect.max_retries.max(1),
            expected_node_id: reconnect.expected_node_id,
        })
    })
}

unsafe fn connect_addr(mgr: *mut HewConnMgr, target_addr: &CStr) -> Result<c_int, String> {
    if mgr.is_null() {
        return Err("manager is null".to_owned());
    }
    // SAFETY: caller guarantees `mgr` remains valid for this call.
    let mgr = unsafe { &*mgr };
    if mgr.transport.is_null() {
        return Err("transport is null".to_owned());
    }
    // SAFETY: transport pointer is valid per manager contract.
    let t = unsafe { &*mgr.transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    let Some(ops) = (unsafe { t.ops.as_ref() }) else {
        return Err("transport ops are null".to_owned());
    };
    let Some(connect_fn) = ops.connect else {
        return Err("transport connect op missing".to_owned());
    };
    // SAFETY: transport impl and C string are valid.
    let conn_id = unsafe { connect_fn(t.r#impl, target_addr.as_ptr()) };
    if conn_id == HEW_CONN_INVALID {
        return Err("transport connect failed".to_owned());
    }
    Ok(conn_id)
}

pub(super) fn spawn_reconnect_worker(mgr: *mut HewConnMgr, conn_id: c_int, plan: ReconnectPlan) {
    if mgr.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mgr` is valid when scheduling workers.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.reconnect_shutdown.load(Ordering::Acquire) {
        return;
    }
    collect_finished_reconnect_workers(mgr_ref);
    let mgr_send = SendConnMgr(mgr);
    let shutdown = Arc::clone(&mgr_ref.reconnect_shutdown);
    let thread_name = format!("hew-reconnect-{conn_id}");
    let handle = thread::Builder::new().name(thread_name).spawn(move || {
        reconnect_worker_loop(mgr_send, shutdown, conn_id, plan);
    });
    match handle {
        Ok(worker) => {
            mgr_ref
                .reconnect_workers
                .access(|workers| workers.push(worker));
        }
        Err(_) => {
            set_last_error(format!(
                "hew_connmgr_reconnect: failed to spawn worker for dropped conn {conn_id}"
            ));
        }
    }
}

/// Outcome of a single [`reconnect_attempt`].
#[derive(Debug, PartialEq, Eq)]
pub(super) enum ReconnectAttemptOutcome {
    /// The reconnected connection was installed and reconnect was re-armed.
    Installed,
    /// The attempt reached `hew_connmgr_expect_peer`/`hew_connmgr_add` and
    /// was rejected there (e.g. a pinned-identity mismatch). The specific
    /// reason is already in `hew_last_error` — set by whichever of those two
    /// calls rejected the attempt.
    Rejected,
    /// The attempt failed before reaching the identity gate (invalid
    /// address, transport connect failure).
    TransportError,
    /// Shutdown or a successor publication made this worker obsolete.
    Cancelled,
}

fn reconnect_may_retry(mgr: &HewConnMgr, plan: &ReconnectPlan) -> bool {
    !mgr.reconnect_shutdown.load(Ordering::Acquire)
        && mgr
            .claims
            .0
            .lock_or_recover()
            .get(&plan.predecessor.node_id)
            .is_none_or(|claim| {
                // A provisional successor can still abort and restore the
                // predecessor; only a resolved claim can obsolete this retry.
                // A claim retired with its member leaves nothing to obsolete
                // it, so the retry continues as a fresh admission.
                claim.state == ClaimState::Reserved || plan.predecessor.matches(claim)
            })
}

/// Runs one reconnect attempt: connect, replay the pinned peer identity
/// (`plan.expected_node_id`) if any, install via `hew_connmgr_add`, and
/// re-arm reconnect on success.
///
/// The retired predecessor remains authoritative through reservation, even
/// when another connection publishes while this attempt is handshaking.
/// Both the worker and tests drive this same install sequence.
pub(super) fn reconnect_attempt(
    mgr_ptr: *mut HewConnMgr,
    plan: &ReconnectPlan,
    dropped_conn_id: c_int,
    attempt: u32,
) -> ReconnectAttemptOutcome {
    crate::transport::with_connection_attempt_deadline(|| {
        reconnect_attempt_inner(mgr_ptr, plan, dropped_conn_id, attempt)
    })
}

fn reconnect_attempt_inner(
    mgr_ptr: *mut HewConnMgr,
    plan: &ReconnectPlan,
    dropped_conn_id: c_int,
    attempt: u32,
) -> ReconnectAttemptOutcome {
    // SAFETY: workers are joined before manager free; tests retain the manager.
    let Some(mgr) = (unsafe { mgr_ptr.as_ref() }) else {
        return ReconnectAttemptOutcome::Cancelled;
    };
    if !reconnect_may_retry(mgr, plan) {
        return ReconnectAttemptOutcome::Cancelled;
    }
    let Ok(target_addr) = std::ffi::CString::new(plan.target_addr.as_str()) else {
        set_last_error(format!(
            "hew_connmgr_reconnect: invalid reconnect address for dropped conn {dropped_conn_id}"
        ));
        return ReconnectAttemptOutcome::TransportError;
    };
    let admission = AdmissionStart::now(mgr, ConnectionOrigin::Dialled);
    // SAFETY: mgr_ptr was checked non-null and remains valid for the connection lifetime.
    let new_conn_id = match unsafe { connect_addr(mgr_ptr, &target_addr) } {
        Ok(new_conn_id) => new_conn_id,
        Err(err) => {
            set_last_error(format!(
                "hew_connmgr_reconnect: attempt {attempt}/{} failed for dropped conn {dropped_conn_id}, addr={}: {err}",
                plan.max_retries, plan.target_addr
            ));
            return ReconnectAttemptOutcome::TransportError;
        }
    };

    if let Some(expected) = plan.expected_node_id {
        // SAFETY: mgr_ptr is valid (connect_addr above already dereferenced
        // it), and new_conn_id was just returned by this manager's transport
        // and is not yet tracked by the manager.
        if unsafe { hew_connmgr_expect_peer(mgr_ptr, new_conn_id, expected) } != 0 {
            // hew_connmgr_expect_peer does not take ownership on failure, and
            // hew_connmgr_add is never reached on this path, so this call
            // owns closing the transport connection.
            // SAFETY: mgr_ptr is valid; new_conn_id is not yet installed, so
            // no reader thread is racing this close.
            unsafe { close_transport_conn((&*mgr_ptr).transport, new_conn_id) };
            return ReconnectAttemptOutcome::Rejected;
        }
    }

    // SAFETY: manager pointer is valid until shutdown and join in free.
    let admitted =
        unsafe { add_connection(mgr_ptr, new_conn_id, Some(&plan.predecessor), admission) };
    let Some(admitted) = admitted else {
        // hew_connmgr_add owns conn_id cleanup on failure (closes the
        // transport connection on all failure paths, including a
        // pinned-identity mismatch) and has already recorded the specific
        // rejection reason via set_last_error.
        return if reconnect_may_retry(mgr, plan) {
            ReconnectAttemptOutcome::Rejected
        } else {
            ReconnectAttemptOutcome::Cancelled
        };
    };
    let owner = match admitted {
        Admitted::Installed(owner) => owner,
        // The peer's own dial won a crossing: it owns the connection and its
        // redial, and this worker's dial is already closed.
        Admitted::Yielded(_) => return ReconnectAttemptOutcome::Cancelled,
    };
    let _ = configure_owner_reconnect(
        mgr,
        &owner,
        &plan.target_addr,
        i32::try_from(plan.max_retries).unwrap_or(i32::MAX),
        plan.expected_node_id,
    );
    ReconnectAttemptOutcome::Installed
}

/// Arm reconnect on the exact admitted connection `owner` names. Admission may
/// close the connection it was handed (a crossing dial that yielded) and the
/// transport may then reissue that id, so the id never names the admitted
/// connection once admission returns.
fn configure_owner_reconnect(
    mgr: &HewConnMgr,
    owner: &ReconnectOwner,
    target_addr: &str,
    max_retries: c_int,
    expected_node_id: Option<u16>,
) -> c_int {
    apply_reconnect(
        mgr,
        |conn| {
            conn.peer_identity == Some(owner.node_id)
                && conn.publication_token == owner.publication_token
        },
        Some(target_addr.to_owned()),
        max_retries,
        expected_node_id,
    )
}

/// Admit a connection the node API dialled to `target_addr` and arm reconnect
/// on the connection that admission kept: this one, or the peer's crossing
/// dial it yielded to. Returns 0 once the peer is connected, or -1 when the
/// dial was refused; admission owns `conn_id` on every path.
///
/// # Safety
///
/// `mgr` is a live manager and `conn_id` a connection its transport opened.
pub(crate) unsafe fn add_dialled_connection(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    target_addr: &CStr,
    expected_node_id: Option<u16>,
) -> c_int {
    // SAFETY: the caller provides a live manager.
    let Some(manager) = (unsafe { mgr.as_ref() }) else {
        set_last_error("hew_connmgr_add: manager is null");
        return -1;
    };
    let admission = AdmissionStart::now(manager, ConnectionOrigin::Dialled);
    // SAFETY: the caller provides a live manager and an owned connection.
    let Some(admitted) = (unsafe { add_connection(mgr, conn_id, None, admission) }) else {
        return -1;
    };
    #[cfg(test)]
    super::admission_script::reach(
        manager,
        super::admission_script::Point::Admitted(conn_id),
        ConnectionOrigin::Dialled,
    );
    let _ = configure_owner_reconnect(
        manager,
        &admitted.owner(),
        &target_addr.to_string_lossy(),
        0,
        expected_node_id,
    );
    0
}

/// Install a stand-in connection under `conn_id`, as a transport that reissued
/// a closed connection's id would.
#[cfg(test)]
pub(crate) fn install_stand_in_for_test(mgr: &HewConnMgr, conn_id: c_int) {
    let channel = Arc::new(super::channel::ChannelProtection::test_noise_pair().0);
    let mut actor = super::ConnectionActor::new(conn_id, channel);
    actor.transport = super::ConnectionTransport::borrowed(std::ptr::null_mut());
    mgr.connections
        .access(|connections| connections.push(actor));
}

/// Each installed connection's id, publication token and reconnect target.
#[cfg(test)]
pub(crate) fn reconnect_targets_for_test(mgr: &HewConnMgr) -> Vec<(c_int, u64, Option<String>)> {
    mgr.connections.access(|connections| {
        connections
            .iter()
            .map(|connection| {
                (
                    connection.conn_id,
                    connection.publication_token,
                    connection
                        .reconnect
                        .as_ref()
                        .map(|reconnect| reconnect.target_addr.clone()),
                )
            })
            .collect()
    })
}

#[expect(
    clippy::needless_pass_by_value,
    reason = "FFI callback signature requires owned values"
)]
fn reconnect_worker_loop(
    mgr: SendConnMgr,
    shutdown: Arc<AtomicBool>,
    dropped_conn_id: c_int,
    plan: ReconnectPlan,
) {
    let mgr_ptr = mgr.0;
    let mut base_backoff_ms = RECONNECT_INITIAL_BACKOFF_MS;

    for attempt in 1..=plan.max_retries {
        if shutdown.load(Ordering::Acquire) {
            return;
        }
        let delay_ms = jittered_backoff_ms(base_backoff_ms);
        if !sleep_until_retry(&shutdown, delay_ms) {
            return;
        }
        if shutdown.load(Ordering::Acquire) {
            return;
        }

        if matches!(
            reconnect_attempt(mgr_ptr, &plan, dropped_conn_id, attempt),
            ReconnectAttemptOutcome::Installed | ReconnectAttemptOutcome::Cancelled
        ) {
            return;
        }

        base_backoff_ms = base_backoff_ms
            .saturating_mul(2)
            .min(RECONNECT_MAX_BACKOFF_MS);
    }

    set_last_error(format!(
        "hew_connmgr_reconnect: giving up after {} attempts for dropped conn {dropped_conn_id}, addr={}",
        plan.max_retries, plan.target_addr
    ));
}

/// Install one persistent discovery owner per distinct configured target.
/// The node startup lock excludes shutdown until the worker is registered.
pub(crate) unsafe fn start_seed(mgr: *mut HewConnMgr, target: super::SeedTarget) -> bool {
    // SAFETY: caller retains the manager through registration.
    let Some(manager) = (unsafe { mgr.as_ref() }) else {
        return false;
    };
    let inserted = manager.seed_targets.access(|seeds| {
        if let Some(existing) = seeds
            .iter()
            .find(|seed| seed.target_addr == target.target_addr)
        {
            if existing != &target {
                return Err(());
            }
            Ok(false)
        } else {
            seeds.push(target.clone());
            Ok(true)
        }
    });
    match inserted {
        Ok(false) => return true,
        Ok(true) => {}
        Err(()) => {
            set_last_error("Node::start: conflicting pins for one seed address");
            return false;
        }
    }
    let send = SendConnMgr(mgr);
    let worker = thread::Builder::new()
        .name("hew-seed".into())
        .spawn(move || {
            seed_worker_loop(&send, &target);
        });
    match worker {
        Ok(worker) => {
            manager
                .reconnect_workers
                .access(|workers| workers.push(worker));
            true
        }
        Err(error) => {
            set_last_error(format!("Node::start: cannot start seed discovery: {error}"));
            false
        }
    }
}

/// What a seed worker does next for the identity it last admitted.
pub(super) enum SeedNext {
    /// A connection still owns the identity, or an admission is in flight.
    Wait,
    /// The identity's last publication retired; reconnect as its successor.
    Reconnect(ReconnectOwner),
    /// The membership table retired the identity; dial as a fresh admission.
    Fresh,
}

/// A seed follows the latest authenticated publication for its discovered
/// identity. A successor transfers supervision; it never revives a stale
/// predecessor or replays actor requests.
pub(super) fn seed_predecessor(mgr: &HewConnMgr, owner: &ReconnectOwner) -> SeedNext {
    let claims = mgr.claims.0.lock_or_recover();
    match claims.get(&owner.node_id) {
        None => SeedNext::Fresh,
        Some(claim) if claim.state == crate::peer_binding::ClaimState::Retired => {
            SeedNext::Reconnect(ReconnectOwner {
                node_id: owner.node_id,
                session_incarnation: claim.session_incarnation,
                publication_token: claim.publication_token,
            })
        }
        Some(_) => SeedNext::Wait,
    }
}

fn seed_worker_loop(send: &SendConnMgr, target: &super::SeedTarget) {
    let mgr_ptr = send.0;
    // SAFETY: the manager joins all registered workers before freeing itself.
    let mgr = unsafe { &*mgr_ptr };
    let mut owner = None;
    let mut backoff = RECONNECT_INITIAL_BACKOFF_MS;
    while !mgr.reconnect_shutdown.load(Ordering::Acquire) {
        let predecessor = match owner.as_ref().map(|owner| seed_predecessor(mgr, owner)) {
            Some(SeedNext::Wait) => {
                if !sleep_until_retry(&mgr.reconnect_shutdown, RECONNECT_SLEEP_SLICE_MS) {
                    return;
                }
                continue;
            }
            Some(SeedNext::Reconnect(predecessor)) => Some(predecessor),
            Some(SeedNext::Fresh) | None => None,
        };
        let installed = crate::transport::with_connection_attempt_deadline(|| {
            seed_attempt(mgr_ptr, target, predecessor.as_ref())
        });
        if let Some(admitted) = installed {
            mgr.seed_identities.access(|identities| {
                if !identities.contains(&admitted.node_id) {
                    identities.push(admitted.node_id);
                }
            });
            owner = Some(admitted);
            backoff = RECONNECT_INITIAL_BACKOFF_MS;
            if !sleep_until_retry(&mgr.reconnect_shutdown, jittered_backoff_ms(backoff)) {
                return;
            }
            continue;
        }
        if !sleep_until_retry(&mgr.reconnect_shutdown, jittered_backoff_ms(backoff)) {
            return;
        }
        backoff = backoff.saturating_mul(2).min(RECONNECT_MAX_BACKOFF_MS);
    }
}

fn seed_attempt(
    mgr_ptr: *mut HewConnMgr,
    target: &super::SeedTarget,
    predecessor: Option<&ReconnectOwner>,
) -> Option<ReconnectOwner> {
    // SAFETY: the worker's manager remains live until it is joined.
    let mgr = unsafe { &*mgr_ptr };
    if mgr.reconnect_shutdown.load(Ordering::Acquire) {
        return None;
    }
    let address = std::ffi::CString::new(target.target_addr.as_str()).ok()?;
    let admission = AdmissionStart::now(mgr, ConnectionOrigin::Dialled);
    // SAFETY: manager and address remain valid through admission.
    let conn_id = unsafe { connect_addr(mgr_ptr, &address) }.ok()?;
    if let Some(expected) = target.expected_node_id {
        // SAFETY: the dial returned a live, uninstalled connection.
        if unsafe { hew_connmgr_expect_peer(mgr_ptr, conn_id, expected) } != 0 {
            // SAFETY: admission has not taken ownership.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            return None;
        }
    }
    // SAFETY: admission consumes the connection on every path. It checks the
    // exact predecessor after authentication and before publishing any route.
    // A yielded crossing dial supervises the winning publication instead.
    unsafe { add_connection(mgr_ptr, conn_id, predecessor, admission) }.map(Admitted::owner)
}
