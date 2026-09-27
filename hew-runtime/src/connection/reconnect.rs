//! Reconnect policy, backoff and the reconnect worker.

use std::ffi::{c_int, CStr};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use rand::rng;
use rand::RngExt;

use crate::set_last_error;
use crate::transport::HEW_CONN_INVALID;

use super::admission::hew_connmgr_add;
use super::handshake::close_transport_conn;
use super::manager::{hew_connmgr_configure_reconnect, hew_connmgr_expect_peer};
use super::{
    HewConnMgr, ReconnectPlan, SendConnMgr, RECONNECT_DEFAULT_MAX_RETRIES,
    RECONNECT_INITIAL_BACKOFF_MS, RECONNECT_JITTER_MAX_PERCENT, RECONNECT_JITTER_MIN_PERCENT,
    RECONNECT_MAX_BACKOFF_MS, RECONNECT_SLEEP_SLICE_MS,
};

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
    jittered.max(1)
}

fn sleep_until_retry(shutdown: &AtomicBool, delay_ms: u64) -> bool {
    let mut remaining = delay_ms;
    while remaining > 0 {
        if shutdown.load(Ordering::Acquire) {
            return false;
        }
        let slice = remaining.min(RECONNECT_SLEEP_SLICE_MS);
        thread::sleep(Duration::from_millis(slice));
        remaining -= slice;
    }
    !shutdown.load(Ordering::Acquire)
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
        let reconnect = conn.reconnect.as_ref()?;
        Some(ReconnectPlan {
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
}

/// Runs one reconnect attempt: connect, replay the pinned peer identity
/// (`plan.expected_node_id`) if any, install via `hew_connmgr_add`, and
/// re-arm reconnect on success.
///
/// Extracted from the retry loop so both the worker and tests can drive the
/// exact install sequence deterministically and same-thread. Bare-address
/// reconnects (`plan.expected_node_id.is_none()`) skip straight to
/// `hew_connmgr_add`, unchanged from before this pin was threaded through.
pub(super) fn reconnect_attempt(
    mgr_ptr: *mut HewConnMgr,
    plan: &ReconnectPlan,
    dropped_conn_id: c_int,
    attempt: u32,
) -> ReconnectAttemptOutcome {
    let Ok(target_addr) = std::ffi::CString::new(plan.target_addr.as_str()) else {
        set_last_error(format!(
            "hew_connmgr_reconnect: invalid reconnect address for dropped conn {dropped_conn_id}"
        ));
        return ReconnectAttemptOutcome::TransportError;
    };
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
    if unsafe { hew_connmgr_add(mgr_ptr, new_conn_id) } != 0 {
        // hew_connmgr_add owns conn_id cleanup on failure (closes the
        // transport connection on all failure paths, including a
        // pinned-identity mismatch) and has already recorded the specific
        // rejection reason via set_last_error.
        return ReconnectAttemptOutcome::Rejected;
    }

    let retries = i32::try_from(plan.max_retries).unwrap_or(i32::MAX);
    // SAFETY: manager and conn_id are valid after successful add.
    let _ = unsafe {
        hew_connmgr_configure_reconnect(
            mgr_ptr,
            new_conn_id,
            target_addr.as_ptr(),
            1,
            retries,
            plan.expected_node_id.map_or(0, i32::from),
        )
    };
    ReconnectAttemptOutcome::Installed
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

        if reconnect_attempt(mgr_ptr, &plan, dropped_conn_id, attempt)
            == ReconnectAttemptOutcome::Installed
        {
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
