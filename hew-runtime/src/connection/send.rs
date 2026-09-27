//! Outbound sends, broadcast and connection queries.

use std::ffi::c_int;
use std::sync::atomic::Ordering;
use std::sync::Arc;
#[cfg(feature = "encryption")]
use std::sync::Mutex;

use crate::mailbox_envelope::MailboxPayloadClass;
use crate::node_identity::{HewLocation, Location};
use crate::peer_binding::ClaimState;
use crate::set_last_error;
use crate::transport::HEW_CONN_INVALID;
use crate::util::MutexExt;

use super::control::encode_envelope;
use super::handshake::send_frame;
use super::{ClaimedSendLease, HewConnMgr, CONN_STATE_ACTIVE, CONN_STATE_CLOSED};

/// Legacy API: outbound queue tuning is no longer supported.
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_set_outbound_capacity(
    mgr: *mut HewConnMgr,
    _conn_id: c_int,
    _capacity: usize,
) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_set_outbound_capacity: manager is null");
        return -1;
    }
    set_last_error(
        "hew_connmgr_set_outbound_capacity: outbound queue support was removed; sends are synchronous",
    );
    -1
}

/// Send a wire envelope over a specific connection.
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// - `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
/// - `target` must point to a valid exact actor location.
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_send(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    target: *const HewLocation,
    msg_type: i32,
    data: *mut u8,
    size: usize,
) -> c_int {
    cabi_guard!(mgr.is_null() || target.is_null(), -1);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        set_last_error("hew_connmgr_send: target location is invalid");
        return -1;
    };

    // Verify the connection is the active authenticated owner of this exact
    // node/session pair. This prevents conn-id and route-slot reuse from
    // redirecting a carried location.
    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        #[cfg(feature = "encryption")]
        let mut noise_out = None::<Arc<Mutex<Option<snow::TransportState>>>>;
        let publication_token = mgr_ref.connections.access(|conns| {
            let active = conns.iter().find(|c| {
                c.conn_id == conn_id
                    && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && c.peer_identity == Some(target.node())
                    && c.peer_session_incarnation == target.incarnation()
            });
            #[cfg(feature = "encryption")]
            if let Some(c) = active {
                noise_out = Some(Arc::clone(&c.noise_transport));
            }
            active.map(|connection| connection.publication_token)
        });
        let Some(publication_token) = publication_token else {
            return -1;
        };
        let owns_claim = mgr_ref
            .claims
            .0
            .lock_or_recover()
            .get(&target.node())
            .is_some_and(|claim| {
                claim.state == ClaimState::Published
                    && claim.conn_id == conn_id
                    && claim.publication_token == publication_token
                    && claim.session_incarnation == target.incarnation()
            });
        if !owns_claim {
            return -1;
        }
        #[cfg(feature = "encryption")]
        {
            maybe_noise = noise_out;
        }
    }

    // SAFETY: data is valid for size bytes per caller contract of hew_connmgr_send.
    let Some(encoded) = (unsafe {
        encode_envelope(
            target,
            msg_type,
            data,
            size,
            MailboxPayloadClass::SerializedCrossNode as u8,
            0,
        )
    }) else {
        return -1;
    };

    #[cfg(feature = "encryption")]
    if let Some(noise_transport) = maybe_noise {
        let mut maybe_ciphertext = None;
        {
            let Ok(mut guard) = noise_transport.lock() else {
                // Policy: per-connection state (C-ABI) — poisoned noise transport
                // means this connection's encryption state is corrupted.
                set_last_error(
                    "hew_connmgr_send: noise_transport mutex poisoned (a thread panicked)",
                );
                return -1;
            };
            if let Some(noise) = guard.as_mut() {
                let mut ciphertext = vec![0u8; encoded.len() + 16];
                let Ok(n) = noise.write_message(&encoded, &mut ciphertext) else {
                    return -1;
                };
                ciphertext.truncate(n);
                maybe_ciphertext = Some(ciphertext);
            }
        }
        if let Some(ciphertext) = maybe_ciphertext {
            // SAFETY: mgr_ref.transport is valid per caller contract; conn_id verified active above.
            if unsafe { send_frame(mgr_ref.transport, conn_id, &ciphertext) } {
                return 0;
            }
            return -1;
        }
    }

    // SAFETY: mgr_ref.transport is valid per caller contract and conn_id is active.
    if unsafe { send_frame(mgr_ref.transport, conn_id, &encoded) } {
        0
    } else {
        -1
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "keeps claimed-slot validation atomic with encrypted/plaintext transport sends"
)]
pub(super) unsafe fn send_preencoded_on_manager(
    mgr_ref: &HewConnMgr,
    conn_id: c_int,
    expected_publication_token: Option<u64>,
    data: *const u8,
    len: usize,
) -> c_int {
    if let Some(publication_token) = expected_publication_token {
        let lease = mgr_ref.connections.access(|connections| {
            let active = connections.iter().find(|connection| {
                connection.conn_id == conn_id
                    && connection.publication_token == publication_token
                    && connection.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
            })?;
            Some(ClaimedSendLease {
                _guard: active.claimed_send_lifecycle.register(),
                publication_removed: Arc::clone(&active.publication_removed),
                #[cfg(feature = "encryption")]
                noise_transport: Arc::clone(&active.noise_transport),
            })
        });
        let Some(lease) = lease else {
            return -1;
        };
        if lease.publication_removed.load(Ordering::Acquire) {
            return -1;
        }

        #[cfg(feature = "encryption")]
        {
            // SAFETY: data is valid for `len` bytes per caller contract.
            let slice = unsafe { std::slice::from_raw_parts(data, len) };
            let mut ciphertext = vec![0u8; len + 16];
            let mut guard = lease.noise_transport.lock_or_recover();
            if let Some(noise) = guard.as_mut() {
                let Ok(n) = noise.write_message(slice, &mut ciphertext) else {
                    return -1;
                };
                ciphertext.truncate(n);
                // SAFETY: teardown closes the connection before waiting for this
                // lease, so a blocked send is interrupted without slot reuse.
                return if unsafe { send_frame(mgr_ref.transport, conn_id, &ciphertext) } {
                    0
                } else {
                    -1
                };
            }
        }

        // SAFETY: transport remains valid while the manager-owned worker is
        // joined; the claimed lease prevents slot removal/reuse until return.
        let transport = unsafe { &*mgr_ref.transport };
        // SAFETY: `transport` remains valid for the manager lifetime.
        let rc = if let Some(ops) = unsafe { transport.ops.as_ref() } {
            if let Some(send_fn) = ops.send {
                // SAFETY: data is valid for `len` bytes per caller contract.
                unsafe { send_fn(transport.r#impl, conn_id, data.cast_mut().cast(), len) }
            } else {
                -1
            }
        } else {
            -1
        };
        return if rc > 0 { 0 } else { -1 };
    }

    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        #[cfg(feature = "encryption")]
        let mut noise_out = None::<Arc<Mutex<Option<snow::TransportState>>>>;
        let ok = mgr_ref.connections.access(|conns| {
            let active = conns.iter().find(|c| {
                c.conn_id == conn_id
                    && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && expected_publication_token.is_none_or(|token| c.publication_token == token)
            });
            #[cfg(feature = "encryption")]
            if let Some(c) = active {
                noise_out = Some(Arc::clone(&c.noise_transport));
            }
            active.is_some()
        });
        if !ok {
            return -1;
        }
        #[cfg(feature = "encryption")]
        {
            maybe_noise = noise_out;
        }
    }

    #[cfg(feature = "encryption")]
    if let Some(noise_arc) = maybe_noise {
        // SAFETY: data is valid for `len` bytes per caller contract.
        let slice = unsafe { std::slice::from_raw_parts(data, len) };
        let mut ciphertext = vec![0u8; len + 16];
        let mut guard = noise_arc.lock_or_recover();
        if let Some(noise) = guard.as_mut() {
            let Ok(n) = noise.write_message(slice, &mut ciphertext) else {
                return -1;
            };
            ciphertext.truncate(n);
            // SAFETY: transport is valid per manager contract; conn_id verified active above.
            return if unsafe { send_frame(mgr_ref.transport, conn_id, &ciphertext) } {
                0
            } else {
                -1
            };
        }
        // No noise state — fall through to plaintext send.
    }

    // Plaintext send path.
    // SAFETY: transport is valid per manager contract.
    let t = unsafe { &*mgr_ref.transport };
    // SAFETY: ops pointer is part of valid transport.
    let rc = if let Some(ops) = unsafe { t.ops.as_ref() } {
        if let Some(send_fn) = ops.send {
            // SAFETY: data is valid for `len` bytes per caller contract; conn_id verified active.
            unsafe { send_fn(t.r#impl, conn_id, data.cast_mut().cast(), len) }
        } else {
            -1
        }
    } else {
        -1
    };
    if rc > 0 {
        0
    } else {
        -1
    }
}

/// Send a pre-encoded wire frame over a specific connection, applying noise
/// encryption when the connection is encrypted.
///
/// Unlike [`hew_connmgr_send`], this function takes bytes that are already
/// encoded into the Hew wire format (e.g., a full ask/reply envelope). The
/// only transformation applied is noise encryption (when enabled).
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// - `mgr` must be a valid pointer for the duration of the call.
/// - `data` must point to at least `len` readable bytes.
pub(crate) unsafe fn hew_connmgr_send_preencoded(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    data: *const u8,
    len: usize,
) -> c_int {
    if mgr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid for the duration of the call.
    let mgr_ref = unsafe { &*mgr };
    // SAFETY: forwarded caller contract.
    unsafe { send_preencoded_on_manager(mgr_ref, conn_id, None, data, len) }
}

/// Send a pre-encoded frame only if `conn_id` still names the exact published
/// connection incarnation identified by `publication_token`.
///
/// # Safety
///
/// `mgr` must be valid and `data` readable for `len` bytes.
pub(crate) unsafe fn hew_connmgr_send_preencoded_claimed(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    data: *const u8,
    len: usize,
) -> c_int {
    if mgr.is_null() || conn_id == HEW_CONN_INVALID || (data.is_null() && len > 0) {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` and `data` validity.
    unsafe { send_preencoded_on_manager(&*mgr, conn_id, Some(publication_token), data, len) }
}

/// Return the connection ID of the first active connection whose handshake
/// identified the peer as `node_id`. Returns -1 if not found.
///
/// Used by `send_reply_envelope` on the accepting side where `conn_by_node`
/// is not populated (only the initiating side calls `hew_node_connect`).
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
pub(crate) unsafe fn hew_connmgr_conn_id_for_node(mgr: *const HewConnMgr, node_id: u16) -> c_int {
    if mgr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.connections.access(|conns| {
        for c in conns.iter() {
            if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
                return c.conn_id;
            }
        }
        -1
    })
}

/// Return the negotiated feature flags for the active connection to `node_id`,
/// or `0` if no active connection exists.
///
/// Used by `node_inbound_router` to determine whether a peer understands the
/// ask-rejection sentinel before sending `HEW_REPLY_REJECT_MSG_TYPE`.
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
pub(crate) unsafe fn hew_connmgr_feature_flags_for_node(
    mgr: *const HewConnMgr,
    node_id: u16,
) -> u32 {
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.connections.access(|conns| {
        for c in conns.iter() {
            if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
                return c.peer_feature_flags;
            }
        }
        0
    })
}

/// Overwrite the recorded feature flags for the active connection to `node_id`.
///
/// Test-only helper used to simulate an older peer after a normal handshake.
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
#[cfg(test)]
pub(crate) unsafe fn hew_connmgr_force_peer_flags_for_node(
    mgr: *mut HewConnMgr,
    node_id: u16,
    flags: u32,
) {
    if mgr.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.connections.access(|conns| {
        for c in conns.iter_mut() {
            if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
                c.peer_feature_flags = flags;
            }
        }
    });
}

/// Return the number of active connections.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_count(mgr: *mut HewConnMgr) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_count: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let count = mgr.connections.access(|conns| conns.len());
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "connection count will not exceed c_int range in practice"
    )]
    {
        count as c_int
    }
}

/// Send a message to all active connections.
///
/// Returns the number of successful sends.
///
/// # Safety
///
/// - `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
/// - `data` must point to at least `size` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_broadcast(
    mgr: *mut HewConnMgr,
    target: *const HewLocation,
    msg_type: i32,
    data: *mut u8,
    size: usize,
) -> c_int {
    cabi_guard!(mgr.is_null() || target.is_null(), 0);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };

    // Collect active connection IDs under the lock.
    let conn_ids: Vec<c_int> = mgr_ref.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE)
            .map(|c| c.conn_id)
            .collect()
    });

    let mut success_count: c_int = 0;
    for cid in conn_ids {
        // SAFETY: mgr is valid, data/size from caller.
        let rc = unsafe { hew_connmgr_send(mgr, cid, target, msg_type, data, size) };
        if rc == 0 {
            success_count += 1;
        }
    }

    success_count
}

/// Get the last activity timestamp (ms) for a connection.
///
/// Returns 0 if the connection is not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_last_activity(mgr: *mut HewConnMgr, conn_id: c_int) -> u64 {
    cabi_guard!(mgr.is_null(), 0);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id)
            .map_or(0, |c| c.last_activity_ms.load(Ordering::Acquire))
    })
}

/// Get the state of a connection.
///
/// Returns [`CONN_STATE_CLOSED`] if the connection is not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_conn_state(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    cabi_guard!(mgr.is_null(), CONN_STATE_CLOSED);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id)
            .map_or(CONN_STATE_CLOSED, |c| c.state.load(Ordering::Acquire))
    })
}
