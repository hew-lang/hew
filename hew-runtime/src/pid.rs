//! Temporary packed actor IDs for node-internal dispatch.
//!
//! Encodes a 16-bit node ID and 48-bit serial number into a single `u64`
//! actor identifier. Distributed identity is carried by
//! [`crate::node_identity::Location`]; these values are receiver-local aliases
//! retained until the Stage 3b compiler aggregate migration.
//!
//! # Encoding
//!
//! ```text
//! ┌──────────────────────────────────────────────────────────────────┐
//! │  63..48  (16 bits)  │  47..0  (48 bits)                        │
//! │  node_id            │  serial                                  │
//! └──────────────────────────────────────────────────────────────────┘
//! ```
//!
//! - Node 0 is the local node (default). IDs 1, 2, 3… are local actors.
//! - Node IDs 1–65535 identify remote nodes in a cluster.
//! - The 48-bit serial supports ~281 trillion actors per node.
//!
//! # C ABI
//!
//! - [`hew_pid_make`] — Compose a PID from node ID and serial.
//! - [`hew_pid_node`] — Extract the node ID from a PID.
//! - [`hew_pid_serial`] — Extract the serial number from a PID.
//! - [`hew_pid_is_local`] — Check if a PID refers to a local actor.
//! - [`hew_pid_set_local_node`] — Set this node's ID for routing.
//! - [`hew_pid_local_node`] — Get this node's ID.
//!
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::ffi::c_int;

/// Bit width of the serial number portion.
const SERIAL_BITS: u32 = 48;
/// Mask for the serial portion (lower 48 bits).
const SERIAL_MASK: u64 = (1u64 << SERIAL_BITS) - 1;

/// Whether an exact actor slot can be represented by the temporary packed
/// node-internal actor ID without truncation.
pub(crate) const fn actor_slot_fits_internal_alias(slot: u64) -> bool {
    slot != 0 && slot <= SERIAL_MASK
}

/// Compose a PID from a node ID and serial number.
///
/// ```text
/// // Local actor serial 42:
/// let pid = hew_pid_make(0, 42); // == 42
/// // Remote actor on node 3, serial 100:
/// let pid = hew_pid_make(3, 100); // == (3 << 48) | 100
/// ```
#[no_mangle]
pub extern "C" fn hew_pid_make(node_id: u16, serial: u64) -> u64 {
    (u64::from(node_id) << SERIAL_BITS) | (serial & SERIAL_MASK)
}

/// Extract the node ID from a PID.
#[no_mangle]
pub extern "C" fn hew_pid_node(pid: u64) -> u16 {
    (pid >> SERIAL_BITS) as u16
}

/// Extract the serial number from a PID.
#[no_mangle]
pub extern "C" fn hew_pid_serial(pid: u64) -> u64 {
    pid & SERIAL_MASK
}

/// Check if a PID refers to an actor on the local node.
///
/// Returns 1 if local, 0 if remote.
///
/// When no runtime is installed the local node ID is treated as 0 (the
/// standalone/default node), matching the behaviour of the old process-global
/// `LOCAL_NODE_ID` which defaulted to 0. This preserves the always-available
/// semantics for callers that check locality before `hew_sched_init` or after
/// teardown. The mutate path (`hew_pid_set_local_node`) stays fail-closed.
#[no_mangle]
pub extern "C" fn hew_pid_is_local(pid: u64) -> c_int {
    let pid_node = hew_pid_node(pid);
    let local = crate::runtime::rt_current_opt().map_or(0, |rt| rt.node.local_node_id());
    c_int::from(pid_node == local || pid_node == 0)
}

/// Set this process's node ID.
///
/// Should be called once at startup before any actors are spawned.
/// Node ID 0 means "local/standalone" (the default).
#[no_mangle]
pub extern "C" fn hew_pid_set_local_node(node_id: u16) {
    crate::runtime::rt_current().node.set_local_node_id(node_id);
}

/// Get this process's node ID.
///
/// When no runtime is installed the local node ID is 0 (the standalone/default
/// node), matching the behaviour of the old process-global `LOCAL_NODE_ID` which
/// defaulted to 0. This preserves the always-available semantics for callers
/// that query the node ID before `hew_sched_init` or after teardown.
/// The mutate path (`hew_pid_set_local_node`) stays fail-closed.
#[no_mangle]
pub extern "C" fn hew_pid_local_node() -> u16 {
    crate::runtime::rt_current_opt().map_or(0, |rt| rt.node.local_node_id())
}

/// Allocate the next actor ID for the local node.
///
/// Combines the local node ID with a monotonically-increasing serial.
pub(crate) fn next_actor_id(serial: u64) -> u64 {
    let node = crate::runtime::rt_current().node.local_node_id();
    hew_pid_make(node, serial)
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    struct PidTestGuard {
        _enter: crate::runtime::EnterGuard,
        _rt: Box<crate::runtime::RuntimeInner>,
    }

    fn guard() -> PidTestGuard {
        let rt = Box::new(crate::runtime::RuntimeInner::new(
            crate::scheduler::worker_less_scheduler(),
        ));
        // SAFETY: the guard owns `rt` and drops `_enter` before it, so the
        // entered runtime outlives the thread-local selection that names it.
        let enter = unsafe { crate::runtime::enter(&rt) };
        PidTestGuard {
            _enter: enter,
            _rt: rt,
        }
    }

    #[test]
    fn local_pid_is_plain_serial() {
        // Node 0 + serial 42 == 42.
        let pid = hew_pid_make(0, 42);
        assert_eq!(pid, 42);
        assert_eq!(hew_pid_node(pid), 0);
        assert_eq!(hew_pid_serial(pid), 42);
    }

    #[test]
    fn remote_pid_encoding() {
        let pid = hew_pid_make(3, 100);
        assert_eq!(hew_pid_node(pid), 3);
        assert_eq!(hew_pid_serial(pid), 100);
        assert_ne!(pid, 100); // Not equal to plain serial.
    }

    #[test]
    fn round_trip() {
        for node in [0, 1, 255, 65535] {
            for serial in [0, 1, 1000, SERIAL_MASK] {
                let pid = hew_pid_make(node, serial);
                assert_eq!(hew_pid_node(pid), node);
                assert_eq!(hew_pid_serial(pid), serial);
            }
        }
    }

    #[test]
    fn serial_mask_truncation() {
        // If serial exceeds 48 bits, the extra bits are masked off.
        let too_large = 1u64 << 48;
        let pid = hew_pid_make(0, too_large);
        assert_eq!(hew_pid_serial(pid), 0);
    }

    #[test]
    fn is_local_checks() {
        let _g = guard();
        // Default node is 0.
        hew_pid_set_local_node(0);
        assert_eq!(hew_pid_is_local(hew_pid_make(0, 1)), 1);
        assert_eq!(hew_pid_is_local(hew_pid_make(5, 1)), 0);

        // Change to node 5.
        hew_pid_set_local_node(5);
        assert_eq!(hew_pid_is_local(hew_pid_make(5, 1)), 1);
        assert_eq!(hew_pid_is_local(hew_pid_make(0, 1)), 1); // Node 0 always local.
        assert_eq!(hew_pid_is_local(hew_pid_make(3, 1)), 0);

        // Reset.
        hew_pid_set_local_node(0);
    }

    #[test]
    fn next_actor_id_integration() {
        let _g = guard();
        hew_pid_set_local_node(0);
        assert_eq!(next_actor_id(42), 42);

        hew_pid_set_local_node(7);
        let id = next_actor_id(42);
        assert_eq!(hew_pid_node(id), 7);
        assert_eq!(hew_pid_serial(id), 42);

        hew_pid_set_local_node(0);
    }

    #[test]
    fn independent_runtimes_keep_separate_local_node_ids() {
        let workers: Vec<_> = [11_u16, 29_u16]
            .into_iter()
            .map(|node| {
                std::thread::spawn(move || {
                    let _g = guard();
                    hew_pid_set_local_node(node);
                    let actor_id = next_actor_id(42);
                    (
                        hew_pid_local_node(),
                        hew_pid_node(actor_id),
                        hew_pid_is_local(hew_pid_make(node, 9)),
                        hew_pid_is_local(hew_pid_make(node + 1, 9)),
                    )
                })
            })
            .collect();

        let mut seen: Vec<_> = workers.into_iter().map(|w| w.join().unwrap()).collect();
        seen.sort_unstable_by_key(|entry| entry.0);
        assert_eq!(seen, vec![(11, 11, 1, 0), (29, 29, 1, 0)]);
    }

    /// `hew_pid_local_node` and `hew_pid_is_local` must not abort when called
    /// with no runtime installed. The old process-global defaulted to node 0, so
    /// both functions return the node-0 / standalone defaults here. Before the
    /// fix both routed through the fail-closed `rt_current()` and panicked.
    #[test]
    fn pid_reads_tolerate_no_runtime() {
        // No runtime guard on this thread.
        assert_eq!(
            hew_pid_local_node(),
            0,
            "local_node must default to 0 with no runtime installed"
        );
        // A node-0 PID is local (node 0 is always the local standalone node).
        assert_eq!(hew_pid_is_local(hew_pid_make(0, 42)), 1);
        // A non-zero node PID is remote when the local node is 0.
        assert_eq!(hew_pid_is_local(hew_pid_make(7, 42)), 0);
    }

    /// With a runtime installed the PID read functions resolve the per-runtime
    /// `NodeSlot`, confirming that the deglobalization is intact and the
    /// no-runtime tolerance did not flatten both reads to always-zero.
    #[test]
    fn pid_reads_with_runtime_use_per_runtime_node_id() {
        let _g = guard();
        hew_pid_set_local_node(13);

        assert_eq!(hew_pid_local_node(), 13);
        assert_eq!(hew_pid_is_local(hew_pid_make(13, 1)), 1);
        assert_eq!(hew_pid_is_local(hew_pid_make(0, 1)), 1); // node 0 always local
        assert_eq!(hew_pid_is_local(hew_pid_make(5, 1)), 0);

        // Reset to default for test isolation.
        hew_pid_set_local_node(0);
    }
}
