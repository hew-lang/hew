//! Location-transparent actor PIDs.
//!
//! Encodes a 16-bit node ID and 48-bit serial number into a single `u64`
//! actor identifier. Local actors (node 0) have IDs identical to plain
//! serial numbers, preserving backward compatibility.
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

use std::ffi::c_int;
use std::sync::atomic::{AtomicU16, Ordering};

/// Bit width of the serial number portion.
const SERIAL_BITS: u32 = 48;
/// Mask for the serial portion (lower 48 bits).
const SERIAL_MASK: u64 = (1u64 << SERIAL_BITS) - 1;

/// The node ID assigned to this process. Default is 0 (local/standalone).
static LOCAL_NODE_ID: AtomicU16 = AtomicU16::new(0);

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
#[no_mangle]
pub extern "C" fn hew_pid_is_local(pid: u64) -> c_int {
    let pid_node = hew_pid_node(pid);
    let local = LOCAL_NODE_ID.load(Ordering::Relaxed);
    c_int::from(pid_node == local || pid_node == 0)
}

/// Set this process's node ID.
///
/// Should be called once at startup before any actors are spawned.
/// Node ID 0 means "local/standalone" (the default).
#[no_mangle]
pub extern "C" fn hew_pid_set_local_node(node_id: u16) {
    LOCAL_NODE_ID.store(node_id, Ordering::Release);
}

/// Get this process's node ID.
#[no_mangle]
pub extern "C" fn hew_pid_local_node() -> u16 {
    LOCAL_NODE_ID.load(Ordering::Acquire)
}

/// Allocate the next actor ID for the local node.
///
/// Combines the local node ID with a monotonically-increasing serial.
pub(crate) fn next_actor_id(serial: u64) -> u64 {
    let node = LOCAL_NODE_ID.load(Ordering::Relaxed);
    hew_pid_make(node, serial)
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

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
        hew_pid_set_local_node(0);
        assert_eq!(next_actor_id(42), 42);

        hew_pid_set_local_node(7);
        let id = next_actor_id(42);
        assert_eq!(hew_pid_node(id), 7);
        assert_eq!(hew_pid_serial(id), 42);

        hew_pid_set_local_node(0);
    }
}
