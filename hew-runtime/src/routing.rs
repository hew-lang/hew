//! PID-based routing table for distributed message delivery.
//!
//! Maps remote node IDs (high 16 bits of a PID) to active transport
//! connection handles.

use crate::util::RwLockExt;
use std::collections::HashMap;
use std::ffi::c_int;
use std::sync::RwLock;

/// Number of low bits reserved for PID serial numbers.
const PID_SERIAL_BITS: u32 = 48;
/// Return value for local delivery or missing route.
const HEW_ROUTE_LOCAL_OR_MISSING: c_int = -1;

/// Maps remote `node_id` → transport connection handle.
#[derive(Debug)]
pub struct HewRoutingTable {
    routes: RwLock<HashMap<u16, c_int>>,
    local_node_id: u16,
}

impl HewRoutingTable {
    #[inline]
    fn node_id_from_pid(target_pid: u64) -> u16 {
        (target_pid >> PID_SERIAL_BITS) as u16
    }
}

/// Create a new routing table for this node.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_routing_table_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_routing_table_new(local_node_id: u16) -> *mut HewRoutingTable {
    Box::into_raw(Box::new(HewRoutingTable {
        routes: RwLock::new(HashMap::new()),
        local_node_id,
    }))
}

/// Free a routing table.
///
/// # Safety
///
/// `table` must be a valid pointer returned by [`hew_routing_table_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_routing_table_free(table: *mut HewRoutingTable) {
    if table.is_null() {
        return;
    }
    // SAFETY: caller guarantees `table` was allocated by `hew_routing_table_new`.
    let _ = unsafe { Box::from_raw(table) };
}

/// Add or replace a route from `node_id` to `conn`.
///
/// # Safety
///
/// `table` must be a valid pointer returned by [`hew_routing_table_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_routing_add_route(
    table: *mut HewRoutingTable,
    node_id: u16,
    conn: c_int,
) {
    if table.is_null() {
        return;
    }
    // SAFETY: caller guarantees `table` is valid.
    let table = unsafe { &*table };
    let mut routes = table.routes.write_or_recover();
    routes.insert(node_id, conn);
}

/// Remove the route for `node_id` if present.
///
/// # Safety
///
/// `table` must be a valid pointer returned by [`hew_routing_table_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_routing_remove_route(table: *mut HewRoutingTable, node_id: u16) {
    if table.is_null() {
        return;
    }
    // SAFETY: caller guarantees `table` is valid.
    let table = unsafe { &*table };
    let mut routes = table.routes.write_or_recover();
    routes.remove(&node_id);
}

/// Resolve a destination PID to a transport connection handle.
///
/// Returns:
/// - `-1` when `target_pid` is local to this node.
/// - `conn` for a known remote route.
/// - `-1` when no route exists.
///
/// # Safety
///
/// `table` must be a valid pointer returned by [`hew_routing_table_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_routing_lookup(table: *mut HewRoutingTable, target_pid: u64) -> c_int {
    if table.is_null() {
        return HEW_ROUTE_LOCAL_OR_MISSING;
    }
    // SAFETY: caller guarantees `table` is valid.
    let table = unsafe { &*table };
    let node_id = HewRoutingTable::node_id_from_pid(target_pid);

    if node_id == table.local_node_id {
        return HEW_ROUTE_LOCAL_OR_MISSING;
    }

    let routes = table.routes.read_or_recover();
    routes
        .get(&node_id)
        .copied()
        .unwrap_or(HEW_ROUTE_LOCAL_OR_MISSING)
}

/// Check whether a destination PID is local to this node.
///
/// Returns 1 if local, 0 otherwise.
///
/// # Safety
///
/// `table` must be a valid pointer returned by [`hew_routing_table_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_routing_is_local(
    table: *mut HewRoutingTable,
    target_pid: u64,
) -> c_int {
    if table.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `table` is valid.
    let table = unsafe { &*table };
    c_int::from(HewRoutingTable::node_id_from_pid(target_pid) == table.local_node_id)
}

// ── Profiler snapshot ───────────────────────────────────────────────────

/// Build a JSON object with the routing table for the profiler HTTP API.
///
/// Format: `{"local_node_id":N,"routes":[{"node_id":N,"conn_id":N},...]}`
#[cfg(feature = "profiler")]
pub fn snapshot_routing_json(table: &HewRoutingTable) -> String {
    use std::fmt::Write as _;

    let routes = table.routes.read_or_recover();
    let routes_json = crate::util::json_array(routes.iter(), |json, (&node_id, &conn_id)| {
        let _ = write!(json, r#"{{"node_id":{node_id},"conn_id":{conn_id}}}"#);
    });

    let mut json = String::new();
    let _ = write!(
        json,
        r#"{{"local_node_id":{},"routes":{routes_json}}}"#,
        table.local_node_id,
    );
    json
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;
    use std::sync::Arc;
    use std::thread;

    struct SharedTable(*mut HewRoutingTable);
    // SAFETY: routing table internals are synchronized by an RwLock.
    unsafe impl Send for SharedTable {}
    // SAFETY: routing table internals are synchronized by an RwLock.
    unsafe impl Sync for SharedTable {}

    #[cfg(feature = "profiler")]
    #[test]
    fn snapshot_routing_json_emits_local_node_and_routes() {
        // SAFETY: pointer lifecycle is bounded to this test.
        unsafe {
            let table = hew_routing_table_new(7);
            hew_routing_add_route(table, 9, 55);

            assert_eq!(
                snapshot_routing_json(&*table),
                r#"{"local_node_id":7,"routes":[{"node_id":9,"conn_id":55}]}"#
            );

            hew_routing_table_free(table);
        }
    }

    #[test]
    fn local_lookup_returns_minus_one() {
        // SAFETY: pointers are created and used within test scope.
        unsafe {
            let table = hew_routing_table_new(7);
            let local_pid = (u64::from(7u16) << PID_SERIAL_BITS) | 0x2A;
            assert_eq!(hew_routing_lookup(table, local_pid), -1);
            assert_eq!(hew_routing_is_local(table, local_pid), 1);
            hew_routing_table_free(table);
        }
    }

    #[test]
    fn remote_route_lookup_and_remove() {
        // SAFETY: pointers are created and used within test scope.
        unsafe {
            let table = hew_routing_table_new(1);
            let remote_pid_a = (u64::from(9u16) << PID_SERIAL_BITS) | 0x64;
            let remote_pid_b = (u64::from(10u16) << PID_SERIAL_BITS) | 0xC8;
            let local_pid = (u64::from(1u16) << PID_SERIAL_BITS) | 7;
            assert_eq!(hew_routing_lookup(table, remote_pid_a), -1);
            assert_eq!(hew_routing_lookup(table, remote_pid_b), -1);
            assert_eq!(hew_routing_lookup(table, local_pid), -1);
            hew_routing_add_route(table, 9, 55);
            hew_routing_add_route(table, 10, 77);
            assert_eq!(hew_routing_lookup(table, remote_pid_a), 55);
            assert_eq!(hew_routing_lookup(table, remote_pid_b), 77);
            assert_eq!(hew_routing_is_local(table, remote_pid_a), 0);
            hew_routing_remove_route(table, 9);
            assert_eq!(hew_routing_lookup(table, remote_pid_a), -1);
            hew_routing_table_free(table);
        }
    }

    #[test]
    fn routing_table_concurrent_access() {
        // SAFETY: pointer lifecycle is bounded to this test.
        unsafe {
            let table = hew_routing_table_new(1);
            assert!(!table.is_null());
            let shared = Arc::new(SharedTable(table));
            let mut threads = Vec::new();

            for tid in 0..8usize {
                let shared = Arc::clone(&shared);
                threads.push(thread::spawn(move || {
                    for i in 0..250usize {
                        #[expect(clippy::cast_possible_truncation, reason = "mod 6 fits in u16")]
                        let node_id = 2 + ((tid + i) % 6) as u16;
                        #[expect(
                            clippy::cast_possible_truncation,
                            clippy::cast_possible_wrap,
                            reason = "test data: small indices fit in c_int"
                        )]
                        let conn = (tid * 1000 + i) as c_int;
                        hew_routing_add_route(shared.0, node_id, conn);
                        let pid = (u64::from(node_id) << PID_SERIAL_BITS) | (i as u64);
                        let _ = hew_routing_lookup(shared.0, pid);
                        if i % 3 == 0 {
                            hew_routing_remove_route(shared.0, node_id);
                        }
                    }
                }));
            }

            for t in threads {
                t.join().expect("thread should complete without panic");
            }

            hew_routing_add_route(table, 42, 4242);
            let pid = (u64::from(42u16) << PID_SERIAL_BITS) | 0x0007;
            assert_eq!(hew_routing_lookup(table, pid), 4242);
            hew_routing_table_free(table);
        }
    }

    // ── node_id extraction ────────────────────────────────────────────

    #[test]
    fn node_id_from_pid_extracts_high_bits() {
        // Hard-coded PID: node_id=42 at bits [63:48], serial=0xDEAD in low 48.
        let pid: u64 = 0x002A_0000_0000_DEAD;
        assert_eq!(HewRoutingTable::node_id_from_pid(pid), 42);
    }

    #[test]
    fn node_id_from_pid_zero() {
        assert_eq!(HewRoutingTable::node_id_from_pid(0), 0);
    }

    #[test]
    fn node_id_from_pid_max_serial_ignored() {
        // node_id=5, serial=all-ones in low 48 bits.
        let pid: u64 = 0x0005_FFFF_FFFF_FFFF;
        assert_eq!(HewRoutingTable::node_id_from_pid(pid), 5);
    }

    #[test]
    fn node_id_from_pid_max_node_id() {
        // node_id=0xFFFF at bits [63:48], serial=0.
        let pid: u64 = 0xFFFF_0000_0000_0000;
        assert_eq!(HewRoutingTable::node_id_from_pid(pid), u16::MAX);
    }

    // ── null pointer safety ───────────────────────────────────────────

    #[test]
    fn null_table_safety() {
        // SAFETY: testing null-pointer guards.
        unsafe {
            hew_routing_table_free(ptr::null_mut());
            hew_routing_add_route(ptr::null_mut(), 1, 42);
            hew_routing_remove_route(ptr::null_mut(), 1);
            assert_eq!(hew_routing_lookup(ptr::null_mut(), 0), -1);
            assert_eq!(hew_routing_is_local(ptr::null_mut(), 0), 0);
        }
    }

    // ── empty table ───────────────────────────────────────────────────

    #[test]
    fn empty_table_lookup_returns_minus_one() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(1);
            let remote_pid = (u64::from(99u16) << PID_SERIAL_BITS) | 1;
            assert_eq!(hew_routing_lookup(table, remote_pid), -1);
            hew_routing_table_free(table);
        }
    }

    // ── overwrite ─────────────────────────────────────────────────────

    #[test]
    fn overwrite_existing_route() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(1);
            let pid = (u64::from(5u16) << PID_SERIAL_BITS) | 0x7;
            hew_routing_add_route(table, 5, 100);
            assert_eq!(hew_routing_lookup(table, pid), 100);
            hew_routing_add_route(table, 5, 200);
            assert_eq!(hew_routing_lookup(table, pid), 200);
            hew_routing_table_free(table);
        }
    }

    // ── remove edge cases ─────────────────────────────────────────────

    #[test]
    fn remove_nonexistent_route_no_panic() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(1);
            hew_routing_remove_route(table, 42); // must not panic
            hew_routing_table_free(table);
        }
    }

    #[test]
    fn remove_then_lookup_returns_minus_one() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(1);
            let pid = (u64::from(3u16) << PID_SERIAL_BITS) | 0xF;
            hew_routing_add_route(table, 3, 77);
            assert_eq!(hew_routing_lookup(table, pid), 77);
            hew_routing_remove_route(table, 3);
            assert_eq!(hew_routing_lookup(table, pid), -1);
            hew_routing_table_free(table);
        }
    }

    // ── is_local ──────────────────────────────────────────────────────

    #[test]
    fn is_local_with_different_serials() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(7);
            let pid_a = (u64::from(7u16) << PID_SERIAL_BITS) | 1;
            let pid_b = (u64::from(7u16) << PID_SERIAL_BITS) | 9999;
            assert_eq!(hew_routing_is_local(table, pid_a), 1);
            assert_eq!(hew_routing_is_local(table, pid_b), 1);
            hew_routing_table_free(table);
        }
    }

    #[test]
    fn is_local_remote_pid() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(7);
            let pid = (u64::from(8u16) << PID_SERIAL_BITS) | 1;
            assert_eq!(hew_routing_is_local(table, pid), 0);
            hew_routing_table_free(table);
        }
    }

    // ── multiple routes ───────────────────────────────────────────────

    #[test]
    fn multiple_distinct_routes_coexist() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(0);
            hew_routing_add_route(table, 1, 10);
            hew_routing_add_route(table, 2, 20);
            hew_routing_add_route(table, 3, 30);
            let pid1 = u64::from(1u16) << PID_SERIAL_BITS;
            let pid2 = u64::from(2u16) << PID_SERIAL_BITS;
            let pid3 = u64::from(3u16) << PID_SERIAL_BITS;
            assert_eq!(hew_routing_lookup(table, pid1), 10);
            assert_eq!(hew_routing_lookup(table, pid2), 20);
            assert_eq!(hew_routing_lookup(table, pid3), 30);
            hew_routing_table_free(table);
        }
    }

    // ── serial bits are transparent to routing ────────────────────────

    #[test]
    fn different_serials_same_node_resolve_same_route() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(0);
            hew_routing_add_route(table, 5, 42);
            let pid_a = (u64::from(5u16) << PID_SERIAL_BITS) | 1;
            let pid_b = (u64::from(5u16) << PID_SERIAL_BITS) | 0xFFFF;
            assert_eq!(hew_routing_lookup(table, pid_a), 42);
            assert_eq!(hew_routing_lookup(table, pid_b), 42);
            hew_routing_table_free(table);
        }
    }

    // ── local short-circuit ───────────────────────────────────────────

    #[test]
    fn local_pid_returns_minus_one_despite_route_existing() {
        // The local-node check short-circuits before the route lookup,
        // so even an explicitly added route for the local node_id is
        // unreachable.
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(7);
            hew_routing_add_route(table, 7, 999);
            let local_pid = (u64::from(7u16) << PID_SERIAL_BITS) | 0x42;
            assert_eq!(hew_routing_lookup(table, local_pid), -1);
            assert_eq!(hew_routing_is_local(table, local_pid), 1);
            hew_routing_table_free(table);
        }
    }

    // ── re-add after remove ───────────────────────────────────────────

    #[test]
    fn readd_route_after_removal() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(0);
            let pid = (u64::from(10u16) << PID_SERIAL_BITS) | 1;
            hew_routing_add_route(table, 10, 55);
            assert_eq!(hew_routing_lookup(table, pid), 55);
            hew_routing_remove_route(table, 10);
            assert_eq!(hew_routing_lookup(table, pid), -1);
            hew_routing_add_route(table, 10, 88);
            assert_eq!(hew_routing_lookup(table, pid), 88);
            hew_routing_table_free(table);
        }
    }

    // ── node_id 0 ─────────────────────────────────────────────────────

    #[test]
    fn node_id_zero_routes_correctly() {
        // SAFETY: pointer lifecycle bounded to this test.
        unsafe {
            let table = hew_routing_table_new(1); // local is 1, not 0
            hew_routing_add_route(table, 0, 77);
            let pid = 0x0000_0000_0000_0001u64; // node_id = 0, serial = 1
            assert_eq!(hew_routing_lookup(table, pid), 77);
            assert_eq!(hew_routing_is_local(table, pid), 0);
            hew_routing_table_free(table);
        }
    }
}
