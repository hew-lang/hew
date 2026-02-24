//! PID-based routing table for distributed message delivery.
//!
//! Maps remote node IDs (high 16 bits of a PID) to active transport
//! connection handles.

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
    let mut routes = match table.routes.write() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
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
    let mut routes = match table.routes.write() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
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

    let routes = match table.routes.read() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_lookup_returns_minus_one() {
        // SAFETY: pointers are created and used within test scope.
        unsafe {
            let table = hew_routing_table_new(7);
            let local_pid = (u64::from(7u16) << PID_SERIAL_BITS) | 42;
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
            let remote_pid = (u64::from(9u16) << PID_SERIAL_BITS) | 100;
            assert_eq!(hew_routing_lookup(table, remote_pid), -1);
            hew_routing_add_route(table, 9, 55);
            assert_eq!(hew_routing_lookup(table, remote_pid), 55);
            assert_eq!(hew_routing_is_local(table, remote_pid), 0);
            hew_routing_remove_route(table, 9);
            assert_eq!(hew_routing_lookup(table, remote_pid), -1);
            hew_routing_table_free(table);
        }
    }
}
