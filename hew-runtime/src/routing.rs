//! Full-`Location` routing for distributed actor delivery.

use crate::node_identity::{Location, NodeId};
use crate::util::RwLockExt;
use std::collections::{HashMap, HashSet};
use std::ffi::c_int;
use std::sync::RwLock;

/// Return value used by internal route-slot probes when no connection is live.
const HEW_ROUTE_MISSING: c_int = -1;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct RouteEntry {
    route_slot: u16,
    session_incarnation: u32,
    conn: c_int,
}

#[derive(Debug)]
struct RoutingState {
    by_node: HashMap<NodeId, RouteEntry>,
    by_slot: HashMap<u16, NodeId>,
    retired_nodes: HashSet<NodeId>,
    known_sessions: HashMap<NodeId, u32>,
}

/// Exact outcome of resolving a carried actor location.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum LocationRoute {
    /// The location names an actor slot on this node.
    Local { actor_id: u64 },
    /// The location names an actor slot behind a live authenticated connection.
    Remote {
        actor_id: u64,
        route_slot: u16,
        conn: c_int,
    },
    /// The identity is configured/current but has no live route.
    Partition,
    /// The identity, session, or route alias is no longer current.
    StaleRef,
}

/// Maps key-derived node identities to receiver-local transport routes.
#[derive(Debug)]
pub struct HewRoutingTable {
    state: RwLock<RoutingState>,
    configured_nodes: HashSet<NodeId>,
    local_node: Option<NodeId>,
    local_session_incarnation: Option<u32>,
    local_route_slot: u16,
}

/// Create a routing table for one authenticated node lifetime.
pub(crate) fn hew_routing_table_new(
    local_route_slot: u16,
    local_node: Option<NodeId>,
    local_session_incarnation: Option<u32>,
    configured_routes: &[(u16, NodeId)],
) -> *mut HewRoutingTable {
    if local_node.is_some() != local_session_incarnation.is_some()
        || local_session_incarnation == Some(0)
    {
        return std::ptr::null_mut();
    }
    let configured_nodes = configured_routes
        .iter()
        .map(|(_, node_id)| *node_id)
        .collect();
    Box::into_raw(Box::new(HewRoutingTable {
        state: RwLock::new(RoutingState {
            by_node: HashMap::new(),
            by_slot: HashMap::new(),
            retired_nodes: HashSet::new(),
            known_sessions: HashMap::new(),
        }),
        configured_nodes,
        local_node,
        local_session_incarnation,
        local_route_slot,
    }))
}

/// Free a routing table.
///
/// # Safety
///
/// `table` must be null or a pointer returned by [`hew_routing_table_new`].
pub(crate) unsafe fn hew_routing_table_free(table: *mut HewRoutingTable) {
    if !table.is_null() {
        // SAFETY: caller guarantees ownership of a table allocation.
        let _ = unsafe { Box::from_raw(table) };
    }
}

/// Publish or replace a live authenticated route.
///
/// Reusing a receiver-local route slot for a different `NodeId` tombstones the
/// prior identity before the replacement becomes visible.
///
/// # Safety
///
/// `table` must point to a live routing table.
pub(crate) unsafe fn hew_routing_add_route(
    table: *mut HewRoutingTable,
    node_id: NodeId,
    route_slot: u16,
    session_incarnation: u32,
    conn: c_int,
) -> bool {
    if table.is_null() || route_slot == 0 || session_incarnation == 0 {
        return false;
    }
    // SAFETY: caller guarantees `table` validity.
    let table = unsafe { &*table };
    let mut state = table.state.write_or_recover();

    if let Some(previous_node) = state.by_slot.get(&route_slot).copied() {
        if previous_node != node_id {
            state.by_node.remove(&previous_node);
            state.retired_nodes.insert(previous_node);
            state.known_sessions.remove(&previous_node);
        }
    }
    if let Some(previous) = state.by_node.get(&node_id).copied() {
        if previous.route_slot != route_slot {
            state.by_slot.remove(&previous.route_slot);
        }
    }

    state.retired_nodes.remove(&node_id);
    state.known_sessions.insert(node_id, session_incarnation);
    state.by_slot.insert(route_slot, node_id);
    state.by_node.insert(
        node_id,
        RouteEntry {
            route_slot,
            session_incarnation,
            conn,
        },
    );
    true
}

/// Remove a route only when the connection still owns it.
///
/// A normal connection loss leaves the configured identity current, so a later
/// lookup classifies as `Partition`, not `StaleRef`.
///
/// # Safety
///
/// `table` must point to a live routing table.
pub(crate) unsafe fn hew_routing_remove_route_if_conn(
    table: *mut HewRoutingTable,
    node_id: NodeId,
    conn: c_int,
) -> bool {
    if table.is_null() {
        return false;
    }
    // SAFETY: caller guarantees `table` validity.
    let table = unsafe { &*table };
    let mut state = table.state.write_or_recover();
    let Some(entry) = state.by_node.get(&node_id).copied() else {
        return false;
    };
    if entry.conn != conn {
        return false;
    }
    state.by_node.remove(&node_id);
    state.by_slot.remove(&entry.route_slot);
    true
}

/// Resolve an exact carried location.
///
/// # Safety
///
/// `table` must point to a live routing table.
pub(crate) unsafe fn hew_routing_lookup_location(
    table: *const HewRoutingTable,
    location: Location,
) -> LocationRoute {
    if table.is_null() {
        return LocationRoute::StaleRef;
    }
    if !crate::pid::actor_slot_fits_internal_alias(location.slot()) {
        return LocationRoute::StaleRef;
    }
    // SAFETY: caller guarantees `table` validity.
    let table = unsafe { &*table };

    if Some(location.node()) == table.local_node {
        return if Some(location.incarnation()) == table.local_session_incarnation {
            LocationRoute::Local {
                actor_id: crate::pid::hew_pid_make(table.local_route_slot, location.slot()),
            }
        } else {
            LocationRoute::StaleRef
        };
    }

    let state = table.state.read_or_recover();
    if let Some(route) = state.by_node.get(&location.node()).copied() {
        return if route.session_incarnation == location.incarnation() {
            LocationRoute::Remote {
                actor_id: crate::pid::hew_pid_make(route.route_slot, location.slot()),
                route_slot: route.route_slot,
                conn: route.conn,
            }
        } else {
            LocationRoute::StaleRef
        };
    }
    if state.retired_nodes.contains(&location.node()) {
        return LocationRoute::StaleRef;
    }
    if !table.configured_nodes.contains(&location.node()) {
        return LocationRoute::StaleRef;
    }
    match state.known_sessions.get(&location.node()).copied() {
        Some(session) if session != location.incarnation() => LocationRoute::StaleRef,
        Some(_) | None => LocationRoute::Partition,
    }
}

/// Resolve a live route slot directly to its connection handle.
///
/// This remains an internal SWIM/reply-table convenience; route slots do not
/// cross a runtime identity boundary.
///
/// # Safety
///
/// `table` must point to a live routing table.
pub(crate) unsafe fn hew_routing_conn_for_route_slot(
    table: *const HewRoutingTable,
    route_slot: u16,
) -> c_int {
    if table.is_null() || route_slot == 0 {
        return HEW_ROUTE_MISSING;
    }
    // SAFETY: caller guarantees `table` validity.
    let table = unsafe { &*table };
    if route_slot == table.local_route_slot {
        return HEW_ROUTE_MISSING;
    }
    let state = table.state.read_or_recover();
    state
        .by_slot
        .get(&route_slot)
        .and_then(|node_id| state.by_node.get(node_id))
        .map_or(HEW_ROUTE_MISSING, |entry| entry.conn)
}

#[cfg(test)]
pub(crate) fn hew_routing_table_new_for_test(local_route_slot: u16) -> *mut HewRoutingTable {
    let mut bytes = [0_u8; 16];
    bytes[14..].copy_from_slice(&local_route_slot.to_be_bytes());
    hew_routing_table_new(
        local_route_slot,
        Some(NodeId::from_bytes(bytes)),
        Some(1),
        &[],
    )
}

#[cfg(test)]
pub(crate) unsafe fn hew_routing_lookup(table: *const HewRoutingTable, packed_pid: u64) -> c_int {
    let route_slot = crate::pid::hew_pid_node(packed_pid);
    // SAFETY: caller guarantees `table` validity.
    unsafe { hew_routing_conn_for_route_slot(table, route_slot) }
}

#[cfg(feature = "profiler")]
pub fn snapshot_routing_json(table: &HewRoutingTable) -> String {
    use std::fmt::Write as _;

    let state = table.state.read_or_recover();
    let routes_json = crate::util::json_array(state.by_node.iter(), |json, (node_id, route)| {
        let _ = write!(
            json,
            r#"{{"node_id":"{node_id}","route_slot":{},"session_incarnation":{},"conn_id":{}}}"#,
            route.route_slot, route.session_incarnation, route.conn
        );
    });

    format!(
        r#"{{"local_node_id":"{}","local_route_slot":{},"session_incarnation":{},"routes":{routes_json}}}"#,
        table
            .local_node
            .map_or_else(|| "unconfigured".to_owned(), |node| node.to_string()),
        table.local_route_slot,
        table.local_session_incarnation.unwrap_or(0)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(byte: u8) -> NodeId {
        NodeId::from_bytes([byte; 16])
    }

    fn location(node: NodeId, slot: u64, session: u32) -> Location {
        Location::new(node, slot, session).unwrap()
    }

    #[test]
    fn exact_location_resolution_distinguishes_live_partition_and_stale() {
        let local = node(1);
        let remote = node(2);
        let unknown = node(3);
        let table = hew_routing_table_new(7, Some(local), Some(11), &[(9, remote)]);
        assert!(!table.is_null());

        // SAFETY: table is live for the test.
        unsafe {
            assert_eq!(
                hew_routing_lookup_location(table, location(local, 42, 11)),
                LocationRoute::Local {
                    actor_id: crate::pid::hew_pid_make(7, 42)
                }
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(local, 42, 10)),
                LocationRoute::StaleRef
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 5)),
                LocationRoute::Partition
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(unknown, 42, 5)),
                LocationRoute::StaleRef
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(local, 1_u64 << 48, 11)),
                LocationRoute::StaleRef
            );

            assert!(hew_routing_add_route(table, remote, 9, 5, 55));
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 5)),
                LocationRoute::Remote {
                    actor_id: crate::pid::hew_pid_make(9, 42),
                    route_slot: 9,
                    conn: 55
                }
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 4)),
                LocationRoute::StaleRef
            );

            assert!(hew_routing_remove_route_if_conn(table, remote, 55));
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 5)),
                LocationRoute::Partition
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 4)),
                LocationRoute::StaleRef
            );
            assert!(hew_routing_add_route(table, remote, 9, 6, 56));
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 5)),
                LocationRoute::StaleRef
            );
            hew_routing_table_free(table);
        }
    }

    #[test]
    fn route_slot_reuse_tombstones_the_old_identity() {
        let local = node(1);
        let old = node(2);
        let new = node(3);
        let table = hew_routing_table_new(7, Some(local), Some(1), &[(9, old), (9, new)]);

        // SAFETY: table is live for the test.
        unsafe {
            assert!(hew_routing_add_route(table, old, 9, 4, 40));
            assert!(hew_routing_add_route(table, new, 9, 1, 41));
            assert_eq!(
                hew_routing_lookup_location(table, location(old, 8, 4)),
                LocationRoute::StaleRef
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(new, 8, 1)),
                LocationRoute::Remote {
                    actor_id: crate::pid::hew_pid_make(9, 8),
                    route_slot: 9,
                    conn: 41
                }
            );
            hew_routing_table_free(table);
        }
    }
}
