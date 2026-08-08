//! Full-`Location` routing for distributed actor delivery.

use crate::node_identity::{Location, NodeId};
#[cfg(test)]
use crate::util::MutexExt;
use crate::util::RwLockExt;
use std::collections::{HashMap, HashSet};
use std::ffi::c_int;
use std::sync::RwLock;
#[cfg(test)]
use std::sync::{Arc, Mutex};

/// Return value used by internal route-slot probes when no connection is live.
const HEW_ROUTE_MISSING: c_int = -1;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct RouteEntry {
    route_slot: u16,
    session_incarnation: u32,
    conn: c_int,
    publication_token: u64,
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
    /// Test-only park point before route registration begins.
    #[cfg(test)]
    add_route_probe: Mutex<Option<Arc<RouteAddProbe>>>,
}

/// Test-only handshake before a route-registration attempt.
#[cfg(test)]
#[derive(Debug)]
pub(crate) struct RouteAddProbe {
    pub(crate) entered: std::sync::mpsc::Sender<()>,
    pub(crate) release: Mutex<std::sync::mpsc::Receiver<()>>,
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
        #[cfg(test)]
        add_route_probe: Mutex::new(None),
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

/// Check whether an admission's immutable coordinates can be represented by
/// this routing table.
///
/// # Reserved route slots
///
/// Slot `0` is local dispatch and the table's own `local_route_slot` names this
/// process. Registering a peer on either is refused, because route publication is
/// what makes `LocationRoute::Remote`'s packed actor id: `hew_pid_make(slot, ..)`
/// on a reserved slot yields a pid that `hew_pid_is_local` reports as LOCAL for
/// an actor that lives on another node — and `hew_routing_conn_for_route_slot`
/// would refuse to route to it, so the peer would also be silently unreachable.
/// Refusing the registration is what keeps that pid unrepresentable.
///
/// # Safety
///
/// `table` must point to a live routing table.
pub(crate) unsafe fn hew_routing_can_add_route(
    table: *mut HewRoutingTable,
    route_slot: u16,
    session_incarnation: u32,
) -> bool {
    if table.is_null() || route_slot == 0 || session_incarnation == 0 {
        return false;
    }
    // SAFETY: caller guarantees `table` validity.
    let table = unsafe { &*table };
    route_slot != table.local_route_slot
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
    publication_token: u64,
) -> bool {
    // SAFETY: inherited from this function's contract.
    if !unsafe { hew_routing_can_add_route(table, route_slot, session_incarnation) } {
        return false;
    }
    // SAFETY: `hew_routing_can_add_route` accepted the non-null table and the
    // caller guarantees that allocation remains live.
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
            publication_token,
        },
    );
    true
}

#[cfg(test)]
impl HewRoutingTable {
    pub(crate) fn route_add_rendezvous(&self) {
        let probe = self.add_route_probe.lock_or_recover().clone();
        if let Some(probe) = probe {
            let _ = probe.entered.send(());
            let _ = probe.release.lock_or_recover().recv();
        }
    }

    pub(crate) fn set_add_route_probe(&self, probe: Option<Arc<RouteAddProbe>>) {
        *self.add_route_probe.lock_or_recover() = probe;
    }
}

/// Remove a route only when the exact connection publication still owns it.
///
/// Transport connection ids are recycled. Matching the publication token as
/// well prevents a delayed remover from dropping a successor's route after it
/// reuses the same numeric id.
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
    publication_token: u64,
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
    if entry.conn != conn || entry.publication_token != publication_token {
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

            assert!(hew_routing_add_route(table, remote, 9, 5, 55, 1));
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

            assert!(hew_routing_remove_route_if_conn(table, remote, 55, 1));
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 5)),
                LocationRoute::Partition
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 4)),
                LocationRoute::StaleRef
            );
            assert!(hew_routing_add_route(table, remote, 9, 6, 56, 2));
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 5)),
                LocationRoute::StaleRef
            );
            hew_routing_table_free(table);
        }
    }

    /// A delayed removal belongs to one exact route publication, not merely to
    /// its recycled transport id.
    ///
    /// Counterfactual: matching only `conn` lets the first removal below erase
    /// the successor route because both publications deliberately use id 55.
    #[test]
    fn stale_publication_cannot_remove_recycled_conn_route() {
        let local = node(1);
        let remote = node(2);
        let table = hew_routing_table_new(7, Some(local), Some(1), &[(9, remote)]);

        // SAFETY: table is live for the test.
        unsafe {
            assert!(hew_routing_add_route(table, remote, 9, 5, 55, 100));
            assert!(hew_routing_add_route(table, remote, 9, 6, 55, 200));

            assert!(
                !hew_routing_remove_route_if_conn(table, remote, 55, 100),
                "the stale publication must not remove its recycled-id successor"
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 6)),
                LocationRoute::Remote {
                    actor_id: crate::pid::hew_pid_make(9, 42),
                    route_slot: 9,
                    conn: 55,
                },
                "the successor route must remain live"
            );

            assert!(
                hew_routing_remove_route_if_conn(table, remote, 55, 200),
                "the exact successor publication must remove its own route"
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 6)),
                LocationRoute::Partition
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
            assert!(hew_routing_add_route(table, old, 9, 4, 40, 1));
            assert!(hew_routing_add_route(table, new, 9, 1, 41, 2));
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

    /// A peer on the local route slot would mint `LocationRoute::Remote` actor
    /// ids whose high half is this node's own slot, so `hew_pid_is_local` would
    /// report a remote actor as local. Registration is refused, and the peer
    /// stays classified `Partition` rather than acquiring an aliased identity.
    #[test]
    fn peer_on_the_local_route_slot_is_refused() {
        let local = node(1);
        let remote = node(2);
        let table = hew_routing_table_new(7, Some(local), Some(11), &[(7, remote)]);

        // SAFETY: table is live for the test.
        unsafe {
            assert!(
                !hew_routing_add_route(table, remote, 7, 5, 55, 1),
                "a peer must not register on the local route slot"
            );
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 5)),
                LocationRoute::Partition,
                "the refused peer must stay unrouted, never resolve to an aliased id"
            );
            // The neighbouring slot is unaffected: the refusal is exact, not a
            // blanket rejection that would strand every peer.
            assert!(hew_routing_add_route(table, remote, 8, 5, 55, 2));
            assert_eq!(
                hew_routing_lookup_location(table, location(remote, 42, 5)),
                LocationRoute::Remote {
                    actor_id: crate::pid::hew_pid_make(8, 42),
                    route_slot: 8,
                    conn: 55
                }
            );
            hew_routing_table_free(table);
        }
    }

    /// Every route slot a registration accepts yields a pid that
    /// `hew_pid_is_local` classifies as remote — the property the reservation
    /// exists to hold, asserted over the reserved values and a live one.
    #[test]
    fn accepted_route_slots_never_mint_a_locally_classified_pid() {
        let _rt = crate::runtime_test_guard();
        crate::pid::hew_pid_set_local_node(7);

        let local = node(1);
        let remote = node(2);
        let table = hew_routing_table_new(7, Some(local), Some(11), &[(8, remote)]);

        // SAFETY: table is live for the test.
        unsafe {
            for reserved in [0_u16, 7] {
                assert!(
                    !hew_routing_add_route(table, remote, reserved, 5, 55, 1),
                    "route slot {reserved} is reserved and must not be registrable"
                );
            }
            assert!(hew_routing_add_route(table, remote, 8, 5, 55, 2));
            let LocationRoute::Remote { actor_id, .. } =
                hew_routing_lookup_location(table, location(remote, 42, 5))
            else {
                panic!("the registered peer must resolve to a remote route");
            };
            assert_eq!(
                crate::pid::hew_pid_is_local(actor_id),
                0,
                "a remote actor's pid must never classify as local"
            );
            hew_routing_table_free(table);
        }

        crate::pid::hew_pid_set_local_node(0);
    }
}
