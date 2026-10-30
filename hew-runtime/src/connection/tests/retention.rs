//! Retiring the claims and routing sessions of members the cluster retired.

use super::*;
use crate::routing::{
    hew_routing_add_route, hew_routing_lookup_location, hew_routing_remove_route_if_conn,
    hew_routing_table_free, hew_routing_table_new_for_test, LocationRoute,
};

/// Past every retention window at the default suspect timeout.
const AFTER_RETENTION_MS: u64 = 1_000_000;

struct Harness {
    mgr: *mut HewConnMgr,
    transport: *mut HewTransport,
    routing: *mut crate::routing::HewRoutingTable,
    cluster: *mut crate::cluster::HewCluster,
}

impl Harness {
    fn new() -> Self {
        let config = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        let transport = Box::into_raw(Box::new(HewTransport {
            ops: std::ptr::null(),
            r#impl: std::ptr::null_mut(),
        }));
        // SAFETY: every pointer is freed in `Drop` after the manager.
        unsafe {
            let routing = hew_routing_table_new_for_test(1);
            let cluster = crate::cluster::hew_cluster_new(&raw const config);
            let mgr = hew_connmgr_new(transport, None, routing, cluster, 1);
            assert!(!mgr.is_null());
            Self {
                mgr,
                transport,
                routing,
                cluster,
            }
        }
    }

    fn mgr(&self) -> &HewConnMgr {
        // SAFETY: the harness owns the live manager.
        unsafe { &*self.mgr }
    }

    fn cluster(&self) -> &crate::cluster::HewCluster {
        // SAFETY: the harness owns the live cluster.
        unsafe { &*self.cluster }
    }

    /// A discovered peer admitted on `slot`, whose connection has closed and
    /// which the cluster then buried in `session`.
    fn departed_peer(&self, slot: u16, identity: NodeId, session: u32, token: u64) {
        self.mgr().claims.0.lock_or_recover().insert(
            identity,
            crate::peer_binding::LiveClaim {
                credential: None,
                route_slot: slot,
                session_incarnation: session,
                conn_id: i32::from(slot),
                publication_token: token,
                state: ClaimState::Retired,
            },
        );
        // SAFETY: the harness owns the live routing table.
        unsafe {
            assert!(hew_routing_add_route(
                self.routing,
                identity,
                slot,
                session,
                i32::from(slot),
                token
            ));
            assert!(hew_routing_remove_route_if_conn(
                self.routing,
                identity,
                i32::from(slot),
                token
            ));
        }
        self.cluster()
            .apply_swim_gossip(&[(slot, session, crate::cluster::MEMBER_DEAD, 2)]);
        assert_eq!(
            self.cluster().member_state(slot),
            crate::cluster::MEMBER_DEAD
        );
    }

    fn lookup(&self, identity: NodeId, session: u32) -> LocationRoute {
        // SAFETY: the harness owns the live routing table.
        unsafe {
            hew_routing_lookup_location(
                self.routing,
                Location::direct(identity, 42, session).expect("location"),
            )
        }
    }
}

impl Drop for Harness {
    fn drop(&mut self) {
        // SAFETY: the manager goes first, then what it borrowed.
        unsafe {
            hew_connmgr_free(self.mgr);
            crate::cluster::hew_cluster_free(self.cluster);
            hew_routing_table_free(self.routing);
            drop(Box::from_raw(self.transport));
        }
    }
}

fn identity(n: u16) -> NodeId {
    let mut bytes = [0xab_u8; 16];
    bytes[14..].copy_from_slice(&n.to_be_bytes());
    NodeId::from_bytes(bytes)
}

/// A discovered peer that leaves for good stops reading as partitioned once
/// membership retires it, and its route slot is free for the next identity.
/// Counterfactual: the retired claim and routing session stay, so the peer
/// reports `Partition` forever and its slot is never reassigned.
#[test]
fn retired_discovered_peer_is_stale_and_frees_its_slot() {
    let harness = Harness::new();
    let gone = identity(2);
    harness.departed_peer(2, gone, 5, 9);
    assert_eq!(harness.lookup(gone, 5), LocationRoute::Partition);

    retire_departed_peers(harness.mgr(), 0);
    assert!(
        harness.mgr().claims.0.lock_or_recover().contains_key(&gone),
        "a buried peer keeps its claim within its window"
    );
    assert_eq!(harness.lookup(gone, 5), LocationRoute::Partition);

    retire_departed_peers(harness.mgr(), AFTER_RETENTION_MS);
    assert!(harness.mgr().claims.0.lock_or_recover().is_empty());
    assert_eq!(harness.lookup(gone, 5), LocationRoute::StaleRef);
    // SAFETY: the harness owns the live routing table.
    assert_eq!(unsafe { &*harness.routing }.remembered_count(), 0);
    let claims = harness.mgr().claims.0.lock_or_recover();
    assert_eq!(
        super::super::identity_claim::select_route_alias(&claims, 1, identity(3), 0),
        Some(2)
    );
}

/// Only a retired claim whose member is gone is dropped. A claim that is
/// published or mid-admission is a live owner and stays even when the
/// cluster has no member for its slot.
#[test]
fn live_claims_are_kept() {
    let harness = Harness::new();
    harness.departed_peer(2, identity(2), 5, 9);
    harness.departed_peer(3, identity(3), 5, 10);
    harness.departed_peer(4, identity(4), 5, 11);
    for (n, state) in [(3, ClaimState::Published), (4, ClaimState::Reserved)] {
        harness
            .mgr()
            .claims
            .0
            .lock_or_recover()
            .get_mut(&identity(n))
            .expect("claim")
            .state = state;
    }
    retire_departed_peers(harness.mgr(), 0);
    retire_departed_peers(harness.mgr(), AFTER_RETENTION_MS);
    assert_eq!(harness.cluster().member_state(3), -1);
    let claims = harness.mgr().claims.0.lock_or_recover();
    assert!(!claims.contains_key(&identity(2)));
    assert!(claims.contains_key(&identity(3)));
    assert!(claims.contains_key(&identity(4)));
}

/// Churn through 1000 departing identities keeps claims and routing records
/// within the cluster's cap on buried members.
#[test]
fn claims_and_routing_stay_bounded_under_churn() {
    let harness = Harness::new();
    for n in 0..1000_u16 {
        let slot = 2 + n;
        harness.departed_peer(slot, identity(slot), 1, u64::from(slot));
        retire_departed_peers(harness.mgr(), u64::from(n));
        assert!(harness.mgr().claims.0.lock_or_recover().len() <= 64);
        // SAFETY: the harness owns the live routing table.
        assert!(unsafe { &*harness.routing }.remembered_count() <= 64);
    }
}

/// A reconnect whose predecessor claim was retired with its member is a
/// fresh admission; a predecessor superseded by another publication is still
/// stale. Counterfactual: the absent claim refuses the retry for good.
#[test]
fn reconnect_after_retirement_is_a_fresh_admission() {
    let harness = Harness::new();
    let predecessor = ReconnectOwner {
        node_id: identity(2),
        session_incarnation: 5,
        publication_token: 9,
    };
    let reserve = |token| {
        reserve_identity_claim(
            harness.mgr(),
            identity(2),
            2,
            5,
            None,
            30,
            token,
            Some(&predecessor),
        )
    };
    assert!(matches!(
        reserve(31),
        ClaimReservation::Reserved { superseded: None }
    ));
    harness
        .mgr()
        .claims
        .0
        .lock_or_recover()
        .get_mut(&identity(2))
        .expect("claim")
        .state = ClaimState::Retired;
    assert!(matches!(reserve(32), ClaimReservation::Rejected(_)));
}
