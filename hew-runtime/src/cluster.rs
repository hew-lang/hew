//! SWIM-based cluster membership protocol for the Hew runtime.
//!
//! Implements a simplified [SWIM][swim] (Scalable Weakly-consistent
//! Infection-style process group Membership) protocol for discovering
//! and monitoring nodes in a Hew actor cluster.
//!
//! [swim]: https://www.cs.cornell.edu/projects/Quicksilver/public_pdfs/SWIM.pdf
//!
//! # Protocol Summary
//!
//! Each node periodically pings a random peer. If no ACK arrives within
//! a timeout, the node sends indirect pings through K other peers.
//! If those also fail, the peer is marked **suspect**. After a configurable
//! suspect timeout, the peer is declared **dead** and removed.
//!
//! Membership changes (join, leave, dead) are disseminated via
//! **piggyback gossip**: each ping/ack message carries a bounded list
//! of recent membership events.
//!
//! # Architecture
//!
//! ```text
//! HewCluster
//!   ├── members: Vec<ClusterMember>  (current membership list)
//!   ├── events: VecDeque<MemberEvent> (recent gossip, bounded)
//!   └── config: ClusterConfig (timeouts, fanout, seed list)
//! ```
//!
//! # C ABI
//!
//! - [`hew_cluster_new`] — Create a cluster instance.
//! - [`hew_cluster_free`] — Destroy a cluster instance.
//! - [`hew_cluster_join`] — Add a seed node and begin protocol.
//! - [`hew_cluster_leave`] — Gracefully leave the cluster.
//! - [`hew_cluster_members`] — Get current member list.
//! - [`hew_cluster_member_count`] — Number of known members.
//! - [`hew_cluster_process_message`] — Handle an incoming SWIM message.
//! - [`hew_cluster_tick`] — Advance the protocol (call periodically).
//! - [`hew_cluster_set_callback`] — Register membership change callback.
//! - [`hew_cluster_set_membership_callback`] — Register event callback with user data.
//! - [`hew_cluster_notify_connection_lost`] — Notify SWIM when a connection drops.
//! - [`hew_cluster_notify_connection_established`] — Notify SWIM when a connection is restored.

use std::collections::VecDeque;
use std::ffi::{c_char, c_int, c_void, CStr};
use std::sync::Mutex;

// ── Member states ──────────────────────────────────────────────────────

/// Member is alive and responding to pings.
pub const MEMBER_ALIVE: i32 = 0;
/// Member did not respond to ping; awaiting indirect confirmation.
pub const MEMBER_SUSPECT: i32 = 1;
/// Member confirmed unreachable; will be removed.
pub const MEMBER_DEAD: i32 = 2;
/// Member has gracefully left the cluster.
pub const MEMBER_LEFT: i32 = 3;

/// Membership callback event: node joined or became alive.
pub const HEW_MEMBERSHIP_EVENT_NODE_JOINED: u8 = 1;
/// Membership callback event: node became suspect.
pub const HEW_MEMBERSHIP_EVENT_NODE_SUSPECT: u8 = 2;
/// Membership callback event: node declared dead.
pub const HEW_MEMBERSHIP_EVENT_NODE_DEAD: u8 = 3;
/// Membership callback event: node left gracefully.
pub const HEW_MEMBERSHIP_EVENT_NODE_LEFT: u8 = 4;

// ── SWIM message types ─────────────────────────────────────────────────

/// Ping request.
pub const SWIM_MSG_PING: i32 = 1;
/// Ping acknowledgement.
pub const SWIM_MSG_ACK: i32 = 2;
/// Indirect ping request (via intermediary).
pub const SWIM_MSG_PING_REQ: i32 = 3;
/// Membership event dissemination.
pub const SWIM_MSG_GOSSIP: i32 = 4;

// ── Data structures ────────────────────────────────────────────────────

/// Information about a cluster member.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct ClusterMember {
    /// Node ID (matches [`crate::pid`] encoding).
    pub node_id: u16,
    /// Member state (alive, suspect, dead, left).
    pub state: i32,
    /// Incarnation number — monotonically increasing to resolve
    /// conflicts (higher incarnation wins).
    pub incarnation: u64,
    /// Address string (e.g., "192.168.1.10:9000"). Null-terminated.
    /// Owned by the cluster; valid until the member is removed.
    pub addr: [u8; 128],
    /// Monotonic timestamp (ms) of last successful ping response.
    pub last_seen_ms: u64,
}

/// A membership event for gossip dissemination.
#[expect(
    dead_code,
    reason = "fields used in gossip dissemination and serialization"
)]
#[derive(Debug, Clone)]
struct MemberEvent {
    /// Node ID of the affected member.
    node_id: u16,
    /// New state (alive, suspect, dead, left).
    new_state: i32,
    /// Incarnation number.
    incarnation: u64,
    /// How many times this event has been piggybacked.
    dissemination_count: u32,
}

/// Cluster configuration.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct ClusterConfig {
    /// This node's ID.
    pub local_node_id: u16,
    /// Protocol period in milliseconds (how often to ping).
    pub protocol_period_ms: u32,
    /// Ping timeout in milliseconds.
    pub ping_timeout_ms: u32,
    /// Suspect timeout in milliseconds (before declaring dead).
    pub suspect_timeout_ms: u32,
    /// Number of indirect ping targets (K in SWIM).
    pub indirect_ping_count: u32,
    /// Maximum gossip events to piggyback per message.
    pub max_gossip_per_msg: u32,
}

impl Default for ClusterConfig {
    fn default() -> Self {
        Self {
            local_node_id: 0,
            protocol_period_ms: 1000,
            ping_timeout_ms: 500,
            suspect_timeout_ms: 3000,
            indirect_ping_count: 3,
            max_gossip_per_msg: 8,
        }
    }
}

/// Callback for membership change notifications.
///
/// Signature: `fn(node_id: u16, new_state: i32, incarnation: u64)`
type MemberChangeCallback = unsafe extern "C" fn(u16, i32, u64);

/// Callback for connection-lifecycle-integrated membership notifications.
///
/// Signature: `fn(node_id: u16, event: u8, user_data: *mut c_void)`.
pub type HewMembershipCallback = extern "C" fn(u16, u8, *mut c_void);

/// The cluster membership manager.
#[derive(Debug)]
pub struct HewCluster {
    /// Cluster configuration.
    config: ClusterConfig,
    /// Current membership list (protected by mutex for thread safety).
    members: Mutex<Vec<ClusterMember>>,
    /// Recent membership events for gossip dissemination.
    events: Mutex<VecDeque<MemberEvent>>,
    /// Our own incarnation number.
    local_incarnation: u64,
    /// Membership change callback.
    callback: Option<MemberChangeCallback>,
    /// Membership event callback.
    membership_callback: Option<HewMembershipCallback>,
    /// User data for [`HewMembershipCallback`].
    membership_callback_user_data: *mut c_void,
    /// Monotonic timestamp of last tick.
    last_tick_ms: u64,
    /// Index for round-robin ping target selection.
    ping_index: usize,
}

/// Maximum number of gossip events to retain.
const MAX_GOSSIP_EVENTS: usize = 64;

// ── Core protocol logic ────────────────────────────────────────────────

impl HewCluster {
    /// Create a new cluster instance.
    fn new(config: ClusterConfig) -> Self {
        Self {
            config,
            members: Mutex::new(Vec::with_capacity(16)),
            events: Mutex::new(VecDeque::with_capacity(MAX_GOSSIP_EVENTS)),
            local_incarnation: 1,
            callback: None,
            membership_callback: None,
            membership_callback_user_data: std::ptr::null_mut(),
            last_tick_ms: 0,
            ping_index: 0,
        }
    }

    /// Add or update a member in the membership list.
    fn upsert_member(&self, node_id: u16, state: i32, incarnation: u64, addr: &[u8]) {
        let mut members = match self.members.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };

        if let Some(existing) = members.iter_mut().find(|m| m.node_id == node_id) {
            if existing.state == MEMBER_DEAD && state == MEMBER_ALIVE {
                eprintln!("[cluster] ignoring ALIVE for dead node {node_id}");
                return;
            }
            // Only update if the new incarnation is higher.
            if incarnation > existing.incarnation
                || (incarnation == existing.incarnation && state > existing.state)
            {
                let old_state = existing.state;
                existing.state = state;
                existing.incarnation = incarnation;
                if !addr.is_empty() {
                    let len = addr.len().min(127);
                    existing.addr[..len].copy_from_slice(&addr[..len]);
                    existing.addr[len] = 0;
                }
                self.emit_event(node_id, state, incarnation);
                self.notify_callback(node_id, state, incarnation);
                self.notify_membership_callback(node_id, state, false, Some(old_state));
            }
        } else {
            let mut member = ClusterMember {
                node_id,
                state,
                incarnation,
                addr: [0u8; 128],
                last_seen_ms: 0,
            };
            let len = addr.len().min(127);
            member.addr[..len].copy_from_slice(&addr[..len]);
            members.push(member);
            self.emit_event(node_id, state, incarnation);
            self.notify_callback(node_id, state, incarnation);
            self.notify_membership_callback(node_id, state, true, None);
        }
    }

    /// Emit a gossip event.
    fn emit_event(&self, node_id: u16, new_state: i32, incarnation: u64) {
        let mut events = match self.events.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        // Deduplicate: remove older event for this node.
        events.retain(|e| e.node_id != node_id);
        // Add new event.
        if events.len() >= MAX_GOSSIP_EVENTS {
            events.pop_front();
        }
        events.push_back(MemberEvent {
            node_id,
            new_state,
            incarnation,
            dissemination_count: 0,
        });
    }

    /// Notify the callback if registered.
    fn notify_callback(&self, node_id: u16, state: i32, incarnation: u64) {
        if let Some(cb) = self.callback {
            // SAFETY: callback is a valid function pointer per caller contract.
            unsafe { cb(node_id, state, incarnation) };
        }
    }

    /// Notify event callback if registered.
    fn notify_membership_callback(
        &self,
        node_id: u16,
        state: i32,
        is_new_member: bool,
        old_state: Option<i32>,
    ) {
        let Some(cb) = self.membership_callback else {
            return;
        };
        let event = match state {
            MEMBER_ALIVE => {
                if is_new_member || matches!(old_state, Some(prev) if prev != MEMBER_ALIVE) {
                    Some(HEW_MEMBERSHIP_EVENT_NODE_JOINED)
                } else {
                    None
                }
            }
            MEMBER_SUSPECT => Some(HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
            MEMBER_DEAD => Some(HEW_MEMBERSHIP_EVENT_NODE_DEAD),
            MEMBER_LEFT => Some(HEW_MEMBERSHIP_EVENT_NODE_LEFT),
            _ => None,
        };
        if let Some(evt) = event {
            cb(node_id, evt, self.membership_callback_user_data);
        }
    }

    /// Get pending gossip events (up to `max_count`), incrementing
    /// dissemination counters and pruning expired events.
    #[expect(
        dead_code,
        reason = "used when wiring gossip into SWIM message piggybacking"
    )]
    fn take_gossip(&self, max_count: usize) -> Vec<MemberEvent> {
        let mut events = match self.events.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        let mut result = Vec::with_capacity(max_count);
        for event in events.iter_mut() {
            if result.len() >= max_count {
                break;
            }
            result.push(event.clone());
            event.dissemination_count += 1;
        }
        // Remove events that have been disseminated enough times.
        events.retain(|e| e.dissemination_count < 8);
        result
    }

    /// Process a received SWIM message.
    fn process_message(
        &mut self,
        msg_type: i32,
        from_node: u16,
        incarnation: u64,
        source_conn_node_id: u16,
    ) {
        if from_node != source_conn_node_id {
            eprintln!(
                "[cluster] rejecting message: from_node {} doesn't match connection node {}",
                from_node, source_conn_node_id
            );
            return;
        }
        match msg_type {
            SWIM_MSG_PING => {
                // Respond with ACK (caller handles sending the response).
                // Update the sender's last_seen.
                self.update_last_seen(from_node);
            }
            SWIM_MSG_ACK => {
                // Mark the sender as alive.
                self.update_last_seen(from_node);
                self.upsert_member(from_node, MEMBER_ALIVE, incarnation, &[]);
            }
            SWIM_MSG_PING_REQ => {
                // Indirect ping — forward the ping to the target.
                // Caller handles the forwarding.
                self.update_last_seen(from_node);
            }
            _ => {}
        }
    }

    /// Update `last_seen_ms` for a member.
    fn update_last_seen(&self, node_id: u16) {
        let mut members = match self.members.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        if let Some(m) = members.iter_mut().find(|m| m.node_id == node_id) {
            // SAFETY: hew_now_ms has no preconditions.
            m.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
            if m.state == MEMBER_SUSPECT {
                m.state = MEMBER_ALIVE;
            }
        }
    }

    /// Advance the protocol: check for suspects and dead members.
    fn tick(&mut self, now_ms: u64) {
        self.last_tick_ms = now_ms;

        let mut members = match self.members.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };

        let suspect_timeout = u64::from(self.config.suspect_timeout_ms);
        let ping_timeout = u64::from(self.config.ping_timeout_ms);

        let mut state_changes: Vec<(u16, i32, u64)> = Vec::new();

        for member in members.iter_mut() {
            if member.state == MEMBER_DEAD || member.state == MEMBER_LEFT {
                continue;
            }

            let elapsed = now_ms.saturating_sub(member.last_seen_ms);

            if member.state == MEMBER_SUSPECT && elapsed > suspect_timeout {
                // Suspect too long → declare dead.
                member.state = MEMBER_DEAD;
                state_changes.push((member.node_id, MEMBER_DEAD, member.incarnation));
            } else if member.state == MEMBER_ALIVE && elapsed > ping_timeout {
                // No response within ping timeout → suspect.
                member.state = MEMBER_SUSPECT;
                state_changes.push((member.node_id, MEMBER_SUSPECT, member.incarnation));
            }
        }

        drop(members);

        // Emit events and callbacks for state changes.
        for (node_id, state, incarnation) in state_changes {
            self.emit_event(node_id, state, incarnation);
            self.notify_callback(node_id, state, incarnation);
            self.notify_membership_callback(node_id, state, false, None);
        }
    }

    /// Notify SWIM state machine that a connection dropped.
    fn notify_connection_lost(&self, node_id: u16) {
        let member = {
            let members = match self.members.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            members
                .iter()
                .find(|m| m.node_id == node_id)
                .map(|m| (m.state, m.incarnation))
        };

        let Some((state, incarnation)) = member else {
            eprintln!(
                "hew-runtime cluster warning: ignoring connection_lost for unknown node_id={node_id}"
            );
            return;
        };

        if state == MEMBER_ALIVE {
            self.upsert_member(node_id, MEMBER_SUSPECT, incarnation, &[]);
        }
    }

    /// Notify SWIM state machine that a connection was established.
    fn notify_connection_established(&self, node_id: u16) {
        let member = {
            let members = match self.members.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            members
                .iter()
                .find(|m| m.node_id == node_id)
                .map(|m| (m.state, m.incarnation))
        };

        if member.is_none() {
            eprintln!("[cluster] unknown node {node_id} connected, waiting for join");
            return;
        }

        if matches!(member, Some((MEMBER_ALIVE, _))) {
            self.update_last_seen(node_id);
            return;
        }

        let incarnation = member.map_or(1, |(_, inc)| inc.saturating_add(1));
        self.upsert_member(node_id, MEMBER_ALIVE, incarnation, &[]);
        self.update_last_seen(node_id);
    }

    /// Get the next ping target (round-robin through members).
    fn next_ping_target(&mut self) -> Option<u16> {
        let members = match self.members.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };

        let alive_members: Vec<u16> = members
            .iter()
            .filter(|m| m.state == MEMBER_ALIVE || m.state == MEMBER_SUSPECT)
            .map(|m| m.node_id)
            .collect();

        if alive_members.is_empty() {
            return None;
        }

        self.ping_index %= alive_members.len();
        let target = alive_members[self.ping_index];
        self.ping_index = (self.ping_index + 1) % alive_members.len();
        Some(target)
    }
}

// ── C ABI ──────────────────────────────────────────────────────────────

/// Create a new cluster membership manager.
///
/// Returns a pointer to the cluster instance (heap-allocated).
///
/// # Safety
///
/// `config` must be a valid pointer to a [`ClusterConfig`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_new(config: *const ClusterConfig) -> *mut HewCluster {
    if config.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `config` is valid.
    let cfg = unsafe { (*config).clone() };
    let cluster = Box::new(HewCluster::new(cfg));
    Box::into_raw(cluster)
}

/// Destroy a cluster instance.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_free(cluster: *mut HewCluster) {
    if !cluster.is_null() {
        // SAFETY: caller guarantees `cluster` is valid.
        let _ = unsafe { Box::from_raw(cluster) };
    }
}

/// Add a seed node to the cluster.
///
/// The node is added as `MEMBER_ALIVE` with incarnation 1.
/// `addr` must be a null-terminated C string (e.g., "192.168.1.10:9000").
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `addr` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_join(
    cluster: *mut HewCluster,
    node_id: u16,
    addr: *const c_char,
) -> c_int {
    if cluster.is_null() || addr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees both pointers are valid.
    let cluster = unsafe { &*cluster };
    // SAFETY: caller guarantees `addr` is a valid null-terminated C string.
    let addr_str = unsafe { CStr::from_ptr(addr) };
    let addr_bytes = addr_str.to_bytes();

    cluster.upsert_member(node_id, MEMBER_ALIVE, 1, addr_bytes);
    0
}

/// Gracefully leave the cluster by marking self as LEFT.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_leave(cluster: *mut HewCluster) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let local_id = cluster.config.local_node_id;
    cluster.upsert_member(local_id, MEMBER_LEFT, cluster.local_incarnation + 1, &[]);
}

/// Return the number of known members (all states).
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_member_count(cluster: *mut HewCluster) -> c_int {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let members = match cluster.members.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "member count will not exceed c_int range in practice"
    )]
    {
        members.len() as c_int
    }
}

/// Process an incoming SWIM protocol message.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_process_message(
    cluster: *mut HewCluster,
    msg_type: i32,
    from_node: u16,
    incarnation: u64,
    source_conn_node_id: u16,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &mut *cluster };
    cluster.process_message(msg_type, from_node, incarnation, source_conn_node_id);
    0
}

/// Advance the protocol: check timeouts, detect suspects/dead members.
///
/// Should be called periodically (e.g., every `protocol_period_ms`).
///
/// Returns the node ID of the next ping target (0 = none).
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_tick(cluster: *mut HewCluster) -> u16 {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { crate::io_time::hew_now_ms() };
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &mut *cluster };
    cluster.tick(now);
    cluster.next_ping_target().unwrap_or(0)
}

/// Register a callback for membership change notifications.
///
/// The callback receives `(node_id, new_state, incarnation)`.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `callback` must be a valid function pointer that remains valid
///   for the cluster's lifetime (or null to clear).
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_set_callback(
    cluster: *mut HewCluster,
    callback: Option<MemberChangeCallback>,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &mut *cluster };
    cluster.callback = callback;
}

/// Register a callback for membership events with user data.
///
/// The callback receives `(node_id, event, user_data)` where `event` is one
/// of `HEW_MEMBERSHIP_EVENT_NODE_*`.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `callback` must remain valid for the cluster lifetime.
/// - `user_data` must remain valid while callback is registered.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_set_membership_callback(
    cluster: *mut HewCluster,
    callback: HewMembershipCallback,
    user_data: *mut c_void,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &mut *cluster };
    cluster.membership_callback = Some(callback);
    cluster.membership_callback_user_data = user_data;
}

/// Notify SWIM membership that a connection to `node_id` has been lost.
///
/// Returns 0 on success, -1 on invalid cluster pointer.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_notify_connection_lost(
    cluster: *mut HewCluster,
    node_id: u16,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.notify_connection_lost(node_id);
    0
}

/// Notify SWIM membership that a connection to `node_id` has been established.
///
/// Returns 0 on success, -1 on invalid cluster pointer.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_notify_connection_established(
    cluster: *mut HewCluster,
    node_id: u16,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.notify_connection_established(node_id);
    0
}

/// Get the number of alive members.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_alive_count(cluster: *mut HewCluster) -> c_int {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let members = match cluster.members.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "member count will not exceed c_int range in practice"
    )]
    {
        members.iter().filter(|m| m.state == MEMBER_ALIVE).count() as c_int
    }
}

/// Get the number of pending gossip events.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_gossip_count(cluster: *mut HewCluster) -> c_int {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let events = match cluster.events.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "gossip count will not exceed c_int range in practice"
    )]
    {
        events.len() as c_int
    }
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_config(node_id: u16) -> ClusterConfig {
        ClusterConfig {
            local_node_id: node_id,
            ..ClusterConfig::default()
        }
    }

    #[test]
    fn create_and_destroy() {
        let config = make_config(1);
        // SAFETY: config is valid.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            assert_eq!(hew_cluster_member_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn join_adds_member() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"192.168.1.10:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);
            assert_eq!(hew_cluster_member_count(cluster), 1);
            assert_eq!(hew_cluster_alive_count(cluster), 1);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn ack_updates_member() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.1:9000";
            hew_cluster_join(cluster, 2, addr.as_ptr());

            // ACK from node 2 should keep it alive.
            hew_cluster_process_message(cluster, SWIM_MSG_ACK, 2, 1, 2);
            assert_eq!(hew_cluster_alive_count(cluster), 1);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn process_message_rejects_source_mismatch() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.1:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);
            assert_eq!(hew_cluster_notify_connection_lost(cluster, 2), 0);

            // ACK claims to be from node 2, but arrived on node 3 connection.
            hew_cluster_process_message(cluster, SWIM_MSG_ACK, 2, 2, 3);
            {
                let cluster_ref = &*cluster;
                let members = cluster_ref.members.lock().unwrap();
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_SUSPECT);
            }
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn gossip_events_generated() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.1:9000";
            hew_cluster_join(cluster, 2, addr.as_ptr());
            // Joining should generate a gossip event.
            assert_eq!(hew_cluster_gossip_count(cluster), 1);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn null_safety() {
        // All operations on null should return gracefully.
        // SAFETY: testing null safety.
        unsafe {
            let null: *mut HewCluster = std::ptr::null_mut();
            assert_eq!(hew_cluster_member_count(null), 0);
            assert_eq!(hew_cluster_alive_count(null), 0);
            assert_eq!(hew_cluster_tick(null), 0);
            hew_cluster_free(null);
        }
    }

    #[test]
    fn incarnation_supersedes() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");

        // Same incarnation, higher state (suspect) should update.
        cluster.upsert_member(2, MEMBER_SUSPECT, 1, &[]);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_SUSPECT);
        }

        // Higher incarnation, alive again should update.
        cluster.upsert_member(2, MEMBER_ALIVE, 2, &[]);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_ALIVE);
            assert_eq!(members[0].incarnation, 2);
        }

        // Lower incarnation should NOT update.
        cluster.upsert_member(2, MEMBER_DEAD, 1, &[]);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_ALIVE); // unchanged
        }
    }

    #[test]
    fn tick_suspects_and_kills() {
        let mut cluster = HewCluster::new(ClusterConfig {
            local_node_id: 1,
            ping_timeout_ms: 100,
            suspect_timeout_ms: 300,
            ..ClusterConfig::default()
        });
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");
        {
            let mut members = cluster.members.lock().unwrap();
            members[0].last_seen_ms = 0;
        }

        // Advance time past ping timeout → should become suspect.
        cluster.tick(150);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_SUSPECT);
        }

        // Advance time past suspect timeout → should become dead.
        cluster.tick(500);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_DEAD);
        }
    }

    #[test]
    fn dead_member_cannot_be_revived_by_alive_update() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_DEAD, 5, b"10.0.0.1:9000");
        cluster.upsert_member(2, MEMBER_ALIVE, 6, &[]);
        let members = cluster.members.lock().unwrap();
        assert_eq!(members[0].state, MEMBER_DEAD);
        assert_eq!(members[0].incarnation, 5);
    }

    #[test]
    fn leave_marks_self_left() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            // First add ourselves.
            let addr = c"127.0.0.1:9000";
            hew_cluster_join(cluster, 1, addr.as_ptr());
            assert_eq!(hew_cluster_alive_count(cluster), 1);

            hew_cluster_leave(cluster);
            assert_eq!(hew_cluster_alive_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    extern "C" fn collect_membership_events(node_id: u16, event: u8, user_data: *mut c_void) {
        // SAFETY: test passes a valid pointer to `Vec<(u16, u8)>`.
        let events = unsafe { &mut *user_data.cast::<Vec<(u16, u8)>>() };
        events.push((node_id, event));
    }

    #[test]
    fn connection_notifications_update_membership() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.1:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);

            assert_eq!(hew_cluster_notify_connection_lost(cluster, 2), 0);
            let cluster_ref = &*cluster;
            {
                let members = cluster_ref.members.lock().unwrap();
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_SUSPECT);
            }

            assert_eq!(hew_cluster_notify_connection_established(cluster, 2), 0);
            {
                let members = cluster_ref.members.lock().unwrap();
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_ALIVE);
            }
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn membership_callback_receives_connection_events() {
        let config = make_config(1);
        let mut events: Vec<(u16, u8)> = Vec::new();
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );
            let addr = c"10.0.0.1:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);
            assert_eq!(hew_cluster_notify_connection_lost(cluster, 2), 0);
            assert_eq!(hew_cluster_notify_connection_established(cluster, 2), 0);
            hew_cluster_free(cluster);
        }

        assert_eq!(
            events,
            vec![
                (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
            ]
        );
    }

    #[test]
    fn connection_lost_unknown_node_is_ignored() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert_eq!(hew_cluster_notify_connection_lost(cluster, 99), 0);
            assert_eq!(hew_cluster_member_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn connection_established_unknown_node_is_ignored() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert_eq!(hew_cluster_notify_connection_established(cluster, 99), 0);
            assert_eq!(hew_cluster_member_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn cluster_membership_callback_on_connection_lost() {
        let config = make_config(1);
        let mut events: Vec<(u16, u8)> = Vec::new();
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.2:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);
            hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );
            assert_eq!(hew_cluster_notify_connection_lost(cluster, 2), 0);
            hew_cluster_free(cluster);
        }
        assert_eq!(events, vec![(2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT)]);
    }
}
