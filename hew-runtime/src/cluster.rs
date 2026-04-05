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

use crate::util::MutexExt;
use std::collections::{HashMap, VecDeque};
use std::ffi::{c_char, c_int, c_void, CStr};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

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

/// Gossip event: a registry name was added (actor registered).
pub const GOSSIP_REGISTRY_ADD: u8 = 5;
/// Gossip event: a registry name was removed (actor unregistered).
pub const GOSSIP_REGISTRY_REMOVE: u8 = 6;

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

/// A registry event for gossip dissemination.
///
/// Propagates actor name registrations and removals across the cluster
/// so that [`crate::hew_node::hew_node_lookup`] can resolve remote actors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryEvent {
    /// The registered actor name.
    pub name: String,
    /// PID of the actor (0 for removals).
    pub actor_pid: u64,
    /// Whether this is an add (`true`) or remove (`false`) event.
    pub is_add: bool,
    /// How many times this event has been piggybacked.
    dissemination_count: u32,
}

#[derive(Debug, Default)]
struct ConnectionTokens {
    current: HashMap<u16, u64>,
    visible: HashMap<u16, u64>,
}

#[derive(Debug)]
enum PublicationTransition {
    Plain,
    TokenEstablished(u64),
    GuardedTokenEstablished {
        publication_token: u64,
        publication_sync: Arc<Mutex<()>>,
        publication_removed: Arc<AtomicBool>,
    },
    TokenLost(u64),
}

#[derive(Debug)]
struct MemberTransition {
    node_id: u16,
    state: i32,
    incarnation: u64,
    is_new_member: bool,
    old_state: Option<i32>,
    publication: PublicationTransition,
}

impl MemberTransition {
    fn membership_event(&self) -> Option<u8> {
        match self.state {
            MEMBER_ALIVE => {
                if self.is_new_member
                    || matches!(self.old_state, Some(prev) if prev != MEMBER_ALIVE)
                {
                    Some(HEW_MEMBERSHIP_EVENT_NODE_JOINED)
                } else {
                    None
                }
            }
            MEMBER_SUSPECT => Some(HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
            MEMBER_DEAD => Some(HEW_MEMBERSHIP_EVENT_NODE_DEAD),
            MEMBER_LEFT => Some(HEW_MEMBERSHIP_EVENT_NODE_LEFT),
            _ => None,
        }
    }

    fn with_publication(mut self, publication: PublicationTransition) -> Self {
        self.publication = publication;
        self
    }
}

#[derive(Debug, Default)]
struct PendingMemberTransitions {
    queue: VecDeque<MemberTransition>,
    draining: bool,
}

/// Callback for registry gossip notifications.
///
/// Signature: `fn(name: *const c_char, actor_pid: u64, is_add: bool, user_data: *mut c_void)`.
pub type HewRegistryGossipCallback = extern "C" fn(*const c_char, u64, bool, *mut c_void);

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
    /// Current connection publication token per peer.
    connection_tokens: Mutex<ConnectionTokens>,
    /// Deferred membership transitions waiting to notify callbacks.
    pending_member_transitions: Mutex<PendingMemberTransitions>,
    /// Recent membership events for gossip dissemination.
    events: Mutex<VecDeque<MemberEvent>>,
    /// Recent registry events for gossip dissemination.
    registry_events: Mutex<VecDeque<RegistryEvent>>,
    /// Our own incarnation number.
    local_incarnation: u64,
    /// Membership change callback.
    callback: Option<MemberChangeCallback>,
    /// Membership event callback.
    membership_callback: Option<HewMembershipCallback>,
    /// User data for [`HewMembershipCallback`].
    membership_callback_user_data: *mut c_void,
    /// Registry gossip callback.
    registry_callback: Option<HewRegistryGossipCallback>,
    /// User data for [`HewRegistryGossipCallback`].
    registry_callback_user_data: *mut c_void,
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
            connection_tokens: Mutex::new(ConnectionTokens::default()),
            pending_member_transitions: Mutex::new(PendingMemberTransitions::default()),
            events: Mutex::new(VecDeque::with_capacity(MAX_GOSSIP_EVENTS)),
            registry_events: Mutex::new(VecDeque::with_capacity(MAX_GOSSIP_EVENTS)),
            local_incarnation: 1,
            callback: None,
            membership_callback: None,
            membership_callback_user_data: std::ptr::null_mut(),
            registry_callback: None,
            registry_callback_user_data: std::ptr::null_mut(),
            last_tick_ms: 0,
            ping_index: 0,
        }
    }

    /// Add or update a member in the membership list.
    fn stage_member_transition_locked(
        members: &mut Vec<ClusterMember>,
        node_id: u16,
        state: i32,
        incarnation: u64,
        addr: &[u8],
    ) -> Option<MemberTransition> {
        if let Some(existing) = members.iter_mut().find(|m| m.node_id == node_id) {
            if existing.state == MEMBER_DEAD && state == MEMBER_ALIVE {
                eprintln!("[cluster] ignoring ALIVE for dead node {node_id}");
                return None;
            }
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
                return Some(MemberTransition {
                    node_id,
                    state,
                    incarnation,
                    is_new_member: false,
                    old_state: Some(old_state),
                    publication: PublicationTransition::Plain,
                });
            }
            return None;
        }

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
        Some(MemberTransition {
            node_id,
            state,
            incarnation,
            is_new_member: true,
            old_state: None,
            publication: PublicationTransition::Plain,
        })
    }

    fn queue_member_transition(&self, transition: MemberTransition) -> bool {
        let mut pending = self.pending_member_transitions.lock_or_recover();
        pending.queue.push_back(transition);
        if pending.draining {
            return false;
        }
        pending.draining = true;
        true
    }

    fn drain_member_transitions(&self) {
        loop {
            let transition = {
                let mut pending = self.pending_member_transitions.lock_or_recover();
                if let Some(transition) = pending.queue.pop_front() {
                    transition
                } else {
                    pending.draining = false;
                    return;
                }
            };
            self.deliver_member_transition(&transition);
        }
    }

    fn deliver_member_transition(&self, transition: &MemberTransition) {
        let membership_event = transition.membership_event();
        let deliver = match &transition.publication {
            PublicationTransition::Plain => {
                if matches!(membership_event, Some(HEW_MEMBERSHIP_EVENT_NODE_JOINED)) {
                    let mut tokens = self.connection_tokens.lock_or_recover();
                    if let Some(publication_token) =
                        tokens.current.get(&transition.node_id).copied()
                    {
                        tokens.visible.insert(transition.node_id, publication_token);
                    }
                }
                true
            }
            PublicationTransition::TokenEstablished(publication_token) => {
                let mut tokens = self.connection_tokens.lock_or_recover();
                match tokens.current.get(&transition.node_id) {
                    Some(current) if *current == *publication_token => {
                        tokens
                            .visible
                            .insert(transition.node_id, *publication_token);
                        true
                    }
                    _ => false,
                }
            }
            PublicationTransition::GuardedTokenEstablished {
                publication_token,
                publication_sync,
                publication_removed,
            } => {
                let _publication = publication_sync.lock_or_recover();
                let mut tokens = self.connection_tokens.lock_or_recover();
                let should_deliver = if publication_removed.load(Ordering::Acquire) {
                    false
                } else {
                    match tokens.current.get(&transition.node_id) {
                        Some(current) if *current == *publication_token => {
                            let members = self.members.lock_or_recover();
                            matches!(
                                members.iter().find(|m| m.node_id == transition.node_id),
                                Some(member)
                                    if member.state == transition.state
                                        && member.incarnation == transition.incarnation
                            )
                        }
                        _ => false,
                    }
                };
                if should_deliver {
                    tokens
                        .visible
                        .insert(transition.node_id, *publication_token);
                    true
                } else {
                    if matches!(
                        tokens.current.get(&transition.node_id),
                        Some(current) if *current == *publication_token
                    ) {
                        tokens.current.remove(&transition.node_id);
                    }
                    if matches!(
                        tokens.visible.get(&transition.node_id),
                        Some(current) if *current == *publication_token
                    ) {
                        tokens.visible.remove(&transition.node_id);
                    }
                    false
                }
            }
            PublicationTransition::TokenLost(publication_token) => {
                let mut tokens = self.connection_tokens.lock_or_recover();
                match tokens.visible.get(&transition.node_id) {
                    Some(current) if *current == *publication_token => {
                        tokens.visible.remove(&transition.node_id);
                        true
                    }
                    _ => false,
                }
            }
        };
        if !deliver {
            return;
        }

        self.emit_event(transition.node_id, transition.state, transition.incarnation);
        self.notify_callback(transition.node_id, transition.state, transition.incarnation);
        if let (Some(cb), Some(event)) = (self.membership_callback, membership_event) {
            cb(
                transition.node_id,
                event,
                self.membership_callback_user_data,
            );
        }
    }

    fn upsert_member(&self, node_id: u16, state: i32, incarnation: u64, addr: &[u8]) {
        let mut should_drain = false;
        {
            let mut members = self.members.lock_or_recover();
            if let Some(transition) = Self::stage_member_transition_locked(
                &mut members,
                node_id,
                state,
                incarnation,
                addr,
            ) {
                should_drain = self.queue_member_transition(transition);
            }
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    /// Emit a gossip event.
    fn emit_event(&self, node_id: u16, new_state: i32, incarnation: u64) {
        let mut events = self.events.lock_or_recover();
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
    #[cfg_attr(
        not(test),
        expect(
            dead_code,
            reason = "used when wiring gossip into SWIM message piggybacking"
        )
    )]
    fn take_gossip(&self, max_count: usize) -> Vec<MemberEvent> {
        let mut events = self.events.lock_or_recover();
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
                "[cluster] rejecting message: from_node {from_node} doesn't match connection node {source_conn_node_id}"
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
        let mut members = self.members.lock_or_recover();
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

        let mut members = self.members.lock_or_recover();

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
        let mut should_drain = false;
        let known_member = {
            let mut members = self.members.lock_or_recover();
            let member = members
                .iter()
                .find(|m| m.node_id == node_id)
                .map(|m| (m.state, m.incarnation));
            if let Some((state, incarnation)) = member {
                if state == MEMBER_ALIVE {
                    if let Some(transition) = Self::stage_member_transition_locked(
                        &mut members,
                        node_id,
                        MEMBER_SUSPECT,
                        incarnation,
                        &[],
                    ) {
                        should_drain = self.queue_member_transition(transition);
                    }
                }
                true
            } else {
                false
            }
        };

        if !known_member {
            eprintln!(
                "hew-runtime cluster warning: ignoring connection_lost for unknown node_id={node_id}"
            );
            return;
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    fn notify_connection_lost_if_current(&self, node_id: u16, publication_token: u64) {
        let mut should_drain = false;
        let known_member = {
            let mut tokens = self.connection_tokens.lock_or_recover();
            match tokens.current.get(&node_id) {
                Some(current) if *current == publication_token => {
                    tokens.current.remove(&node_id);
                }
                _ => return,
            }

            let mut clear_visible_now = false;
            let known_member = {
                let mut members = self.members.lock_or_recover();
                let member = members
                    .iter()
                    .find(|m| m.node_id == node_id)
                    .map(|m| (m.state, m.incarnation));
                if let Some((state, incarnation)) = member {
                    if state == MEMBER_ALIVE {
                        if let Some(transition) = Self::stage_member_transition_locked(
                            &mut members,
                            node_id,
                            MEMBER_SUSPECT,
                            incarnation,
                            &[],
                        ) {
                            should_drain =
                                self.queue_member_transition(transition.with_publication(
                                    PublicationTransition::TokenLost(publication_token),
                                ));
                        }
                    } else {
                        clear_visible_now = true;
                    }
                    true
                } else {
                    clear_visible_now = true;
                    false
                }
            };

            if clear_visible_now
                && matches!(
                    tokens.visible.get(&node_id),
                    Some(current) if *current == publication_token
                )
            {
                tokens.visible.remove(&node_id);
            }
            known_member
        };
        if !known_member {
            eprintln!(
                "hew-runtime cluster warning: ignoring connection_lost for unknown node_id={node_id}"
            );
            return;
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    /// Notify SWIM state machine that a connection was established.
    fn notify_connection_established(&self, node_id: u16) {
        let mut should_drain = false;
        let known_member = {
            let mut members = self.members.lock_or_recover();
            let member = members
                .iter()
                .find(|m| m.node_id == node_id)
                .map(|m| (m.state, m.incarnation));
            if let Some((state, incarnation)) = member {
                if state == MEMBER_ALIVE {
                    if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                        // SAFETY: hew_now_ms has no preconditions.
                        member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                    }
                } else {
                    let incarnation = incarnation.saturating_add(1);
                    if let Some(transition) = Self::stage_member_transition_locked(
                        &mut members,
                        node_id,
                        MEMBER_ALIVE,
                        incarnation,
                        &[],
                    ) {
                        if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                            // SAFETY: hew_now_ms has no preconditions.
                            member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                        }
                        should_drain = self.queue_member_transition(transition);
                    }
                }
                true
            } else {
                false
            }
        };

        if !known_member {
            eprintln!("[cluster] unknown node {node_id} connected, waiting for join");
            return;
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    fn notify_connection_established_for_token(&self, node_id: u16, publication_token: u64) {
        let mut should_drain = false;
        let known_member = {
            let mut tokens = self.connection_tokens.lock_or_recover();
            tokens.current.insert(node_id, publication_token);

            let mut members = self.members.lock_or_recover();
            let member = members
                .iter()
                .find(|m| m.node_id == node_id)
                .map(|m| (m.state, m.incarnation));
            if let Some((state, incarnation)) = member {
                if state == MEMBER_ALIVE {
                    if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                        // SAFETY: hew_now_ms has no preconditions.
                        member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                    }
                    tokens.visible.insert(node_id, publication_token);
                } else {
                    let incarnation = incarnation.saturating_add(1);
                    if let Some(transition) = Self::stage_member_transition_locked(
                        &mut members,
                        node_id,
                        MEMBER_ALIVE,
                        incarnation,
                        &[],
                    ) {
                        if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                            // SAFETY: hew_now_ms has no preconditions.
                            member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                        }
                        should_drain = self.queue_member_transition(transition.with_publication(
                            PublicationTransition::TokenEstablished(publication_token),
                        ));
                    }
                }
                true
            } else {
                false
            }
        };

        if !known_member {
            eprintln!("[cluster] unknown node {node_id} connected, waiting for join");
            return;
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    /// Get the next ping target (round-robin through members).
    fn next_ping_target(&mut self) -> Option<u16> {
        let members = self.members.lock_or_recover();

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

    // ── Registry gossip ────────────────────────────────────────────────

    /// Queue a registry add event for gossip dissemination.
    pub fn emit_registry_add(&self, name: &str, actor_pid: u64) {
        let mut events = self.registry_events.lock_or_recover();
        // Deduplicate: remove prior event for the same name.
        events.retain(|e| e.name != name);
        if events.len() >= MAX_GOSSIP_EVENTS {
            events.pop_front();
        }
        events.push_back(RegistryEvent {
            name: name.to_owned(),
            actor_pid,
            is_add: true,
            dissemination_count: 0,
        });
    }

    /// Queue a registry remove event for gossip dissemination.
    pub fn emit_registry_remove(&self, name: &str) {
        let mut events = self.registry_events.lock_or_recover();
        events.retain(|e| e.name != name);
        if events.len() >= MAX_GOSSIP_EVENTS {
            events.pop_front();
        }
        events.push_back(RegistryEvent {
            name: name.to_owned(),
            actor_pid: 0,
            is_add: false,
            dissemination_count: 0,
        });
    }

    /// Get pending registry gossip events (up to `max_count`),
    /// incrementing dissemination counters and pruning expired events.
    pub fn take_registry_gossip(&self, max_count: usize) -> Vec<RegistryEvent> {
        let mut events = self.registry_events.lock_or_recover();
        let mut result = Vec::with_capacity(max_count);
        for event in events.iter_mut() {
            if result.len() >= max_count {
                break;
            }
            result.push(event.clone());
            event.dissemination_count += 1;
        }
        events.retain(|e| e.dissemination_count < 8);
        result
    }

    /// Get the number of pending registry gossip events.
    pub fn registry_gossip_count(&self) -> usize {
        let events = self.registry_events.lock_or_recover();
        events.len()
    }

    /// Process an inbound registry gossip event received from a peer.
    pub fn apply_registry_event(&self, name: &str, actor_pid: u64, is_add: bool) {
        let Some(cb) = self.registry_callback else {
            return;
        };
        let Ok(c_name) = std::ffi::CString::new(name) else {
            return;
        };
        cb(
            c_name.as_ptr(),
            actor_pid,
            is_add,
            self.registry_callback_user_data,
        );
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
    let members = cluster.members.lock_or_recover();
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

/// Register a callback for registry gossip events.
///
/// The callback receives `(name, actor_pid, is_add, user_data)`.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `callback` must remain valid for the cluster lifetime.
/// - `user_data` must remain valid while callback is registered.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_set_registry_callback(
    cluster: *mut HewCluster,
    callback: HewRegistryGossipCallback,
    user_data: *mut c_void,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &mut *cluster };
    cluster.registry_callback = Some(callback);
    cluster.registry_callback_user_data = user_data;
}

/// Queue a registry-add gossip event for dissemination.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_registry_add(
    cluster: *mut HewCluster,
    name: *const c_char,
    actor_pid: u64,
) {
    if cluster.is_null() || name.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    // SAFETY: caller guarantees `name` is a valid null-terminated C string.
    let name_str = unsafe { CStr::from_ptr(name) }.to_string_lossy();
    cluster.emit_registry_add(&name_str, actor_pid);
}

/// Queue a registry-remove gossip event for dissemination.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_registry_remove(
    cluster: *mut HewCluster,
    name: *const c_char,
) {
    if cluster.is_null() || name.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    // SAFETY: caller guarantees `name` is a valid null-terminated C string.
    let name_str = unsafe { CStr::from_ptr(name) }.to_string_lossy();
    cluster.emit_registry_remove(&name_str);
}

/// Get the number of pending registry gossip events.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_registry_gossip_count(cluster: *mut HewCluster) -> c_int {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "registry gossip count will not exceed c_int range in practice"
    )]
    {
        cluster.registry_gossip_count() as c_int
    }
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

pub(crate) unsafe fn hew_cluster_notify_connection_lost_if_current(
    cluster: *mut HewCluster,
    node_id: u16,
    publication_token: u64,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.notify_connection_lost_if_current(node_id, publication_token);
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

#[cfg_attr(
    not(test),
    expect(dead_code, reason = "token-only helper is exercised by focused tests")
)]
pub(crate) unsafe fn hew_cluster_notify_connection_established_for_token(
    cluster: *mut HewCluster,
    node_id: u16,
    publication_token: u64,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.notify_connection_established_for_token(node_id, publication_token);
    0
}

pub(crate) unsafe fn hew_cluster_notify_connection_established_for_token_if_not_removed(
    cluster: *mut HewCluster,
    node_id: u16,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
    publication_removed: &Arc<AtomicBool>,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let mut should_drain = false;
    let known_member = {
        let _publication = publication_sync.lock_or_recover();
        if publication_removed.load(Ordering::Acquire) {
            return 0;
        }
        let mut tokens = cluster.connection_tokens.lock_or_recover();
        tokens.current.insert(node_id, publication_token);

        let mut members = cluster.members.lock_or_recover();
        let member = members
            .iter()
            .find(|m| m.node_id == node_id)
            .map(|m| (m.state, m.incarnation));
        if let Some((state, old_incarnation)) = member {
            if state == MEMBER_ALIVE {
                if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                    // SAFETY: hew_now_ms has no preconditions.
                    member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                }
                tokens.visible.insert(node_id, publication_token);
            } else {
                let incarnation = old_incarnation.saturating_add(1);
                if let Some(transition) = HewCluster::stage_member_transition_locked(
                    &mut members,
                    node_id,
                    MEMBER_ALIVE,
                    incarnation,
                    &[],
                ) {
                    if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                        // SAFETY: hew_now_ms has no preconditions.
                        member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                    }
                    should_drain = cluster.queue_member_transition(transition.with_publication(
                        PublicationTransition::GuardedTokenEstablished {
                            publication_token,
                            publication_sync: Arc::clone(publication_sync),
                            publication_removed: Arc::clone(publication_removed),
                        },
                    ));
                } else if matches!(
                    tokens.current.get(&node_id),
                    Some(current) if *current == publication_token
                ) {
                    tokens.current.remove(&node_id);
                }
            }
            true
        } else {
            false
        }
    };
    if !known_member {
        eprintln!("[cluster] unknown node {node_id} connected, waiting for join");
    } else if should_drain {
        cluster.drain_member_transitions();
    }
    1
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
    let members = cluster.members.lock_or_recover();
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
    let events = cluster.events.lock_or_recover();
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "gossip count will not exceed c_int range in practice"
    )]
    {
        events.len() as c_int
    }
}

// ── Profiler snapshot ───────────────────────────────────────────────────

/// Build a JSON array of cluster members for the profiler HTTP API.
///
/// Each element: `{"node_id":N,"state":"S","incarnation":N,"addr":"S","last_seen_ms":N}`
#[cfg(feature = "profiler")]
pub fn snapshot_members_json(cluster: &HewCluster) -> String {
    use std::fmt::Write as _;

    // SAFETY: hew_now_ms has no preconditions.
    let now_ms = unsafe { crate::io_time::hew_now_ms() };

    let members = cluster.members.lock_or_recover();

    crate::util::json_array(members.iter(), |json, m| {
        let state_str = match m.state {
            MEMBER_ALIVE => "alive",
            MEMBER_SUSPECT => "suspect",
            MEMBER_DEAD => "dead",
            MEMBER_LEFT => "left",
            _ => "unknown",
        };
        // Extract address as UTF-8 trimmed of null bytes.
        let addr_end = m.addr.iter().position(|&b| b == 0).unwrap_or(m.addr.len());
        let addr = std::str::from_utf8(&m.addr[..addr_end]).unwrap_or("");
        // Emit last_seen_ms as a relative "ms ago" value for the observer client.
        let last_seen_ago_ms = now_ms.saturating_sub(m.last_seen_ms);
        let _ = write!(
            json,
            r#"{{"node_id":{},"state":"{}","incarnation":{},"addr":"#,
            m.node_id, state_str, m.incarnation,
        );
        crate::util::push_json_string(json, addr);
        let _ = write!(json, r#","last_seen_ms":{last_seen_ago_ms}}}"#);
    })
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

    #[cfg(feature = "profiler")]
    #[test]
    fn snapshot_members_json_escapes_addr_field() {
        let cluster = HewCluster::new(make_config(1));
        // SAFETY: hew_now_ms has no preconditions.
        let before_snapshot_ms = unsafe { crate::io_time::hew_now_ms() };
        let fixture_last_seen_ms = before_snapshot_ms.saturating_sub(25);
        let mut member = ClusterMember {
            node_id: 7,
            state: MEMBER_ALIVE,
            incarnation: 42,
            addr: [0; 128],
            last_seen_ms: fixture_last_seen_ms,
        };
        let addr = b"node\"\\\\name\n:9000";
        member.addr[..addr.len()].copy_from_slice(addr);
        cluster.members.lock_or_recover().push(member);

        let json = snapshot_members_json(&cluster);
        // SAFETY: hew_now_ms has no preconditions.
        let after_snapshot_ms = unsafe { crate::io_time::hew_now_ms() };
        let prefix = r#"[{"node_id":7,"state":"alive","incarnation":42,"addr":"node\"\\\\name\n:9000","last_seen_ms":"#;
        assert!(
            json.starts_with(prefix),
            "snapshot should preserve field order and escape the address: {json}"
        );
        assert!(
            json.ends_with("}]"),
            "snapshot should end with a single object: {json}"
        );

        let last_seen_ms = json
            .trim_start_matches(prefix)
            .trim_end_matches("}]")
            .parse::<u64>()
            .expect("last_seen_ms should be numeric");
        let min_elapsed_ms = before_snapshot_ms.saturating_sub(fixture_last_seen_ms);
        let max_elapsed_ms = after_snapshot_ms.saturating_sub(fixture_last_seen_ms);
        assert!(
            (min_elapsed_ms..=max_elapsed_ms).contains(&last_seen_ms),
            "snapshot should emit relative ms-ago output in [{min_elapsed_ms}, {max_elapsed_ms}], got {last_seen_ms}"
        );
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
    #[expect(
        clippy::too_many_lines,
        reason = "test coordinates the stale lost-vs-established race in one place"
    )]
    fn tokenized_connection_lost_serializes_replacement_publish() {
        struct BlockingMembershipState {
            events: std::sync::Mutex<Vec<(u16, u8)>>,
            suspect_seen: std::sync::mpsc::Sender<()>,
            release: std::sync::Arc<std::sync::Barrier>,
        }

        extern "C" fn block_on_suspect(node_id: u16, event: u8, user_data: *mut c_void) {
            // SAFETY: user_data points at the BlockingMembershipState allocated in this test.
            let state = unsafe { &*user_data.cast::<BlockingMembershipState>() };
            state
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push((node_id, event));
            if event == HEW_MEMBERSHIP_EVENT_NODE_SUSPECT {
                state
                    .suspect_seen
                    .send(())
                    .expect("suspect callback should notify the test");
                state.release.wait();
            }
        }

        struct SendCluster(*mut HewCluster);
        // SAFETY: the test keeps the cluster alive until both worker threads join.
        unsafe impl Send for SendCluster {}

        let config = make_config(1);
        let (suspect_tx, suspect_rx) = std::sync::mpsc::channel::<()>();
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let callback_state = Box::into_raw(Box::new(BlockingMembershipState {
            events: std::sync::Mutex::new(Vec::new()),
            suspect_seen: suspect_tx,
            release: std::sync::Arc::clone(&release),
        }));

        // SAFETY: test-owned cluster and callback state remain valid until the
        // explicit free/drop calls after both worker threads complete.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(cluster, block_on_suspect, callback_state.cast());
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );

            let (lost_done_tx, lost_done_rx) = std::sync::mpsc::channel::<()>();
            let lost_cluster = SendCluster(cluster);
            let lost_handle = std::thread::spawn(move || {
                let cluster = lost_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 1);
                assert_eq!(rc, 0);
                lost_done_tx
                    .send(())
                    .expect("lost thread should report completion");
            });

            suspect_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost path should reach the membership callback");

            let (established_done_tx, established_done_rx) = std::sync::mpsc::channel::<()>();
            let established_cluster = SendCluster(cluster);
            let established_handle = std::thread::spawn(move || {
                let cluster = established_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_established_for_token(cluster.0, 2, 2);
                assert_eq!(rc, 0);
                established_done_tx
                    .send(())
                    .expect("established thread should report completion");
            });

            assert_eq!(
                (&*callback_state)
                    .events
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .clone(),
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "replacement publish must not emit JOINED before the lost callback is released"
            );

            release.wait();

            lost_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost thread should finish after the callback is released");
            established_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("replacement publish should complete after lost finishes");

            lost_handle.join().expect("lost thread should not panic");
            established_handle
                .join()
                .expect("established thread should not panic");

            let cluster_ref = &*cluster;
            {
                let members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_ALIVE);
            }
            let events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                ],
                "replacement publish must not slip in between token retirement and the lost update"
            );

            hew_cluster_free(cluster);
            drop(Box::from_raw(callback_state));
        }
    }

    #[test]
    fn tokenized_connection_established_observes_sync_cancellation() {
        struct SendCluster(*mut HewCluster);
        // SAFETY: the test keeps the cluster alive until the worker joins.
        unsafe impl Send for SendCluster {}

        let config = make_config(1);
        let mut events: Vec<(u16, u8)> = Vec::new();
        let publication_sync = std::sync::Arc::new(std::sync::Mutex::new(()));
        let publication_removed = std::sync::Arc::new(AtomicBool::new(false));

        // SAFETY: test context with explicit cleanup before return.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );
            assert_eq!(
                hew_cluster_notify_connection_lost_if_current(cluster, 2, 1),
                0
            );

            let publication_guard = publication_sync
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let members_guard = (&*cluster)
                .members
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let (publish_done_tx, publish_done_rx) = std::sync::mpsc::channel::<c_int>();
            let publish_cluster = SendCluster(cluster);
            let publication_sync_for_thread = std::sync::Arc::clone(&publication_sync);
            let publication_removed_for_thread = std::sync::Arc::clone(&publication_removed);
            let publish_handle = std::thread::spawn(move || {
                let cluster = publish_cluster;
                // SAFETY: test keeps the cluster alive until the worker joins.
                let rc = hew_cluster_notify_connection_established_for_token_if_not_removed(
                    cluster.0,
                    2,
                    2,
                    &publication_sync_for_thread,
                    &publication_removed_for_thread,
                );
                publish_done_tx
                    .send(rc)
                    .expect("publish thread should report completion");
            });

            assert!(
                matches!(
                    publish_done_rx.recv_timeout(std::time::Duration::from_millis(100)),
                    Err(std::sync::mpsc::RecvTimeoutError::Timeout)
                ),
                "publish should remain blocked while the publication lock is held"
            );

            publication_removed.store(true, Ordering::Release);
            drop(publication_guard);
            drop(members_guard);

            assert_eq!(
                publish_done_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("publish thread should complete once cancellation is visible"),
                0
            );
            publish_handle
                .join()
                .expect("publish thread should not panic");

            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "a cancelled delayed publish must not emit a stale JOINED event"
            );
            assert!(
                !(&*cluster)
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .current
                    .contains_key(&2),
                "a cancelled publish must not install a live token"
            );

            hew_cluster_free(cluster);
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "test coordinates delayed guarded publish, tick, and callback release end-to-end"
    )]
    fn tokenized_connection_established_stays_alive_while_guarded_delivery_waits() {
        struct BlockingMembershipState {
            events: std::sync::Mutex<Vec<(u16, u8)>>,
            suspect_seen: std::sync::mpsc::Sender<()>,
            release: std::sync::Arc<std::sync::Barrier>,
            blocked_first_suspect: AtomicBool,
        }

        extern "C" fn block_on_first_suspect(node_id: u16, event: u8, user_data: *mut c_void) {
            // SAFETY: user_data points at the BlockingMembershipState allocated in this test.
            let state = unsafe { &*user_data.cast::<BlockingMembershipState>() };
            state
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push((node_id, event));
            if event == HEW_MEMBERSHIP_EVENT_NODE_SUSPECT
                && !state.blocked_first_suspect.swap(true, Ordering::AcqRel)
            {
                state
                    .suspect_seen
                    .send(())
                    .expect("suspect callback should notify the test");
                state.release.wait();
            }
        }

        struct SendCluster(*mut HewCluster);
        // SAFETY: the test keeps the cluster alive until all worker threads join.
        unsafe impl Send for SendCluster {}

        let config = ClusterConfig {
            local_node_id: 1,
            ping_timeout_ms: 100,
            suspect_timeout_ms: 300,
            ..ClusterConfig::default()
        };
        let publication_sync = std::sync::Arc::new(std::sync::Mutex::new(()));
        let publication_removed = std::sync::Arc::new(AtomicBool::new(false));
        let (suspect_tx, suspect_rx) = std::sync::mpsc::channel::<()>();
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let callback_state = Box::into_raw(Box::new(BlockingMembershipState {
            events: std::sync::Mutex::new(Vec::new()),
            suspect_seen: suspect_tx,
            release: std::sync::Arc::clone(&release),
            blocked_first_suspect: AtomicBool::new(false),
        }));

        // SAFETY: test-owned cluster and callback state remain valid until the
        // explicit free/drop calls after all worker threads complete.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(
                cluster,
                block_on_first_suspect,
                callback_state.cast(),
            );
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );

            let (lost_done_tx, lost_done_rx) = std::sync::mpsc::channel::<()>();
            let lost_cluster = SendCluster(cluster);
            let lost_handle = std::thread::spawn(move || {
                let cluster = lost_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 1);
                assert_eq!(rc, 0);
                lost_done_tx
                    .send(())
                    .expect("lost thread should report completion");
            });

            suspect_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost path should reach the membership callback");

            while crate::io_time::hew_now_ms() < 2 {
                std::thread::sleep(std::time::Duration::from_millis(1));
            }

            {
                let cluster_ref = &*cluster;
                let mut members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter_mut().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_SUSPECT);
                member.last_seen_ms = 1;
            }

            assert_eq!(
                hew_cluster_notify_connection_established_for_token_if_not_removed(
                    cluster,
                    2,
                    2,
                    &publication_sync,
                    &publication_removed,
                ),
                1
            );

            {
                let cluster_ref = &*cluster;
                let members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(
                    member.state, MEMBER_ALIVE,
                    "queued guarded publish must eagerly restore ALIVE state so tick cannot mark DEAD"
                );
                assert_eq!(member.incarnation, 2);
                assert!(
                    member.last_seen_ms > 1,
                    "queued guarded publish must refresh last_seen before callback delivery"
                );
                let guarded_tick = member
                    .last_seen_ms
                    .saturating_add(u64::from(config.ping_timeout_ms));
                drop(members);
                (&mut *cluster).tick(guarded_tick);
            }

            let queued_events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                queued_events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "queued guarded publish must suppress tick-driven DEAD before delayed delivery runs"
            );

            release.wait();

            lost_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost thread should finish after the callback is released");
            lost_handle.join().expect("lost thread should not panic");

            let events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                ],
                "guarded delivery should publish JOINED exactly once after the blocked callback drains"
            );

            let cluster_ref = &*cluster;
            {
                let members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_ALIVE);
                assert_eq!(member.incarnation, 2);
            }
            {
                let tokens = cluster_ref
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert_eq!(tokens.current.get(&2), Some(&2));
                assert_eq!(tokens.visible.get(&2), Some(&2));
            }

            hew_cluster_free(cluster);
            drop(Box::from_raw(callback_state));
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "test coordinates delayed guarded publish cancellation end-to-end"
    )]
    fn tokenized_connection_established_skips_removed_delivery_after_queueing() {
        struct BlockingMembershipState {
            events: std::sync::Mutex<Vec<(u16, u8)>>,
            suspect_seen: std::sync::mpsc::Sender<()>,
            release: std::sync::Arc<std::sync::Barrier>,
            blocked_first_suspect: AtomicBool,
        }

        extern "C" fn block_on_first_suspect(node_id: u16, event: u8, user_data: *mut c_void) {
            // SAFETY: user_data points at the BlockingMembershipState allocated in this test.
            let state = unsafe { &*user_data.cast::<BlockingMembershipState>() };
            state
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push((node_id, event));
            if event == HEW_MEMBERSHIP_EVENT_NODE_SUSPECT
                && !state.blocked_first_suspect.swap(true, Ordering::AcqRel)
            {
                state
                    .suspect_seen
                    .send(())
                    .expect("suspect callback should notify the test");
                state.release.wait();
            }
        }

        struct SendCluster(*mut HewCluster);
        // SAFETY: the test keeps the cluster alive until all worker threads join.
        unsafe impl Send for SendCluster {}

        let config = make_config(1);
        let publication_sync = std::sync::Arc::new(std::sync::Mutex::new(()));
        let publication_removed = std::sync::Arc::new(AtomicBool::new(false));
        let (suspect_tx, suspect_rx) = std::sync::mpsc::channel::<()>();
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let callback_state = Box::into_raw(Box::new(BlockingMembershipState {
            events: std::sync::Mutex::new(Vec::new()),
            suspect_seen: suspect_tx,
            release: std::sync::Arc::clone(&release),
            blocked_first_suspect: AtomicBool::new(false),
        }));

        // SAFETY: test-owned cluster and callback state remain valid until the
        // explicit free/drop calls after all worker threads complete.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(
                cluster,
                block_on_first_suspect,
                callback_state.cast(),
            );
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );

            let (lost_done_tx, lost_done_rx) = std::sync::mpsc::channel::<()>();
            let lost_cluster = SendCluster(cluster);
            let lost_handle = std::thread::spawn(move || {
                let cluster = lost_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 1);
                assert_eq!(rc, 0);
                lost_done_tx
                    .send(())
                    .expect("lost thread should report completion");
            });

            suspect_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost path should reach the membership callback");

            let (publish_done_tx, publish_done_rx) = std::sync::mpsc::channel::<c_int>();
            let publish_cluster = SendCluster(cluster);
            let publication_sync_for_thread = std::sync::Arc::clone(&publication_sync);
            let publication_removed_for_thread = std::sync::Arc::clone(&publication_removed);
            let publish_handle = std::thread::spawn(move || {
                let cluster = publish_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_established_for_token_if_not_removed(
                    cluster.0,
                    2,
                    2,
                    &publication_sync_for_thread,
                    &publication_removed_for_thread,
                );
                publish_done_tx
                    .send(rc)
                    .expect("publish thread should report completion");
            });

            assert_eq!(
                publish_done_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("guarded publish should queue while the lost callback is blocked"),
                1
            );

            {
                let _publication = publication_sync
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                publication_removed.store(true, Ordering::Release);
            }

            let (retire_done_tx, retire_done_rx) = std::sync::mpsc::channel::<()>();
            let retire_cluster = SendCluster(cluster);
            let retire_handle = std::thread::spawn(move || {
                let cluster = retire_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 2);
                assert_eq!(rc, 0);
                retire_done_tx
                    .send(())
                    .expect("retire thread should report completion");
            });

            retire_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("removal should retire the hidden token before lost drains");
            release.wait();

            lost_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost thread should finish after the callback is released");

            lost_handle.join().expect("lost thread should not panic");
            publish_handle
                .join()
                .expect("publish thread should not panic");
            retire_handle
                .join()
                .expect("retire thread should not panic");

            let events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "removing a hidden delayed publish must not emit stale JOINED or duplicate SUSPECT events"
            );
            let cluster_ref = &*cluster;
            {
                let tokens = cluster_ref
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert!(
                    !tokens.current.contains_key(&2),
                    "removing a hidden delayed publish must clear the staged token"
                );
                assert!(
                    !tokens.visible.contains_key(&2),
                    "removing a hidden delayed publish must not leak visibility"
                );
            }
            {
                let members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(
                    member.state, MEMBER_SUSPECT,
                    "removing a hidden delayed publish must leave the node suspect"
                );
            }

            hew_cluster_free(cluster);
            drop(Box::from_raw(callback_state));
        }
    }

    #[test]
    fn tokenized_connection_established_clears_hidden_current_when_guarded_delivery_is_stale() {
        let cluster = HewCluster::new(make_config(1));
        let publication_sync = Arc::new(Mutex::new(()));
        let publication_removed = Arc::new(AtomicBool::new(false));

        cluster.upsert_member(2, MEMBER_SUSPECT, 1, b"10.0.0.1:9000");
        let transition = {
            let mut members = cluster.members.lock_or_recover();
            let transition =
                HewCluster::stage_member_transition_locked(&mut members, 2, MEMBER_ALIVE, 2, &[])
                    .expect("suspect member should stage a guarded ALIVE transition");
            if let Some(member) = members.iter_mut().find(|m| m.node_id == 2) {
                // SAFETY: hew_now_ms has no preconditions.
                member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
            }
            transition.with_publication(PublicationTransition::GuardedTokenEstablished {
                publication_token: 2,
                publication_sync: Arc::clone(&publication_sync),
                publication_removed: Arc::clone(&publication_removed),
            })
        };

        {
            let mut tokens = cluster.connection_tokens.lock_or_recover();
            tokens.current.insert(2, 2);
        }

        cluster.upsert_member(2, MEMBER_DEAD, 3, &[]);
        cluster.deliver_member_transition(&transition);

        {
            let tokens = cluster.connection_tokens.lock_or_recover();
            assert!(
                !tokens.current.contains_key(&2),
                "suppressed guarded delivery must clear the hidden current token"
            );
            assert!(
                !tokens.visible.contains_key(&2),
                "suppressed guarded delivery must not leak visibility"
            );
        }
        {
            let members = cluster.members.lock_or_recover();
            let member = members.iter().find(|m| m.node_id == 2).unwrap();
            assert_eq!(member.state, MEMBER_DEAD);
            assert_eq!(member.incarnation, 3);
        }
    }

    #[test]
    fn tokenized_connection_establish_before_join_promotes_visible_lost() {
        let config = make_config(1);
        let mut events: Vec<(u16, u8)> = Vec::new();

        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );

            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                (&*cluster)
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .visible
                    .get(&2),
                Some(&1),
                "join should publish the pre-join connection token as externally visible"
            );

            assert_eq!(
                hew_cluster_notify_connection_lost_if_current(cluster, 2, 1),
                0
            );
            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "a pre-join tokenized establish should still allow the later tokenized lost"
            );
            {
                let tokens = (&*cluster)
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert!(
                    !tokens.current.contains_key(&2) && !tokens.visible.contains_key(&2),
                    "lost should retire both current and visible token state"
                );
            }

            hew_cluster_free(cluster);
        }
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

    // ── Registry gossip tests ──────────────────────────────────────────

    #[test]
    fn registry_add_event_queued() {
        let cluster = HewCluster::new(make_config(1));
        assert_eq!(cluster.registry_gossip_count(), 0);

        cluster.emit_registry_add("counter", 0x1234);
        assert_eq!(cluster.registry_gossip_count(), 1);

        let events = cluster.take_registry_gossip(10);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].name, "counter");
        assert_eq!(events[0].actor_pid, 0x1234);
        assert!(events[0].is_add);
    }

    #[test]
    fn registry_remove_event_queued() {
        let cluster = HewCluster::new(make_config(1));
        cluster.emit_registry_remove("counter");
        assert_eq!(cluster.registry_gossip_count(), 1);

        let events = cluster.take_registry_gossip(10);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].name, "counter");
        assert_eq!(events[0].actor_pid, 0);
        assert!(!events[0].is_add);
    }

    #[test]
    fn registry_events_deduplicate_by_name() {
        let cluster = HewCluster::new(make_config(1));
        cluster.emit_registry_add("counter", 0x1111);
        cluster.emit_registry_add("counter", 0x2222);
        assert_eq!(cluster.registry_gossip_count(), 1);

        let events = cluster.take_registry_gossip(10);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].actor_pid, 0x2222);
    }

    #[test]
    fn registry_events_pruned_after_dissemination() {
        let cluster = HewCluster::new(make_config(1));
        cluster.emit_registry_add("alpha", 1);
        // Disseminate 8 times to reach the prune threshold.
        for _ in 0..8 {
            let _ = cluster.take_registry_gossip(10);
        }
        assert_eq!(cluster.registry_gossip_count(), 0);
    }

    #[test]
    fn registry_callback_invoked_on_apply() {
        use std::ffi::CStr;

        extern "C" fn collect_registry(
            name: *const c_char,
            pid: u64,
            is_add: bool,
            user_data: *mut c_void,
        ) {
            // SAFETY: test passes a valid Vec pointer.
            let vec = unsafe { &mut *user_data.cast::<Vec<(String, u64, bool)>>() };
            // SAFETY: name is a valid NUL-terminated C string from the cluster callback.
            let s = unsafe { CStr::from_ptr(name) }
                .to_string_lossy()
                .into_owned();
            vec.push((s, pid, is_add));
        }

        let mut cluster = HewCluster::new(make_config(1));
        let mut collected: Vec<(String, u64, bool)> = Vec::new();
        cluster.registry_callback = Some(collect_registry);
        cluster.registry_callback_user_data = (&raw mut collected).cast();

        cluster.apply_registry_event("counter", 0x42, true);
        cluster.apply_registry_event("timer", 0, false);

        assert_eq!(collected.len(), 2);
        assert_eq!(collected[0], ("counter".to_owned(), 0x42, true));
        assert_eq!(collected[1], ("timer".to_owned(), 0, false));
    }

    #[test]
    fn registry_gossip_cabi_round_trip() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert_eq!(hew_cluster_registry_gossip_count(cluster), 0);

            let name = c"my_actor";
            hew_cluster_registry_add(cluster, name.as_ptr(), 0xABCD);
            assert_eq!(hew_cluster_registry_gossip_count(cluster), 1);

            hew_cluster_registry_remove(cluster, name.as_ptr());
            // Dedup replaces the add with a remove.
            assert_eq!(hew_cluster_registry_gossip_count(cluster), 1);

            hew_cluster_free(cluster);
        }
    }

    // ── take_gossip tests ──────────────────────────────────────────────

    #[test]
    fn take_gossip_respects_max_count() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");
        cluster.upsert_member(3, MEMBER_ALIVE, 1, b"10.0.0.2:9000");
        cluster.upsert_member(4, MEMBER_ALIVE, 1, b"10.0.0.3:9000");

        let batch = cluster.take_gossip(2);
        assert_eq!(batch.len(), 2);
        assert_eq!(batch[0].node_id, 2);
        assert_eq!(batch[1].node_id, 3);
    }

    #[test]
    fn take_gossip_prunes_after_eight_disseminations() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");

        // After 7 disseminations, the event should still be present.
        for _ in 0..7 {
            let batch = cluster.take_gossip(10);
            assert_eq!(
                batch.len(),
                1,
                "event should survive before reaching threshold"
            );
        }
        // The 8th take_gossip increments count to 8 and then prunes.
        let batch = cluster.take_gossip(10);
        assert_eq!(batch.len(), 1, "returned on the call that prunes");
        // Now the event is gone.
        let batch = cluster.take_gossip(10);
        assert_eq!(batch.len(), 0, "pruned after 8 disseminations");
    }

    // ── next_ping_target tests ─────────────────────────────────────────

    #[test]
    fn next_ping_target_empty_returns_none() {
        let mut cluster = HewCluster::new(make_config(1));
        assert_eq!(cluster.next_ping_target(), None);
    }

    #[test]
    fn next_ping_target_round_robins_through_members() {
        let mut cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(10, MEMBER_ALIVE, 1, b"a:1");
        cluster.upsert_member(20, MEMBER_ALIVE, 1, b"b:1");

        let first = cluster.next_ping_target().unwrap();
        let second = cluster.next_ping_target().unwrap();
        let third = cluster.next_ping_target().unwrap();
        assert_eq!(first, 10);
        assert_eq!(second, 20);
        // Wraps around.
        assert_eq!(third, 10);
    }

    #[test]
    fn next_ping_target_skips_dead_and_left() {
        let mut cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"a:1");
        cluster.upsert_member(3, MEMBER_DEAD, 5, b"b:1");
        cluster.upsert_member(4, MEMBER_LEFT, 1, b"c:1");
        cluster.upsert_member(5, MEMBER_SUSPECT, 1, b"d:1");

        // Only nodes 2 (alive) and 5 (suspect) should be selected.
        let mut targets = Vec::new();
        for _ in 0..4 {
            targets.push(cluster.next_ping_target().unwrap());
        }
        assert!(!targets.contains(&3), "dead member must not be pinged");
        assert!(!targets.contains(&4), "left member must not be pinged");
        assert!(targets.contains(&2));
        assert!(targets.contains(&5));
    }

    // ── emit_event tests ───────────────────────────────────────────────

    #[test]
    fn emit_event_deduplicates_by_node_id() {
        let cluster = HewCluster::new(make_config(1));
        cluster.emit_event(2, MEMBER_ALIVE, 1);
        cluster.emit_event(3, MEMBER_ALIVE, 1);
        cluster.emit_event(2, MEMBER_SUSPECT, 2);

        let events = cluster.events.lock().unwrap();
        // Node 2's first event should be replaced.
        assert_eq!(events.len(), 2);
        assert_eq!(events[0].node_id, 3);
        assert_eq!(events[1].node_id, 2);
        assert_eq!(events[1].new_state, MEMBER_SUSPECT);
    }

    #[test]
    fn emit_event_evicts_oldest_at_capacity() {
        let cluster = HewCluster::new(make_config(1));
        // Fill to MAX_GOSSIP_EVENTS with distinct node IDs.
        for i in 0..MAX_GOSSIP_EVENTS {
            #[expect(clippy::cast_possible_truncation, reason = "test values fit in u16")]
            let node_id = (i + 100) as u16;
            cluster.emit_event(node_id, MEMBER_ALIVE, 1);
        }
        {
            let events = cluster.events.lock().unwrap();
            assert_eq!(events.len(), MAX_GOSSIP_EVENTS);
        }

        // One more should evict the oldest (node 100).
        cluster.emit_event(999, MEMBER_ALIVE, 1);
        let events = cluster.events.lock().unwrap();
        assert_eq!(events.len(), MAX_GOSSIP_EVENTS);
        assert!(
            !events.iter().any(|e| e.node_id == 100),
            "oldest event (node 100) should have been evicted"
        );
        assert!(events.iter().any(|e| e.node_id == 999));
    }

    // ── process_message tests ──────────────────────────────────────────

    #[test]
    fn process_message_ping_recovers_suspect_to_alive() {
        let mut cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_SUSPECT, 1, b"10.0.0.1:9000");

        // A PING from node 2 should update last_seen and recover to alive.
        cluster.process_message(SWIM_MSG_PING, 2, 1, 2);
        let members = cluster.members.lock().unwrap();
        assert_eq!(members[0].state, MEMBER_ALIVE);
    }

    #[test]
    fn process_message_unknown_type_is_noop() {
        let mut cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");

        // Unknown message type should not change anything.
        cluster.process_message(999, 2, 1, 2);
        let members = cluster.members.lock().unwrap();
        assert_eq!(members[0].state, MEMBER_ALIVE);
    }

    // ── upsert_member address tests ────────────────────────────────────

    #[test]
    fn upsert_member_truncates_long_address() {
        let cluster = HewCluster::new(make_config(1));
        let long_addr = [b'A'; 200];
        cluster.upsert_member(2, MEMBER_ALIVE, 1, &long_addr);

        let members = cluster.members.lock().unwrap();
        // Address should be truncated to 127 bytes + null terminator.
        assert_eq!(&members[0].addr[..127], &[b'A'; 127]);
        assert_eq!(members[0].addr[127], 0);
    }

    // ── tick edge cases ────────────────────────────────────────────────

    #[test]
    fn tick_skips_left_members() {
        let mut cluster = HewCluster::new(ClusterConfig {
            local_node_id: 1,
            ping_timeout_ms: 100,
            suspect_timeout_ms: 300,
            ..ClusterConfig::default()
        });
        cluster.upsert_member(2, MEMBER_LEFT, 1, b"10.0.0.1:9000");
        {
            let mut members = cluster.members.lock().unwrap();
            members[0].last_seen_ms = 0;
        }

        // Even after a long time, LEFT should remain LEFT (not transition to suspect/dead).
        cluster.tick(10_000);
        let members = cluster.members.lock().unwrap();
        assert_eq!(members[0].state, MEMBER_LEFT);
    }

    // ── connection_established on dead member ──────────────────────────

    #[test]
    fn connection_established_dead_member_stays_dead() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_DEAD, 5, b"10.0.0.1:9000");

        // Re-establishing a connection should not revive a dead member.
        cluster.notify_connection_established(2);
        let members = cluster.members.lock().unwrap();
        assert_eq!(
            members[0].state, MEMBER_DEAD,
            "dead member must not be revived via connection"
        );
    }

    // ── apply_registry_event edge cases ────────────────────────────────

    #[test]
    fn apply_registry_event_without_callback_is_noop() {
        let cluster = HewCluster::new(make_config(1));
        // No callback registered — should not panic.
        cluster.apply_registry_event("counter", 42, true);
    }

    #[test]
    fn apply_registry_event_name_with_interior_nul_is_noop() {
        extern "C" fn should_not_be_called(_: *const c_char, _: u64, _: bool, _: *mut c_void) {
            panic!("callback should not be invoked for invalid name");
        }
        let mut cluster = HewCluster::new(make_config(1));
        cluster.registry_callback = Some(should_not_be_called);
        cluster.registry_callback_user_data = std::ptr::null_mut();

        // Name with interior null byte — CString::new fails, early return.
        cluster.apply_registry_event("bad\0name", 42, true);
    }

    // ── membership callback edge cases ─────────────────────────────────

    #[test]
    fn alive_to_alive_upsert_skips_joined_callback() {
        let mut events: Vec<(u16, u8)> = Vec::new();
        let mut cluster = HewCluster::new(make_config(1));
        cluster.membership_callback = Some(collect_membership_events);
        cluster.membership_callback_user_data = (&raw mut events).cast();

        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");
        // Same state, same incarnation — should NOT fire again.
        cluster.upsert_member(2, MEMBER_ALIVE, 1, &[]);

        // Only one JOINED event for the initial insert.
        assert_eq!(events.len(), 1);
        assert_eq!(events[0], (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED));
    }

    // ── registry gossip overflow ───────────────────────────────────────

    #[test]
    fn registry_gossip_overflow_evicts_oldest() {
        let cluster = HewCluster::new(make_config(1));
        for i in 0..MAX_GOSSIP_EVENTS {
            cluster.emit_registry_add(&format!("actor_{i}"), i as u64);
        }
        assert_eq!(cluster.registry_gossip_count(), MAX_GOSSIP_EVENTS);

        // One more should evict the oldest.
        cluster.emit_registry_add("overflow", 999);
        assert_eq!(cluster.registry_gossip_count(), MAX_GOSSIP_EVENTS);

        let events = cluster.take_registry_gossip(MAX_GOSSIP_EVENTS + 1);
        assert!(
            !events.iter().any(|e| e.name == "actor_0"),
            "oldest should be evicted"
        );
        assert!(events.iter().any(|e| e.name == "overflow"));
    }

    // ── extended CABI null safety ──────────────────────────────────────

    #[test]
    fn null_safety_extended() {
        extern "C" fn noop_registry_cb(_: *const c_char, _: u64, _: bool, _: *mut c_void) {}

        // SAFETY: testing null safety of remaining CABI functions.
        unsafe {
            let null: *mut HewCluster = std::ptr::null_mut();

            // Functions that return -1 on null.
            assert_eq!(hew_cluster_join(null, 1, c"addr".as_ptr()), -1);
            assert_eq!(
                hew_cluster_process_message(null, SWIM_MSG_PING, 1, 1, 1),
                -1
            );
            assert_eq!(hew_cluster_notify_connection_lost(null, 1), -1);
            assert_eq!(hew_cluster_notify_connection_established(null, 1), -1);

            // Functions that return 0 on null.
            assert_eq!(hew_cluster_gossip_count(null), 0);
            assert_eq!(hew_cluster_registry_gossip_count(null), 0);

            // Functions that return gracefully on null.
            hew_cluster_leave(null);
            hew_cluster_set_callback(null, None);
            hew_cluster_set_membership_callback(
                null,
                collect_membership_events,
                std::ptr::null_mut(),
            );
            hew_cluster_set_registry_callback(null, noop_registry_cb, std::ptr::null_mut());

            // Null name pointers.
            hew_cluster_registry_add(null, std::ptr::null(), 0);
            hew_cluster_registry_remove(null, std::ptr::null());
        }
    }

    #[test]
    fn join_with_null_addr_returns_error() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert_eq!(hew_cluster_join(cluster, 2, std::ptr::null()), -1);
            assert_eq!(hew_cluster_member_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn hew_cluster_new_null_config_returns_null() {
        // SAFETY: testing null safety.
        unsafe {
            let cluster = hew_cluster_new(std::ptr::null());
            assert!(cluster.is_null());
        }
    }
}
