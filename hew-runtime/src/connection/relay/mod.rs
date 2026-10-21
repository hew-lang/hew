//! Bounded relay carriage for ordinary end-to-end admitted peer sessions.

use std::collections::HashMap;
use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, Weak};

use crate::envelope::{self, ControlFrame, NodeSessionIdentity};
use crate::node_identity::NodeId;
use crate::node_protocol_generated as wire;
use crate::peer_binding::{ClaimState, TransportSelection};
use crate::util::MutexExt;
use crate::{set_last_error, transport::with_connection_attempt_deadline};

use super::reconnect::{collect_finished_reconnect_workers, next_publication_token};
use super::send::claim_send;
use super::{HewConnMgr, ReconnectOwner, SendConnMgr};

pub(super) mod transport;
use transport::Endpoint;

const CHUNK_BYTES: usize = 32 * 1024;
const MAX_CIRCUITS: usize = 64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct Carrier {
    pub(super) conn: c_int,
    pub(super) publication: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct CircuitKey {
    carrier: Carrier,
    id: u64,
}

#[derive(Debug)]
struct Circuit {
    conn_id: c_int,
    left: CircuitKey,
    right: Option<CircuitKey>,
    endpoint: Option<Endpoint>,
    closed: AtomicBool,
}

#[derive(Debug)]
struct Pending {
    generation: u64,
    expected: NodeSessionIdentity,
    circuit: Weak<Circuit>,
}

#[derive(Debug, Default)]
struct State {
    circuits: HashMap<CircuitKey, Arc<Circuit>>,
    slots: Vec<Weak<Circuit>>,
    incoming_open: HashMap<Carrier, u64>,
    pending: HashMap<NodeId, Pending>,
}

#[derive(Debug, Default)]
pub(super) struct RelayHub {
    state: Mutex<State>,
    /// Allocating and sending opens together preserves the per-carrier high-water fence.
    opens: Mutex<()>,
    pub(super) hint_cursor: AtomicUsize,
}

impl RelayHub {
    fn install(&self, left: CircuitKey, right: Option<CircuitKey>) -> Option<Arc<Circuit>> {
        let mut state = self.state.lock_or_recover();
        if state.circuits.contains_key(&left)
            || right.is_some_and(|key| state.circuits.contains_key(&key))
        {
            return None;
        }
        let slot = state
            .slots
            .iter()
            .position(|slot| slot.upgrade().is_none())
            .unwrap_or(state.slots.len());
        if slot >= MAX_CIRCUITS {
            return None;
        }
        let circuit = Arc::new(Circuit {
            conn_id: c_int::try_from(crate::transport::MAX_CONNS + slot).ok()?,
            left,
            right,
            endpoint: right.is_none().then(Endpoint::default),
            closed: AtomicBool::new(false),
        });
        if slot == state.slots.len() {
            state.slots.push(Arc::downgrade(&circuit));
        } else {
            state.slots[slot] = Arc::downgrade(&circuit);
        }
        state.circuits.insert(left, Arc::clone(&circuit));
        if let Some(right) = right {
            state.circuits.insert(right, Arc::clone(&circuit));
        }
        Some(circuit)
    }

    fn accept_open(&self, key: CircuitKey) -> bool {
        let mut state = self.state.lock_or_recover();
        let previous = state.incoming_open.entry(key.carrier).or_default();
        if key.id == 0 || key.id <= *previous {
            return false;
        }
        *previous = key.id;
        true
    }
}

fn local_session(manager: &HewConnMgr) -> Option<NodeSessionIdentity> {
    Some(NodeSessionIdentity {
        node_id: manager.local_identity?,
        session_incarnation: manager.local_session_incarnation?,
    })
}

fn send_control(manager: &HewConnMgr, key: CircuitKey, kind: u64, payload: Vec<u8>) -> bool {
    let Some(claim) = claim_send(manager, key.carrier.conn, Some(key.carrier.publication)) else {
        return false;
    };
    let Ok(frame) = envelope::encode_control_frame(&ControlFrame {
        version: envelope::WIRE_VERSION,
        ctrl_kind: kind,
        payload,
    }) else {
        return false;
    };
    claim.send(&frame) == 0
}

fn send_close(manager: &HewConnMgr, key: CircuitKey) {
    let Some(claim) = claim_send(manager, key.carrier.conn, Some(key.carrier.publication)) else {
        return;
    };
    let payload = wire::RelayClose { circuit: key.id }.encode(hew_codec::Format::Cbor);
    let Ok(frame) = envelope::encode_control_frame(&ControlFrame {
        version: envelope::WIRE_VERSION,
        ctrl_kind: envelope::CTRL_RELAY_CLOSE,
        payload,
    }) else {
        return;
    };
    // Closing never waits on a subscriber or a slow carrier. The existing send
    // lease keeps this exact transport alive until the pool callback returns.
    let _ = claim.submit(move || Some(frame), |_| {});
}

fn close_circuit(manager: &HewConnMgr, circuit: &Arc<Circuit>, source: Option<Carrier>) {
    if circuit.closed.swap(true, Ordering::AcqRel) {
        return;
    }
    {
        let mut state = manager.relay.state.lock_or_recover();
        for key in [Some(circuit.left), circuit.right].into_iter().flatten() {
            if state
                .circuits
                .get(&key)
                .is_some_and(|current| Arc::ptr_eq(current, circuit))
            {
                state.circuits.remove(&key);
            }
        }
    }
    if let Some(endpoint) = &circuit.endpoint {
        endpoint.close();
    }
    for key in [Some(circuit.left), circuit.right].into_iter().flatten() {
        if source != Some(key.carrier) {
            send_close(manager, key);
        }
    }
}

pub(super) fn carrier_lost(manager: &HewConnMgr, conn: c_int, publication: u64) {
    let carrier = Carrier { conn, publication };
    let circuits = {
        let mut state = manager.relay.state.lock_or_recover();
        state.incoming_open.remove(&carrier);
        state
            .circuits
            .iter()
            .filter(|(key, _)| key.carrier == carrier)
            .map(|(_, circuit)| Arc::clone(circuit))
            .collect::<Vec<_>>()
    };
    for circuit in circuits {
        close_circuit(manager, &circuit, Some(carrier));
    }
}

pub(super) fn shutdown(manager: &HewConnMgr) {
    let circuits = manager
        .relay
        .state
        .lock_or_recover()
        .circuits
        .values()
        .cloned()
        .collect::<Vec<_>>();
    for circuit in circuits {
        close_circuit(manager, &circuit, None);
    }
}

fn predecessor(manager: &HewConnMgr, expected: NodeSessionIdentity) -> Option<ReconnectOwner> {
    let claims = manager.claims.0.lock_or_recover();
    let claim = claims.get(&expected.node_id)?;
    (claim.state == ClaimState::Retired
        && claim.session_incarnation == expected.session_incarnation)
        .then_some(ReconnectOwner {
            node_id: expected.node_id,
            session_incarnation: expected.session_incarnation,
            publication_token: claim.publication_token,
        })
}

fn admit(manager: *mut HewConnMgr, circuit: Arc<Circuit>, expected: NodeSessionIdentity) {
    // SAFETY: this worker is tracked until the manager joins it at shutdown.
    let manager_ref = unsafe { &*manager };
    let previous = predecessor(manager_ref, expected);
    let conn_id = circuit.conn_id;
    let io = transport::connection(manager, circuit);
    with_connection_attempt_deadline(|| {
        // SAFETY: the owned I/O lease, tracked worker and exact expected origin
        // remain live throughout the common direct/relayed admission transaction.
        unsafe {
            super::admission::add_connection_on_transport(
                manager,
                conn_id,
                io,
                Some(expected),
                previous.as_ref(),
                &mut None,
            )
        }
    });
}

fn track_worker(manager: &HewConnMgr, work: impl FnOnce() + Send + 'static) -> bool {
    collect_finished_reconnect_workers(manager);
    match std::thread::Builder::new()
        .name("hew-relay-admission".into())
        .spawn(work)
    {
        Ok(worker) => {
            manager
                .reconnect_workers
                .access(|workers| workers.push(worker));
            true
        }
        Err(error) => {
            set_last_error(format!("relay admission worker: {error}"));
            false
        }
    }
}

pub(super) fn observe_hint(
    manager: *mut HewConnMgr,
    carrier: Carrier,
    expected: NodeSessionIdentity,
) {
    // SAFETY: the calling carrier reader pins the manager while this worker is registered.
    let manager_ref = unsafe { &*manager };
    let Some(local) = local_session(manager_ref) else {
        return;
    };
    if manager_ref.reconnect_shutdown.load(Ordering::Acquire)
        || manager_ref.auth.transport() != TransportSelection::Tcp
        || manager_ref.auth.admission().is_none()
        || local.node_id.to_bytes() >= expected.node_id.to_bytes()
    {
        return;
    }
    {
        let claims = manager_ref.claims.0.lock_or_recover();
        if claims.get(&expected.node_id).is_some_and(|claim| {
            claim.session_incarnation > expected.session_incarnation
                || (claim.session_incarnation == expected.session_incarnation
                    && claim.state != ClaimState::Retired)
        }) {
            return;
        }
    }
    let Some(generation) = next_publication_token(manager_ref) else {
        return;
    };
    let displaced = {
        let mut state = manager_ref.relay.state.lock_or_recover();
        if state.pending.get(&expected.node_id).is_some_and(|pending| {
            pending.expected.session_incarnation >= expected.session_incarnation
        }) {
            return;
        }
        let occupied = state
            .slots
            .iter()
            .filter(|slot| slot.upgrade().is_some())
            .count();
        let starting = state
            .pending
            .values()
            .filter(|pending| pending.circuit.upgrade().is_none())
            .count();
        if occupied + starting >= MAX_CIRCUITS {
            return;
        }
        state
            .pending
            .insert(
                expected.node_id,
                Pending {
                    generation,
                    expected,
                    circuit: Weak::new(),
                },
            )
            .and_then(|pending| pending.circuit.upgrade())
    };
    if let Some(displaced) = displaced {
        close_circuit(manager_ref, &displaced, None);
    }
    let send = SendConnMgr(manager);
    if !track_worker(manager_ref, move || {
        hint_worker(&send, carrier, expected, local, generation);
    }) {
        let mut state = manager_ref.relay.state.lock_or_recover();
        if state
            .pending
            .get(&expected.node_id)
            .is_some_and(|pending| pending.generation == generation)
        {
            state.pending.remove(&expected.node_id);
        }
    }
}

fn hint_worker(
    send: &SendConnMgr,
    carrier: Carrier,
    expected: NodeSessionIdentity,
    local: NodeSessionIdentity,
    generation: u64,
) {
    let manager = send.0;
    // SAFETY: this tracked worker is joined before the manager is freed.
    let manager_ref = unsafe { &*manager };
    with_connection_attempt_deadline(|| {
        if let Some(circuit) = open_origin(manager_ref, carrier, expected, local, generation) {
            admit(manager, circuit, expected);
        }
    });
    let mut state = manager_ref.relay.state.lock_or_recover();
    if state
        .pending
        .get(&expected.node_id)
        .is_some_and(|pending| pending.generation == generation)
    {
        state.pending.remove(&expected.node_id);
    }
}

fn open_origin(
    manager_ref: &HewConnMgr,
    carrier: Carrier,
    expected: NodeSessionIdentity,
    local: NodeSessionIdentity,
    generation: u64,
) -> Option<Arc<Circuit>> {
    if manager_ref.reconnect_shutdown.load(Ordering::Acquire) {
        return None;
    }
    let _opens = manager_ref.relay.opens.lock_or_recover();
    claim_send(manager_ref, carrier.conn, Some(carrier.publication))?;
    let key = CircuitKey {
        carrier,
        id: next_publication_token(manager_ref)?,
    };
    let circuit = manager_ref.relay.install(key, None)?;
    let retained = {
        let mut state = manager_ref.relay.state.lock_or_recover();
        state
            .pending
            .get_mut(&expected.node_id)
            .is_some_and(|pending| {
                if pending.generation != generation {
                    return false;
                }
                pending.circuit = Arc::downgrade(&circuit);
                true
            })
    };
    if !retained {
        close_circuit(manager_ref, &circuit, None);
        return None;
    }
    let payload = wire::RelayOpen {
        circuit: key.id,
        origin: envelope::wire_session(local),
        target: envelope::wire_session(expected),
    }
    .encode(hew_codec::Format::Cbor);
    if !send_control(manager_ref, key, envelope::CTRL_RELAY_OPEN, payload) {
        close_circuit(manager_ref, &circuit, None);
        return None;
    }
    Some(circuit)
}

fn admitted_worker(send: &SendConnMgr, circuit: Arc<Circuit>, expected: NodeSessionIdentity) {
    admit(send.0, circuit, expected);
}

pub(super) fn handle_control(
    manager: *mut HewConnMgr,
    conn: c_int,
    publication: u64,
    control: &ControlFrame,
) {
    if control.payload.len() > CHUNK_BYTES + 1024 {
        set_last_error("relay control exceeds its carriage bound");
        return;
    }
    let Some((_, peer)) =
        super::control::authenticated_peer_identity(manager, conn, publication, "relay")
    else {
        return;
    };
    // SAFETY: the authenticated calling reader pins this manager.
    let manager_ref = unsafe { &*manager };
    let carrier = Carrier { conn, publication };
    match control.ctrl_kind {
        envelope::CTRL_RELAY_OPEN => handle_open(manager, carrier, peer, &control.payload),
        envelope::CTRL_RELAY_CHUNK => handle_chunk(manager, carrier, &control.payload),
        envelope::CTRL_RELAY_CLOSE => {
            let Ok(close) = wire::RelayClose::decode(hew_codec::Format::Cbor, &control.payload)
            else {
                return;
            };
            let key = CircuitKey {
                carrier,
                id: close.circuit,
            };
            let circuit = manager_ref
                .relay
                .state
                .lock_or_recover()
                .circuits
                .get(&key)
                .cloned();
            if let Some(circuit) = circuit {
                close_circuit(manager_ref, &circuit, Some(carrier));
            }
        }
        _ => {}
    }
}

fn handle_open(
    manager: *mut HewConnMgr,
    carrier: Carrier,
    peer: NodeSessionIdentity,
    payload: &[u8],
) {
    // SAFETY: the authenticated caller reader pins this manager.
    let manager_ref = unsafe { &*manager };
    let conn = carrier.conn;
    let Ok(open) = wire::RelayOpen::decode(hew_codec::Format::Cbor, payload) else {
        return;
    };
    let (Ok(origin), Ok(target)) = (
        envelope::checked_session(&open.origin, 2),
        envelope::checked_session(&open.target, 3),
    ) else {
        return;
    };
    let Some(local) = local_session(manager_ref) else {
        return;
    };
    let key = CircuitKey {
        carrier,
        id: open.circuit,
    };
    if !manager_ref.relay.accept_open(key) {
        set_last_error("relay rejected a replayed circuit open");
        return;
    }
    if target == local {
        if origin.node_id.to_bytes() >= local.node_id.to_bytes()
            || manager_ref.auth.transport() != TransportSelection::Tcp
        {
            send_close(manager_ref, key);
            return;
        }
        let Some(circuit) = manager_ref.relay.install(key, None) else {
            send_close(manager_ref, key);
            return;
        };
        let work_circuit = Arc::clone(&circuit);
        let send = SendConnMgr(manager);
        if !track_worker(manager_ref, move || {
            admitted_worker(&send, work_circuit, origin);
        }) {
            close_circuit(manager_ref, &circuit, None);
        }
    } else {
        if origin != peer {
            set_last_error("relay open origin differs from its authenticated sender");
            send_close(manager_ref, key);
            return;
        }
        let Some(destination) =
            super::send::claim_send_by_identity(manager_ref, target.node_id, None)
        else {
            send_close(manager_ref, key);
            return;
        };
        if destination.peer_session() != target || destination.connection_id() == conn {
            send_close(manager_ref, key);
            return;
        }
        let _opens = manager_ref.relay.opens.lock_or_recover();
        let Some(id) = next_publication_token(manager_ref) else {
            send_close(manager_ref, key);
            return;
        };
        let right = CircuitKey {
            carrier: Carrier {
                conn: destination.connection_id(),
                publication: destination.publication_token,
            },
            id,
        };
        let Some(circuit) = manager_ref.relay.install(key, Some(right)) else {
            send_close(manager_ref, key);
            return;
        };
        let payload = wire::RelayOpen {
            circuit: id,
            origin: open.origin,
            target: open.target,
        }
        .encode(hew_codec::Format::Cbor);
        if !with_connection_attempt_deadline(|| {
            send_control(manager_ref, right, envelope::CTRL_RELAY_OPEN, payload)
        }) {
            close_circuit(manager_ref, &circuit, None);
        }
    }
}

fn handle_chunk(manager: *mut HewConnMgr, carrier: Carrier, payload: &[u8]) {
    // SAFETY: the authenticated caller reader pins this manager.
    let manager_ref = unsafe { &*manager };
    let Ok(chunk) = wire::RelayChunk::decode(hew_codec::Format::Cbor, payload) else {
        return;
    };
    if chunk.body.is_empty() || chunk.body.len() > CHUNK_BYTES {
        return;
    }
    let key = CircuitKey {
        carrier,
        id: chunk.circuit,
    };
    let circuit = manager_ref
        .relay
        .state
        .lock_or_recover()
        .circuits
        .get(&key)
        .cloned();
    let Some(circuit) = circuit else {
        return;
    };
    if let Some(endpoint) = &circuit.endpoint {
        if !endpoint.push(chunk.offset, chunk.body) {
            close_circuit(manager_ref, &circuit, None);
        }
    } else {
        let destination = if key == circuit.left {
            circuit.right
        } else {
            Some(circuit.left)
        };
        if let Some(destination) = destination {
            let payload = wire::RelayChunk {
                circuit: destination.id,
                ..chunk
            }
            .encode(hew_codec::Format::Cbor);
            if !with_connection_attempt_deadline(|| {
                send_control(
                    manager_ref,
                    destination,
                    envelope::CTRL_RELAY_CHUNK,
                    payload,
                )
            }) {
                close_circuit(manager_ref, &circuit, None);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(publication: u64, id: u64) -> CircuitKey {
        CircuitKey {
            carrier: Carrier {
                conn: 7,
                publication,
            },
            id,
        }
    }

    #[test]
    fn circuit_open_replay_is_bound_to_the_carrier_publication() {
        let hub = RelayHub::default();
        assert!(!hub.accept_open(key(1, 0)));
        assert!(hub.accept_open(key(1, 3)));
        assert!(!hub.accept_open(key(1, 3)));
        assert!(!hub.accept_open(key(1, 2)));
        assert!(hub.accept_open(key(1, 4)));
        assert!(hub.accept_open(key(2, 1)));
        assert!(!hub.accept_open(key(1, 3)));
    }

    #[test]
    fn retained_endpoint_prevents_slot_reuse_and_stale_key_adoption() {
        let hub = RelayHub::default();
        let first = hub.install(key(1, 1), None).unwrap();
        let mut circuits = vec![Arc::clone(&first)];
        for id in 2..=MAX_CIRCUITS {
            circuits.push(hub.install(key(1, id as u64), None).unwrap());
        }
        assert!(hub.install(key(1, 100), None).is_none());
        hub.state.lock_or_recover().circuits.remove(&first.left);
        assert!(hub.install(key(2, 1), None).is_none());
        circuits.remove(0);
        let previous_slot = first.conn_id;
        drop(first);
        let successor = hub.install(key(2, 1), None).unwrap();
        assert_eq!(successor.conn_id, previous_slot);
        assert!(!hub
            .state
            .lock_or_recover()
            .circuits
            .contains_key(&key(1, 1)));
        assert!(Arc::ptr_eq(
            &hub.state.lock_or_recover().circuits[&key(2, 1)],
            &successor,
        ));
    }
}
