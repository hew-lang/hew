//! Framed byte queues owned by one relay circuit and its connection leases.

use std::collections::VecDeque;
use std::ffi::{c_int, c_void};
use std::sync::{Arc, Condvar, Mutex, MutexGuard};

use crate::node_protocol_generated as wire;
use crate::transport::{
    connection_attempt_remaining, HewTransport, HewTransportOps, MAX_FRAME_SIZE,
};
use crate::util::MutexExt;

use super::{close_circuit, send_control, Circuit, SendConnMgr, CHUNK_BYTES};
use crate::connection::ConnectionTransport;

#[cfg(feature = "quic")]
#[path = "tls.rs"]
mod tls;

#[derive(Debug, Default)]
struct Incoming {
    chunks: VecDeque<Vec<u8>>,
    front_offset: usize,
    available: usize,
    wire_offset: u64,
    pending_length: Option<usize>,
    closed: bool,
}

#[derive(Debug, Default)]
pub(super) struct Endpoint {
    incoming: Mutex<Incoming>,
    changed: Condvar,
    send_offset: Mutex<u64>,
}

impl Endpoint {
    pub(super) fn push(&self, offset: u64, body: Vec<u8>) -> bool {
        let mut incoming = self.incoming.lock_or_recover();
        let Some(end) = offset.checked_add(u64::try_from(body.len()).unwrap_or(u64::MAX)) else {
            return false;
        };
        if incoming.closed
            || incoming.wire_offset != offset
            || incoming.available.saturating_add(body.len()) > MAX_FRAME_SIZE + 4
        {
            return false;
        }
        incoming.wire_offset = end;
        incoming.available += body.len();
        incoming.chunks.push_back(body);
        self.changed.notify_all();
        true
    }

    pub(super) fn close(&self) {
        let mut incoming = self.incoming.lock_or_recover();
        incoming.closed = true;
        incoming.chunks.clear();
        incoming.available = 0;
        self.changed.notify_all();
    }

    fn read_exact<'a>(
        &'a self,
        mut incoming: MutexGuard<'a, Incoming>,
        output: &mut [u8],
    ) -> Option<MutexGuard<'a, Incoming>> {
        let mut written = 0;
        while written < output.len() {
            if incoming.closed {
                return None;
            }
            if let Some(chunk) = incoming.chunks.pop_front() {
                let start = incoming.front_offset;
                let count = (chunk.len() - start).min(output.len() - written);
                output[written..written + count].copy_from_slice(&chunk[start..start + count]);
                incoming.available -= count;
                written += count;
                if start + count == chunk.len() {
                    incoming.front_offset = 0;
                } else {
                    incoming.front_offset += count;
                    incoming.chunks.push_front(chunk);
                }
                continue;
            }
            incoming = if let Some(remaining) = connection_attempt_remaining() {
                if remaining.is_zero() {
                    return None;
                }
                let (guard, timeout) = self
                    .changed
                    .wait_timeout(incoming, remaining)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                if timeout.timed_out() {
                    return None;
                }
                guard
            } else {
                self.changed
                    .wait(incoming)
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
            };
        }
        Some(incoming)
    }

    fn recv(&self, output: &mut [u8]) -> Option<c_int> {
        let mut incoming = self.incoming.lock_or_recover();
        let length = if let Some(length) = incoming.pending_length {
            length
        } else {
            let mut header = [0; 4];
            incoming = self.read_exact(incoming, &mut header)?;
            usize::try_from(u32::from_le_bytes(header)).ok()?
        };
        if length > MAX_FRAME_SIZE {
            return None;
        }
        incoming.pending_length = Some(length);
        if length > output.len() {
            return c_int::try_from(length).ok();
        }
        incoming = self.read_exact(incoming, &mut output[..length])?;
        incoming.pending_length = None;
        c_int::try_from(length).ok()
    }
}

struct Driver {
    manager: SendConnMgr,
    circuit: Arc<Circuit>,
    #[cfg(feature = "quic")]
    tls: Option<Mutex<tls::Tls>>,
}

pub(super) fn connection(
    manager: *mut crate::connection::HewConnMgr,
    circuit: Arc<Circuit>,
    initiator: bool,
) -> Option<ConnectionTransport> {
    // SAFETY: the tracked admission worker pins the manager through this call.
    #[cfg(feature = "quic")]
    let manager_ref = unsafe { &*manager };
    #[cfg(feature = "quic")]
    let tls = match manager_ref.auth.transport() {
        crate::peer_binding::TransportSelection::Tcp => None,
        crate::peer_binding::TransportSelection::QuicMesh => {
            match tls::new(&manager_ref.auth, initiator) {
                Ok(tls) => Some(Mutex::new(tls)),
                Err(error) => {
                    crate::set_last_error(error);
                    close_circuit(manager_ref, &circuit, None);
                    return None;
                }
            }
        }
    };
    #[cfg(not(feature = "quic"))]
    let _ = initiator;
    let driver = Box::new(Driver {
        manager: SendConnMgr(manager),
        circuit,
        #[cfg(feature = "quic")]
        tls,
    });
    #[cfg(feature = "quic")]
    if driver.tls.is_some() && !tls::handshake(&driver) {
        close_circuit(manager_ref, &driver.circuit, None);
        return None;
    }
    #[cfg(feature = "quic")]
    let ops = if driver.tls.is_some() { &TLS_OPS } else { &OPS };
    #[cfg(not(feature = "quic"))]
    let ops = &OPS;
    Some(ConnectionTransport::owned(HewTransport {
        ops,
        r#impl: Box::into_raw(driver).cast(),
    }))
}

pub(crate) unsafe fn is_noise_transport(transport: *const HewTransport) -> bool {
    // SAFETY: the caller pins a live transport, or supplies null.
    unsafe { transport.as_ref() }
        .is_some_and(|transport| std::ptr::eq(transport.ops, &raw const OPS))
}

unsafe extern "C" fn send(
    opaque: *mut c_void,
    conn: c_int,
    data: *const c_void,
    length: usize,
) -> c_int {
    // SAFETY: the owning transport lease pins its driver throughout this call.
    let Some(driver) = (unsafe { opaque.cast::<Driver>().as_ref() }) else {
        return -1;
    };
    if data.is_null() || conn != driver.circuit.conn_id || length > MAX_FRAME_SIZE {
        return -1;
    }
    // SAFETY: the transport contract supplies the advertised readable payload.
    let data = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), length) };
    #[cfg(feature = "quic")]
    if driver.tls.is_some() {
        return if tls::send(driver, data) {
            c_int::try_from(length).unwrap_or(-1)
        } else {
            -1
        };
    }
    send_raw(driver, data)
}

fn send_raw(driver: &Driver, data: &[u8]) -> c_int {
    let Some(endpoint) = &driver.circuit.endpoint else {
        return -1;
    };
    // SAFETY: the connection/worker lease pins the manager until this driver drops.
    let manager = unsafe { &*driver.manager.0 };
    let mut offset = endpoint.send_offset.lock_or_recover();
    let header = u32::try_from(data.len())
        .expect("frame limit fits u32")
        .to_le_bytes();
    for chunk in std::iter::once(header.as_slice()).chain(data.chunks(CHUNK_BYTES)) {
        if driver
            .circuit
            .closed
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return -1;
        }
        let Some(next) = offset.checked_add(u64::try_from(chunk.len()).unwrap_or(u64::MAX)) else {
            close_circuit(manager, &driver.circuit, None);
            return -1;
        };
        let payload = wire::RelayChunk {
            circuit: driver.circuit.left.id,
            offset: *offset,
            body: chunk.to_vec(),
        }
        .encode(hew_codec::Format::Cbor);
        if !send_control(
            manager,
            driver.circuit.left,
            crate::envelope::CTRL_RELAY_CHUNK,
            payload,
        ) {
            close_circuit(manager, &driver.circuit, None);
            return -1;
        }
        *offset = next;
    }
    c_int::try_from(data.len()).unwrap_or(-1)
}

unsafe extern "C" fn recv(
    opaque: *mut c_void,
    conn: c_int,
    data: *mut c_void,
    capacity: usize,
) -> c_int {
    // SAFETY: the owning transport lease pins its driver throughout this call.
    let Some(driver) = (unsafe { opaque.cast::<Driver>().as_ref() }) else {
        return -1;
    };
    if data.is_null() || conn != driver.circuit.conn_id {
        return -1;
    }
    let Some(endpoint) = &driver.circuit.endpoint else {
        return -1;
    };
    // SAFETY: the transport contract supplies the advertised writable buffer.
    let output = unsafe { std::slice::from_raw_parts_mut(data.cast::<u8>(), capacity) };
    #[cfg(feature = "quic")]
    let result = if driver.tls.is_some() {
        tls::recv(driver, output)
    } else {
        endpoint.recv(output)
    };
    #[cfg(not(feature = "quic"))]
    let result = endpoint.recv(output);
    if let Some(length) = result {
        return length;
    }
    // SAFETY: the reader/worker owns its manager lease until this driver drops.
    close_circuit(unsafe { &*driver.manager.0 }, &driver.circuit, None);
    -1
}

unsafe extern "C" fn close(opaque: *mut c_void, conn: c_int) {
    // SAFETY: the owning transport lease pins its driver throughout this call.
    let Some(driver) = (unsafe { opaque.cast::<Driver>().as_ref() }) else {
        return;
    };
    if conn == driver.circuit.conn_id {
        // SAFETY: the connection/worker lease pins the manager.
        close_circuit(unsafe { &*driver.manager.0 }, &driver.circuit, None);
    }
}

unsafe extern "C" fn destroy(opaque: *mut c_void) {
    if opaque.is_null() {
        return;
    }
    // SAFETY: the last transport owner transfers this one driver allocation.
    let driver = unsafe { Box::from_raw(opaque.cast::<Driver>()) };
    // SAFETY: transport owners drop before their reader/send/worker manager leases.
    close_circuit(unsafe { &*driver.manager.0 }, &driver.circuit, None);
}

static OPS: HewTransportOps = HewTransportOps {
    connect: None,
    listen: None,
    accept: None,
    send: Some(send),
    recv: Some(recv),
    close_conn: Some(close),
    destroy: Some(destroy),
};

#[cfg(feature = "quic")]
static TLS_OPS: HewTransportOps = HewTransportOps {
    connect: None,
    listen: None,
    accept: None,
    send: Some(send),
    recv: Some(recv),
    close_conn: Some(close),
    destroy: Some(destroy),
};

#[cfg(feature = "quic")]
pub(crate) unsafe fn is_tls_transport(transport: *const HewTransport) -> bool {
    // SAFETY: the caller pins a live transport, or supplies null.
    unsafe { transport.as_ref() }
        .is_some_and(|transport| std::ptr::eq(transport.ops, &raw const TLS_OPS))
}

#[cfg(feature = "quic")]
pub(crate) unsafe fn peer_spki(transport: *const HewTransport, conn: c_int) -> Option<Vec<u8>> {
    // SAFETY: the caller pins a live transport for this credential projection.
    if !unsafe { is_tls_transport(transport) } {
        return None;
    }
    // SAFETY: the exact ops identity establishes this private driver type.
    let driver = unsafe { &*(*transport).r#impl.cast::<Driver>() };
    if conn != driver.circuit.conn_id {
        return None;
    }
    tls::peer_spki(driver)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn queued_frames_preserve_capacity_retry_boundaries_and_sequence() {
        let endpoint = Endpoint::default();
        let mut bytes = 5_u32.to_le_bytes().to_vec();
        bytes.extend_from_slice(b"first");
        bytes.extend_from_slice(&6_u32.to_le_bytes());
        bytes.extend_from_slice(b"second");
        assert!(endpoint.push(0, bytes));
        let mut small = [0xaa; 2];
        assert_eq!(endpoint.recv(&mut small), Some(5));
        assert_eq!(small, [0xaa; 2]);
        assert_eq!(endpoint.recv(&mut small), Some(5));
        let mut output = [0; 16];
        assert_eq!(endpoint.recv(&mut output), Some(5));
        assert_eq!(&output[..5], b"first");
        assert_eq!(endpoint.recv(&mut output), Some(6));
        assert_eq!(&output[..6], b"second");
        assert!(!endpoint.push(0, vec![1]));
        endpoint.close();
        assert_eq!(endpoint.recv(&mut output), None);
        assert!(!endpoint.push(19, vec![1]));
    }

    #[test]
    fn hostile_frame_length_and_queue_expansion_are_bounded() {
        let endpoint = Endpoint::default();
        assert!(!endpoint.push(0, vec![0; MAX_FRAME_SIZE + 5]));
        assert!(endpoint.push(0, u32::MAX.to_le_bytes().to_vec()));
        assert_eq!(endpoint.recv(&mut [0; 16]), None);
    }
    #[test]
    fn carrier_retirement_wakes_its_reader_and_cannot_close_a_replacement() {
        use crate::connection::relay::{Carrier, CircuitKey};
        let mut transport = HewTransport {
            ops: std::ptr::null(),
            r#impl: std::ptr::null_mut(),
        };
        // SAFETY: this test pins transport and manager until both readers join.
        unsafe {
            let manager = crate::connection::hew_connmgr_new(
                &raw mut transport,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                1,
            );
            let owner = &*manager;
            let carrier = Carrier {
                conn: 7,
                publication: 1,
            };
            let circuit = owner
                .relay
                .install(CircuitKey { carrier, id: 1 }, None, &[])
                .unwrap();
            let reading = Arc::clone(&circuit);
            let started = Arc::new(std::sync::Barrier::new(2));
            let reader_started = Arc::clone(&started);
            let worker = std::thread::spawn(move || {
                reader_started.wait();
                reading.endpoint.as_ref().unwrap().recv(&mut [0; 16])
            });
            started.wait();
            super::super::carrier_lost(owner, 7, 1);
            assert_eq!(worker.join().unwrap(), None);
            assert!(circuit.closed.load(std::sync::atomic::Ordering::Acquire));
            let successor = owner
                .relay
                .install(
                    CircuitKey {
                        carrier: Carrier {
                            conn: 7,
                            publication: 2,
                        },
                        id: 1,
                    },
                    None,
                    &[],
                )
                .unwrap();
            super::super::carrier_lost(owner, 7, 1);
            let endpoint = successor.endpoint.as_ref().unwrap();
            let mut bytes = 4_u32.to_le_bytes().to_vec();
            bytes.extend_from_slice(b"live");
            assert!(endpoint.push(0, bytes));
            let mut output = [0; 4];
            assert_eq!(endpoint.recv(&mut output), Some(4));
            assert_eq!(&output, b"live");
            crate::connection::hew_connmgr_free(manager);
            assert_eq!(endpoint.recv(&mut output), None);
        }
    }
}
