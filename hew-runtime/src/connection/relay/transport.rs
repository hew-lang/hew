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
}

pub(super) fn connection(
    manager: *mut crate::connection::HewConnMgr,
    circuit: Arc<Circuit>,
) -> ConnectionTransport {
    let driver = Box::new(Driver {
        manager: SendConnMgr(manager),
        circuit,
    });
    ConnectionTransport::owned(HewTransport {
        ops: &raw const OPS,
        r#impl: Box::into_raw(driver).cast(),
    })
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
    let Some(endpoint) = &driver.circuit.endpoint else {
        return -1;
    };
    // SAFETY: the transport contract supplies the advertised readable payload.
    let data = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), length) };
    // SAFETY: the connection/worker lease pins the manager until this driver drops.
    let manager = unsafe { &*driver.manager.0 };
    let mut offset = endpoint.send_offset.lock_or_recover();
    let header = u32::try_from(length)
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
    c_int::try_from(length).unwrap_or(-1)
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
    if let Some(length) = endpoint.recv(output) {
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
}
