//! TLS record carriage using the direct mesh verifier and identity configuration.

use std::io::{Cursor, Read, Write};

use rustls::pki_types::ServerName;

use super::{send_raw, Driver, CHUNK_BYTES, MAX_FRAME_SIZE};
use crate::peer_binding::PeerAuthSnapshot;
use crate::util::MutexExt;

pub(super) struct Tls {
    connection: rustls::Connection,
    plaintext: Vec<u8>,
}

pub(super) fn new(auth: &PeerAuthSnapshot, initiator: bool) -> Result<Tls, String> {
    let material = auth
        .mesh_identity()
        .ok_or("relay TLS identity is unavailable")?;
    let (identity, _) = crate::quic_mesh::mesh_tls_from_material(material);
    let (server, client) = crate::quic_mesh::checked_tls_configs(&identity, true)
        .map_err(|error| error.to_string())?;
    let mut connection = if initiator {
        rustls::Connection::Client(
            rustls::ClientConnection::new(
                std::sync::Arc::new(client),
                ServerName::try_from("hew-relay").expect("fixed TLS server name"),
            )
            .map_err(|error| error.to_string())?,
        )
    } else {
        rustls::Connection::Server(
            rustls::ServerConnection::new(std::sync::Arc::new(server))
                .map_err(|error| error.to_string())?,
        )
    };
    connection.set_buffer_limit(Some(CHUNK_BYTES + 4));
    Ok(Tls {
        connection,
        plaintext: Vec::new(),
    })
}

/// Drain records while holding the TLS engine lock, preserving their stream
/// order against reader-generated handshake responses and ordinary sends.
/// Raw receive waits never hold this lock.
fn flush(driver: &Driver, tls: &mut Tls) -> bool {
    while tls.connection.wants_write() {
        let mut records = Vec::new();
        if tls.connection.write_tls(&mut records).is_err() || records.is_empty() {
            return false;
        }
        for bytes in records.chunks(CHUNK_BYTES) {
            if send_raw(driver, bytes) < 0 {
                return false;
            }
        }
    }
    true
}

fn read_packet(driver: &Driver) -> Option<Vec<u8>> {
    let endpoint = driver.circuit.endpoint.as_ref()?;
    let mut bytes = vec![0; CHUNK_BYTES];
    let count = usize::try_from(endpoint.recv(&mut bytes)?).ok()?;
    if count == 0 || count > bytes.len() {
        return None;
    }
    bytes.truncate(count);
    Some(bytes)
}

fn ingest(driver: &Driver, tls: &mut Tls, bytes: &[u8]) -> bool {
    let mut input = Cursor::new(bytes);
    while usize::try_from(input.position()).unwrap_or(usize::MAX) < bytes.len() {
        if !matches!(tls.connection.read_tls(&mut input), Ok(count) if count > 0) {
            return false;
        }
        let Ok(state) = tls.connection.process_new_packets() else {
            return false;
        };
        let count = state.plaintext_bytes_to_read();
        if tls.plaintext.len().saturating_add(count) > MAX_FRAME_SIZE + 4 {
            return false;
        }
        let start = tls.plaintext.len();
        tls.plaintext.resize(start + count, 0);
        if tls
            .connection
            .reader()
            .read_exact(&mut tls.plaintext[start..])
            .is_err()
        {
            return false;
        }
        if state.peer_has_closed() {
            return false;
        }
    }
    flush(driver, tls)
}

pub(super) fn handshake(driver: &Driver) -> bool {
    let Some(engine) = &driver.tls else {
        return false;
    };
    loop {
        {
            let mut tls = engine.lock_or_recover();
            if !flush(driver, &mut tls) {
                return false;
            }
            if !tls.connection.is_handshaking() {
                return tls
                    .connection
                    .peer_certificates()
                    .is_some_and(|chain| !chain.is_empty());
            }
        }
        let Some(packet) = read_packet(driver) else {
            return false;
        };
        if !ingest(driver, &mut engine.lock_or_recover(), &packet) {
            return false;
        }
    }
}

pub(super) fn send(driver: &Driver, payload: &[u8]) -> bool {
    let Some(engine) = &driver.tls else {
        return false;
    };
    let header = u32::try_from(payload.len())
        .expect("checked frame size")
        .to_le_bytes();
    for piece in std::iter::once(header.as_slice()).chain(payload.chunks(CHUNK_BYTES)) {
        // The channel send gate owns the whole plaintext frame. Release the
        // engine between record batches so inbound TLS can progress concurrently.
        let mut tls = engine.lock_or_recover();
        if tls.connection.writer().write_all(piece).is_err() || !flush(driver, &mut tls) {
            return false;
        }
    }
    true
}

pub(super) fn recv(driver: &Driver, output: &mut [u8]) -> Option<std::ffi::c_int> {
    let engine = driver.tls.as_ref()?;
    loop {
        {
            let mut tls = engine.lock_or_recover();
            if tls.plaintext.len() >= 4 {
                let length =
                    usize::try_from(u32::from_le_bytes(tls.plaintext[..4].try_into().ok()?))
                        .ok()?;
                if length > MAX_FRAME_SIZE {
                    return None;
                }
                if length > output.len() {
                    return std::ffi::c_int::try_from(length).ok();
                }
                if tls.plaintext.len() >= length + 4 {
                    output[..length].copy_from_slice(&tls.plaintext[4..length + 4]);
                    tls.plaintext.drain(..length + 4);
                    return std::ffi::c_int::try_from(length).ok();
                }
            }
        }
        let packet = read_packet(driver)?;
        if !ingest(driver, &mut engine.lock_or_recover(), &packet) {
            return None;
        }
    }
}

pub(super) fn peer_spki(driver: &Driver) -> Option<Vec<u8>> {
    let tls = driver.tls.as_ref()?.lock_or_recover();
    let certificate = tls.connection.peer_certificates()?.first()?;
    crate::quic_mesh::extract_spki_from_cert_der(certificate.as_ref()).ok()
}
