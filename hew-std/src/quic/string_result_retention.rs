//! Measured result-retention proofs for handle-scoped QUIC strings.
//!
//! One real loopback lifecycle supplies every positive producer state:
//! endpoint and connection addresses come from established Quinn handles,
//! endpoint error comes from an invalid connect attempt, stream error comes
//! from an invalid application error code, and connection error comes from
//! opening a stream after the peer disconnects.
//!
//! Every admitted result proves:
//!
//! - R1: two simultaneously-live results occupy distinct allocations.
//! - R2: releasing one result leaves its sibling readable, so each is an
//!   independent owner.
//! - R3: releasing both results leaves the live source able to produce a
//!   third.
//!
//! Successful endpoint connect and stream send also prove their prior error
//! state clears without invalidating the handle. Null handles and cleared
//! slots produce the canonical empty string, audited separately and never
//! used as positive admission evidence.

use std::sync::mpsc;
use std::time::Duration;

use hew_cabi::string::string_release;
use hew_runtime::bytes::hew_bytes_drop;

use crate::test_string::ManagedString;

use super::*;

fn assert_transferred(
    symbol: &str,
    mut call: impl FnMut() -> *mut HewString,
    validate: impl Fn(&str),
) {
    let first = call();
    let second = call();
    assert!(
        !first.is_null() && !second.is_null(),
        "{symbol}: expected two live results"
    );
    assert_ne!(
        first, second,
        "{symbol}: R1 failed: two live results share an allocation"
    );

    // SAFETY: both results are live owners held by this test.
    unsafe {
        validate(string_as_str(first));
        validate(string_as_str(second));
        string_release(first);
        // R2: releasing one owner must leave its sibling intact.
        validate(string_as_str(second));
        string_release(second);
    }

    let third = call();
    assert!(
        !third.is_null(),
        "{symbol}: R3 failed after caller releases"
    );
    // SAFETY: `third` is a fresh live owner.
    unsafe {
        validate(string_as_str(third));
        string_release(third);
    }
}

fn assert_canonical_empty(symbol: &str, value: *mut HewString) {
    assert!(
        value.is_null(),
        "{symbol}: an empty result is the canonical empty string"
    );
}

unsafe fn take_event_kind(event: *mut HewQuicEvent) -> i32 {
    assert!(!event.is_null(), "QUIC event must be present");
    // SAFETY: `event` is a live event returned by this module.
    let kind = unsafe { hew_quic_event_kind(event) };
    // SAFETY: this caller owns the event and releases it exactly once.
    unsafe { hew_quic_event_free(event) };
    kind
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "one ordered QUIC lifecycle is the ownership proof; splitting it would obscure handle validity and cleanup"
)]
fn loopback_handle_string_results_are_transferred_and_sources_remain_usable() {
    let bind = ManagedString::new("127.0.0.1:0");
    // SAFETY: `bind` owns a live managed address.
    let server_endpoint = unsafe { hew_quic_new_server(bind.as_ptr()) };
    let client_endpoint = hew_quic_new_client();
    assert!(
        !server_endpoint.is_null() && !client_endpoint.is_null(),
        "QUIC loopback endpoints must be constructed"
    );

    assert_transferred(
        "hew_quic_endpoint_local_addr",
        // SAFETY: the client endpoint remains live through all calls.
        || unsafe { hew_quic_endpoint_local_addr(client_endpoint) },
        |text| {
            let (_, port) = text
                .rsplit_once(':')
                .expect("endpoint address includes a port");
            assert_ne!(port, "0", "ephemeral endpoint must report its bound port");
        },
    );

    let bad_address = ManagedString::new("not-an-address");
    let server_name = ManagedString::new("localhost");
    // SAFETY: the endpoint and both managed strings remain live through the call.
    let failed = unsafe {
        hew_quic_endpoint_connect(client_endpoint, bad_address.as_ptr(), server_name.as_ptr())
    };
    assert!(
        failed.is_null(),
        "invalid address must induce endpoint error"
    );
    assert_transferred(
        "hew_quic_endpoint_last_error",
        // SAFETY: the failed connect leaves the endpoint live.
        || unsafe { hew_quic_endpoint_last_error(client_endpoint) },
        |text| {
            assert!(text.contains("could not resolve connect address"));
            assert!(text.contains("parse failure"));
        },
    );

    // SAFETY: the server endpoint is live and has a bound local address.
    let server_port = unsafe { &*server_endpoint }
        .endpoint
        .local_addr()
        .expect("server endpoint has local address")
        .port();
    let server_endpoint_addr = server_endpoint as usize;
    let (accepted_tx, accepted_rx) = mpsc::sync_channel(1);
    let accept_thread = std::thread::spawn(move || {
        let endpoint = server_endpoint_addr as *mut HewQuicEndpoint;
        // SAFETY: the parent keeps the endpoint live until this thread joins.
        let connection = unsafe { hew_quic_endpoint_accept(endpoint) };
        assert!(!connection.is_null(), "server must accept client");
        // SAFETY: the connection is live; the client sends a probe before the
        // parent waits for this result, making the lazy QUIC stream observable.
        let stream = unsafe { hew_quic_conn_accept_stream(connection) };
        assert!(!stream.is_null(), "server must accept client stream");
        accepted_tx
            .send((connection as usize, stream as usize))
            .expect("send accepted QUIC handles");
    });

    let address = ManagedString::new(format!("127.0.0.1:{server_port}"));
    // SAFETY: endpoint/address/server-name remain live through the call.
    let client_connection = unsafe {
        hew_quic_endpoint_connect(client_endpoint, address.as_ptr(), server_name.as_ptr())
    };
    assert!(
        !client_connection.is_null(),
        "valid loopback connect must succeed"
    );
    // A successful connect clears the error used for the positive retention
    // proof while leaving the endpoint address available.
    // SAFETY: the endpoint remains live after connecting.
    assert_canonical_empty("hew_quic_endpoint_last_error(clear)", unsafe {
        hew_quic_endpoint_last_error(client_endpoint)
    });
    // SAFETY: observation getter borrows the live endpoint.
    assert!(unsafe { hew_quic_endpoint_accepted_connections(client_endpoint) } == 0);

    assert_transferred(
        "hew_quic_conn_local_addr",
        // SAFETY: the client connection remains live through all calls.
        || unsafe { hew_quic_conn_local_addr(client_connection) },
        |text| {
            let (_, port) = text
                .rsplit_once(':')
                .expect("connection local address includes a port");
            assert_ne!(port, "0", "connection must report its bound port");
        },
    );
    assert_transferred(
        "hew_quic_conn_peer_addr",
        // SAFETY: the client connection remains live through all calls.
        || unsafe { hew_quic_conn_peer_addr(client_connection) },
        |text| {
            assert!(text.ends_with(&server_port.to_string()));
        },
    );
    // SAFETY: a healthy live connection reports no error.
    assert_canonical_empty("hew_quic_conn_last_error(healthy)", unsafe {
        hew_quic_conn_last_error(client_connection)
    });

    // SAFETY: the established connection can open a bidirectional stream.
    let client_stream = unsafe { hew_quic_conn_open_stream(client_connection) };
    assert!(!client_stream.is_null(), "client stream must open");
    // SAFETY: `-7` is deliberately outside the application error-code range.
    assert_eq!(unsafe { hew_quic_stream_stop(client_stream, -7) }, -1);
    assert_transferred(
        "hew_quic_stream_last_error",
        // SAFETY: invalid stop does not consume the live stream.
        || unsafe { hew_quic_stream_last_error(client_stream) },
        |text| {
            assert_eq!(
                text,
                "invalid QUIC application error code -7: expected 0..=2^62-1"
            );
        },
    );

    let probe = bytes_triple_from_slice(b"retention-probe");
    // SAFETY: stream/probe are live and the call only borrows the triple.
    let send_status = unsafe { hew_quic_stream_send(client_stream, &raw const probe) };
    assert_eq!(
        send_status, 0,
        "stream must remain usable after caller releases error results"
    );
    // SAFETY: the test owns the runtime bytes allocation.
    unsafe { hew_bytes_drop(probe.ptr) };
    // A successful send clears the invalid-stop error.
    // SAFETY: the stream remains live after the successful send.
    assert_canonical_empty("hew_quic_stream_last_error(clear)", unsafe {
        hew_quic_stream_last_error(client_stream)
    });

    let (server_connection_addr, server_stream_addr) = accepted_rx
        .recv_timeout(Duration::from_secs(5))
        .expect("server must accept loopback stream");
    accept_thread.join().expect("accept thread must finish");
    let server_connection = server_connection_addr as *mut HewQuicConn;
    let server_stream = server_stream_addr as *mut HewQuicStream;

    // Drain the stream-opened event before later waiting for disconnect.
    assert_eq!(
        // SAFETY: server connection/event are live.
        unsafe { take_event_kind(hew_quic_conn_on_event(server_connection)) },
        EVENT_STREAM_OPENED
    );
    // SAFETY: the client sent one non-empty probe.
    let received = unsafe { hew_quic_stream_recv(server_stream) };
    assert_eq!(received.len as usize, b"retention-probe".len());
    // SAFETY: non-empty receive results are caller-owned runtime bytes.
    unsafe { hew_bytes_drop(received.ptr) };

    // SAFETY: finishing the client send side makes server receive observe EOF.
    assert_eq!(unsafe { hew_quic_stream_finish(client_stream) }, 0);
    // SAFETY: server stream remains live until EOF.
    let eof = unsafe { hew_quic_stream_recv(server_stream) };
    assert_eq!(eof.len, 0);
    // SAFETY: both stream handles are released exactly once.
    assert_eq!(unsafe { hew_quic_stream_close(client_stream) }, 0);
    // SAFETY: as above.
    assert_eq!(unsafe { hew_quic_stream_close(server_stream) }, 0);
    assert_eq!(
        // SAFETY: server stream close queued one event.
        unsafe { take_event_kind(hew_quic_conn_on_event(server_connection)) },
        EVENT_STREAM_CLOSED
    );

    // SAFETY: the client connection is no longer used after this transfer.
    assert_eq!(unsafe { hew_quic_conn_disconnect(client_connection) }, 0);
    assert_eq!(
        // SAFETY: peer disconnect queues an event on the live server handle.
        unsafe { take_event_kind(hew_quic_conn_on_event(server_connection)) },
        EVENT_DISCONNECTED
    );

    // Opening after peer disconnect deterministically populates the
    // connection-scoped error while preserving the observation source.
    // SAFETY: the server handle remains live until explicit disconnect.
    let failed_stream = unsafe { hew_quic_conn_open_stream(server_connection) };
    assert!(
        failed_stream.is_null(),
        "closed connection must reject a new stream"
    );
    assert_transferred(
        "hew_quic_conn_last_error",
        // SAFETY: failed open leaves the connection observation live.
        || unsafe { hew_quic_conn_last_error(server_connection) },
        |text| {
            assert!(
                !text.is_empty(),
                "closed connection must retain a diagnostic"
            );
        },
    );
    // SAFETY: caller releases did not consume connection telemetry.
    let peer_after_release = unsafe { hew_quic_conn_peer_addr(server_connection) };
    assert!(!peer_after_release.is_null());
    // SAFETY: this telemetry result is caller-owned and released exactly once.
    unsafe { string_release(peer_after_release) };

    // SAFETY: each remaining transport handle is released exactly once.
    assert_eq!(unsafe { hew_quic_conn_disconnect(server_connection) }, 0);
    // SAFETY: client endpoint owns no live connection after disconnect.
    unsafe { hew_quic_endpoint_close(client_endpoint) };
    // SAFETY: server endpoint owns no live connection after disconnect.
    unsafe { hew_quic_endpoint_close(server_endpoint) };
}

#[test]
fn null_handle_paths_return_the_canonical_empty_string() {
    for (symbol, value) in [
        (
            "hew_quic_endpoint_local_addr(null)",
            // SAFETY: null is explicitly accepted.
            unsafe { hew_quic_endpoint_local_addr(std::ptr::null()) },
        ),
        (
            "hew_quic_endpoint_last_error(null)",
            // SAFETY: null is explicitly accepted.
            unsafe { hew_quic_endpoint_last_error(std::ptr::null()) },
        ),
        (
            "hew_quic_conn_local_addr(null)",
            // SAFETY: null is explicitly accepted.
            unsafe { hew_quic_conn_local_addr(std::ptr::null()) },
        ),
        (
            "hew_quic_conn_peer_addr(null)",
            // SAFETY: null is explicitly accepted.
            unsafe { hew_quic_conn_peer_addr(std::ptr::null()) },
        ),
        (
            "hew_quic_conn_last_error(null)",
            // SAFETY: null is explicitly accepted.
            unsafe { hew_quic_conn_last_error(std::ptr::null()) },
        ),
        (
            "hew_quic_stream_last_error(null)",
            // SAFETY: null is explicitly accepted.
            unsafe { hew_quic_stream_last_error(std::ptr::null()) },
        ),
    ] {
        assert_canonical_empty(symbol, value);
    }
}
