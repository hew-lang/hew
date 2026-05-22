//! Smoke tests for the `quic_mesh` module skeleton.
//!
//! Gate requirements:
//! - Two `QuicMesh`es can listen + connect to each other (mTLS, SPKI-pinned).
//! - At least 2 concurrent streams per connection are independently
//!   flow-controlled (HOL-blocking counter-example).
//! - Datagrams send and receive (typed callback surface).

#[cfg(all(feature = "quic", not(target_arch = "wasm32")))]
mod tests {
    use hew_runtime::quic_mesh::{LaneKind, MeshTls, QuicMesh};

    /// Build a pair of [`MeshTls`] configs where each side pins the other's SPKI.
    ///
    /// Returns `(tls_a, tls_b)` where `tls_a` allows B's cert and vice-versa.
    fn make_tls_pair() -> (MeshTls, MeshTls) {
        let (tls_a, spki_a) =
            MeshTls::self_signed(vec!["node-a".into()]).expect("tls_a self_signed");
        let (tls_b, spki_b) =
            MeshTls::self_signed(vec!["node-b".into()]).expect("tls_b self_signed");

        let tls_a = tls_a.with_peer_spki(spki_b);
        let tls_b = tls_b.with_peer_spki(spki_a);

        (tls_a, tls_b)
    }

    /// Two meshes can listen and connect to each other with mTLS.
    #[tokio::test]
    async fn listen_and_connect() {
        let (tls_a, tls_b) = make_tls_pair();

        let mesh_a =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_a).expect("mesh_a listen");
        let addr_a = mesh_a.local_addr().expect("mesh_a local_addr");
        assert_ne!(addr_a.port(), 0);

        let mesh_b =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_b).expect("mesh_b listen");

        // B connects to A; A accepts B.
        let (conn_b, conn_a_result) = tokio::join!(mesh_b.connect(addr_a), mesh_a.accept());

        let conn_b = conn_b.expect("b→a connect");
        let conn_a = conn_a_result
            .expect("accept returned None")
            .expect("accept handshake");

        // Sanity: remote addresses are non-loopback-zero.
        assert_ne!(conn_b.remote_address(), "0.0.0.0:0".parse().unwrap());
        assert_ne!(conn_a.remote_address(), "0.0.0.0:0".parse().unwrap());
    }

    /// mTLS rejects a connection when the peer SPKI is not in the allowlist.
    ///
    /// Node C has a cert that neither A nor B has pinned. When C tries to
    /// connect to A, the handshake must fail.
    #[tokio::test]
    async fn mtls_rejects_unknown_peer() {
        let (tls_a, _tls_b) = make_tls_pair();
        let (tls_c, _spki_c) = MeshTls::self_signed(vec!["node-c-unknown".into()]).expect("tls_c");
        // tls_c's allowlist is empty and tls_a does not pin C's SPKI.

        let mesh_a =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_a).expect("mesh_a listen");
        let addr_a = mesh_a.local_addr().expect("addr_a");

        // Build a mesh_c that trusts no-one (empty allowlist). The connect
        // should fail because A's SPKI is not in C's allowlist, OR A rejects
        // C because C's SPKI is not in A's allowlist. Either side failing the
        // handshake is a pass.
        let mesh_c =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_c).expect("mesh_c listen");

        let result = mesh_c.connect(addr_a).await;
        assert!(
            result.is_err(),
            "connection from unknown peer must be rejected (fail-closed mTLS)"
        );
    }

    /// Two concurrent streams on the same connection are independently
    /// flow-controlled. This is the HOL-blocking counter-example:
    ///
    /// - Stream 1: large payload (flow-control pressure)
    /// - Stream 2: small marker
    ///
    /// Stream 2 must complete even while stream 1 is in flight. If HOL
    /// blocking were present (single shared stream), stream 2 would stall
    /// behind stream 1.
    #[tokio::test]
    async fn two_concurrent_streams_no_hol_blocking() {
        let (tls_a, tls_b) = make_tls_pair();

        let mesh_a =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_a).expect("mesh_a listen");
        let addr_a = mesh_a.local_addr().expect("addr_a");

        let mesh_b =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_b).expect("mesh_b listen");

        let (b_connect, a_accept) = tokio::join!(mesh_b.connect(addr_a), mesh_a.accept());
        let conn_b = b_connect.expect("connect");
        let conn_a = a_accept.expect("accept None").expect("accept err");

        let lane1 = LaneKind(1);
        let lane2 = LaneKind(2);

        // B opens two streams concurrently.
        let (stream1_res, stream2_res) = tokio::join!(
            conn_b.open_stream_with_lane_header(lane1),
            conn_b.open_stream_with_lane_header(lane2),
        );
        let (id1, mut send1, _recv1) = stream1_res.expect("open stream 1");
        let (id2, mut send2, _recv2) = stream2_res.expect("open stream 2");

        // Stream IDs must be distinct.
        assert_ne!(id1, id2, "concurrent streams must have distinct IDs");

        // Write markers on both streams before accepting either on A's side.
        // Stream 1: 64 KiB (flow-control pressure)
        let big_payload = vec![0xABu8; 64 * 1024];
        let marker2 = b"HOL-free-marker";

        // Send both concurrently — A does not drain stream 1 until after we
        // check stream 2. A single HOL-blocked stream would stall send2 here.
        let (send1_res, send2_res) =
            tokio::join!(send1.write_all(&big_payload), send2.write_all(marker2),);
        send1_res.expect("stream 1 write");
        send2_res.expect("stream 2 write");

        // A accepts stream 2 first (the small one). In the legacy single-stream
        // model this would be impossible because stream 1's bulk data would
        // need to be drained first.
        let (a_id1, a_lane1, _a_send1, a_recv1) =
            conn_a.accept_stream().await.expect("a accept stream 1");
        let (a_id2, a_lane2, _a_send2, a_recv2) =
            conn_a.accept_stream().await.expect("a accept stream 2");

        // One of the accepted streams must be lane2 (the small marker).
        // Determine which is which by lane.
        let (mut small_recv, mut big_recv) = if a_lane1 == lane2 {
            assert_eq!(a_lane2, lane1);
            (a_recv1, a_recv2)
        } else {
            assert_eq!(a_lane1, lane1, "unexpected lane");
            assert_eq!(a_lane2, lane2, "unexpected lane");
            (a_recv2, a_recv1)
        };

        // IDs are assigned by each side's counter independently; just verify
        // they are valid (non-equal to each other).
        assert_ne!(a_id1, a_id2);

        // Read the small marker from the lane2 stream. This must succeed
        // independently of the large stream 1 payload — proof of no HOL
        // blocking at the application level.
        let mut buf2 = vec![0u8; marker2.len()];
        small_recv
            .read_exact(&mut buf2)
            .await
            .expect("stream 2 read");
        assert_eq!(&buf2, marker2, "stream 2 marker mismatch");

        // Now drain stream 1 to verify it also delivered correctly.
        let mut buf1 = vec![0u8; big_payload.len()];
        big_recv.read_exact(&mut buf1).await.expect("stream 1 read");
        assert_eq!(buf1, big_payload, "stream 1 payload mismatch");
    }

    /// Datagrams send and receive via the typed callback surface.
    #[tokio::test]
    async fn datagram_send_recv() {
        let (tls_a, tls_b) = make_tls_pair();

        let mesh_a =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_a).expect("mesh_a listen");
        let addr_a = mesh_a.local_addr().expect("addr_a");

        let mesh_b =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_b).expect("mesh_b listen");

        let (b_connect, a_accept) = tokio::join!(mesh_b.connect(addr_a), mesh_a.accept());
        let conn_b = b_connect.expect("connect");
        let conn_a = a_accept.expect("accept None").expect("accept err");

        // Register a callback on A's side that collects received datagrams.
        let (tx, mut rx) = tokio::sync::mpsc::channel::<bytes::Bytes>(8);
        conn_a
            .register_datagram_handler(move |data| {
                let _ = tx.try_send(data);
            })
            .await;

        // B sends a datagram to A.
        let payload = bytes::Bytes::from_static(b"swim-gossip-ping");
        conn_b
            .send_datagram(payload.clone())
            .expect("send_datagram");

        // A's handler must receive it within a reasonable timeout.
        let received = tokio::time::timeout(std::time::Duration::from_secs(5), rx.recv())
            .await
            .expect("datagram timeout")
            .expect("channel closed");

        assert_eq!(received, payload, "datagram payload mismatch");
    }

    /// `PeerConn::close` sends a QUIC `CONNECTION_CLOSE` frame. Subsequent
    /// operations on the connection return errors rather than blocking.
    #[tokio::test]
    async fn close_sends_connection_close() {
        let (tls_a, tls_b) = make_tls_pair();

        let mesh_a =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_a).expect("mesh_a listen");
        let addr_a = mesh_a.local_addr().expect("addr_a");

        let mesh_b =
            QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_b).expect("mesh_b listen");

        let (b_connect, a_accept) = tokio::join!(mesh_b.connect(addr_a), mesh_a.accept());
        let conn_b = b_connect.expect("connect");
        let _conn_a = a_accept.expect("accept None").expect("accept err");

        // Close B's side with an application code.
        conn_b.close(42, b"test-close").await;

        // Opening a new stream after close must fail (fail-closed).
        let result = conn_b.open_stream(LaneKind(99)).await;
        assert!(result.is_err(), "open_stream after close must fail");
    }
}
