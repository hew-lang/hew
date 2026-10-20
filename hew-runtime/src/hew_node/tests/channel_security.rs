//! Node channels are always protected (#3332): a TCP peer that does not
//! speak Noise is refused in both directions, a Noise peer is admitted, and
//! node traffic never crosses the wire in the clear.

use super::*;
use crate::connection::ChannelRefusal;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};

/// Write one frame in the TCP transport's framing: a little-endian `u32`
/// length, then the payload.
fn write_frame(stream: &mut TcpStream, payload: &[u8]) {
    let len = u32::try_from(payload.len()).expect("test frame fits u32");
    stream.write_all(&len.to_le_bytes()).expect("frame header");
    stream.write_all(payload).expect("frame payload");
}

/// Offer plaintext node traffic; the node may already have closed the socket.
fn offer_plaintext_frame(stream: &mut TcpStream) {
    let payload = b"plaintext node traffic";
    let len = u32::try_from(payload.len()).expect("test frame fits u32");
    let _ = stream.write_all(&len.to_le_bytes());
    let _ = stream.write_all(payload);
}

fn last_error() -> String {
    let error = crate::hew_last_error();
    if error.is_null() {
        return String::new();
    }
    // SAFETY: `hew_last_error` returns a live NUL-terminated string.
    unsafe { CStr::from_ptr(error) }
        .to_string_lossy()
        .into_owned()
}

fn read_frame(stream: &mut TcpStream) -> Vec<u8> {
    let mut header = [0u8; 4];
    stream.read_exact(&mut header).expect("frame header");
    let mut payload = vec![0u8; u32::from_le_bytes(header) as usize];
    stream.read_exact(&mut payload).expect("frame payload");
    payload
}

/// Read until the node closes the socket; returns what arrived first. A
/// reset counts as a close: the node drops the socket with our plaintext
/// frame still unread.
fn read_until_closed(stream: &mut TcpStream) -> Vec<u8> {
    let mut received = Vec::new();
    let mut chunk = [0u8; 4096];
    loop {
        match stream.read(&mut chunk) {
            Ok(0) | Err(_) => return received,
            Ok(n) => received.extend_from_slice(&chunk[..n]),
        }
    }
}

fn mint_identity(
    dir: &std::path::Path,
    name: &str,
) -> (crate::peer_binding::StableNoiseIdentity, std::path::PathBuf) {
    let path = dir.join(name);
    let identity =
        crate::encryption::noise_identity_load_or_create(&path).expect("mint noise identity");
    (identity, path)
}

fn contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack
        .windows(needle.len())
        .any(|window| window == needle)
}

#[test]
fn tcp_dial_refuses_plaintext_and_previous_protocol_peers() {
    let _guard = crate::runtime_test_guard();
    let version = u16::from(crate::envelope::WIRE_VERSION);
    for (offers_noise, version, reason) in [
        (false, version, ChannelRefusal::PlaintextPeer.to_string()),
        (true, version - 1, "handshake protocol mismatch".to_owned()),
    ] {
        let dir = tempfile::tempdir().expect("key directory");
        let (local, local_path) = mint_identity(dir.path(), "local.key");
        let (plaintext_peer, _) = mint_identity(dir.path(), "peer.key");
        let peer_pub = plaintext_peer.public();
        let peer_node = crate::node_identity::NodeId::from_noise_static_key(&peer_pub);
        // The peer's key is pinned, so only the missing channel can refuse it.
        let (node, _port) = start_tcp_node_pinning(710, local, &local_path, &[(711, peer_pub)]);

        let listener = TcpListener::bind("127.0.0.1:0").expect("plaintext peer listener");
        let peer_addr = listener.local_addr().expect("listener address");
        let peer = thread::spawn(move || {
            let (mut stream, _) = listener.accept().expect("node dials the peer");
            let _ = read_frame(&mut stream);
            let mut handshake =
                crate::connection::test_handshake_record(peer_node, 1, peer_pub, offers_noise);
            handshake[4..6].copy_from_slice(&version.to_be_bytes());
            write_frame(&mut stream, &handshake);
            offer_plaintext_frame(&mut stream);
            read_until_closed(&mut stream)
        });

        let target = CString::new(format!("711@{peer_addr}")).expect("connect target");
        crate::hew_clear_error();
        // SAFETY: the node and target are valid for the call.
        let rc = unsafe { hew_node_connect(node.as_ptr(), target.as_ptr()) };
        let error = last_error();
        assert_eq!(
            rc,
            hew_cabi::node::NodeFailure::Refused as i32,
            "a plaintext peer must be refused"
        );
        assert!(
            error.contains(&reason),
            "the refusal must name {reason}, got: {error}"
        );
        // SAFETY: the node is live and started.
        unsafe {
            let mgr = (*node.as_ptr()).conn_mgr;
            assert_eq!(connection::hew_connmgr_count(mgr), 0);
            assert!(connection::hew_connmgr_conn_id_for_node(mgr, 711) < 0);
        }
        assert!(
            peer.join().expect("plaintext peer thread").is_empty(),
            "the node sends nothing after refusing the channel"
        );

        // SAFETY: the node is live until dropped.
        unsafe { assert_eq!(hew_node_stop(node.as_ptr()), 0) };
    }
}

#[test]
fn tcp_accept_refuses_a_plaintext_dialler_and_then_admits_a_noise_peer() {
    let _guard = crate::runtime_test_guard();
    let dir = tempfile::tempdir().expect("key directory");
    let (a, a_path) = mint_identity(dir.path(), "a.key");
    let (b, b_path) = mint_identity(dir.path(), "b.key");
    let (plaintext_peer, _) = mint_identity(dir.path(), "plaintext.key");
    let (a_pub, b_pub, plaintext_pub) = (a.public(), b.public(), plaintext_peer.public());
    let (node_a, _) = start_tcp_node_pinning(720, a, &a_path, &[(721, b_pub)]);
    let (node_b, port_b) =
        start_tcp_node_pinning(721, b, &b_path, &[(720, a_pub), (722, plaintext_pub)]);

    // A pinned key over a plaintext channel is still refused.
    let mut raw = TcpStream::connect(("127.0.0.1", port_b)).expect("raw dial");
    write_frame(
        &mut raw,
        &crate::connection::test_handshake_record(
            crate::node_identity::NodeId::from_noise_static_key(&plaintext_pub),
            1,
            plaintext_pub,
            false,
        ),
    );
    let _ = read_frame(&mut raw);
    offer_plaintext_frame(&mut raw);
    assert!(
        read_until_closed(&mut raw).is_empty(),
        "the node closes a plaintext dialler without sending node traffic"
    );
    // SAFETY: node_b is live and started.
    let installed = unsafe { connection::hew_connmgr_count((*node_b.as_ptr()).conn_mgr) };
    assert_eq!(
        installed, 0,
        "no connection is installed for the plaintext dialler"
    );

    let target = CString::new(format!("721@127.0.0.1:{port_b}")).expect("connect target");
    // SAFETY: both nodes are live and started.
    unsafe {
        connect_with_retry(node_a.as_ptr(), &target);
        wait_for_handshake(node_a.as_ptr(), node_b.as_ptr());
        assert_eq!(
            connection::hew_connmgr_count((*node_b.as_ptr()).conn_mgr),
            1
        );
        assert_eq!(hew_node_stop(node_a.as_ptr()), 0);
        assert_eq!(hew_node_stop(node_b.as_ptr()), 0);
    }
}

/// Forward one TCP connection to `upstream_port`, recording every byte that
/// crosses in either direction.
fn start_recording_relay(upstream_port: u16) -> (u16, Arc<Mutex<Vec<u8>>>) {
    let listener = TcpListener::bind("127.0.0.1:0").expect("relay listener");
    let port = listener.local_addr().expect("relay address").port();
    let recorded = Arc::new(Mutex::new(Vec::new()));
    let sink = Arc::clone(&recorded);
    thread::spawn(move || {
        let (downstream, _) = listener.accept().expect("relay accept");
        let upstream = TcpStream::connect(("127.0.0.1", upstream_port)).expect("relay upstream");
        let pipe = |mut from: TcpStream, mut to: TcpStream, sink: Arc<Mutex<Vec<u8>>>| {
            thread::spawn(move || {
                let mut chunk = [0u8; 4096];
                while let Ok(n) = from.read(&mut chunk) {
                    if n == 0 {
                        break;
                    }
                    sink.lock_or_recover().extend_from_slice(&chunk[..n]);
                    if to.write_all(&chunk[..n]).is_err() {
                        break;
                    }
                }
                let _ = to.shutdown(std::net::Shutdown::Both);
            });
        };
        pipe(
            downstream.try_clone().expect("clone downstream"),
            upstream.try_clone().expect("clone upstream"),
            Arc::clone(&sink),
        );
        pipe(upstream, downstream, sink);
    });
    (port, recorded)
}

#[test]
fn tcp_noise_channel_never_carries_node_traffic_in_the_clear() {
    let _guard = crate::runtime_test_guard();
    let (node_a, _, node_b, port_b) = start_authorized_tcp_pair(730, 731);
    let (relay_port, recorded) = start_recording_relay(port_b);
    let target = CString::new(format!("731@127.0.0.1:{relay_port}")).expect("connect target");
    // SAFETY: both nodes are live and started.
    unsafe {
        connect_with_retry(node_a.as_ptr(), &target);
        wait_for_handshake(node_a.as_ptr(), node_b.as_ptr());
    }

    let marker = *b"hew-channel-marker:this-payload-must-never-cross-the-wire-plain!";
    // SAFETY: node_b is live for this borrow.
    let b = unsafe { &*node_b.as_ptr() };
    let location = Location::new(
        b.auth.node_identity().expect("node b identity"),
        0x99,
        b.auth.session_incarnation().expect("node b session"),
    )
    .expect("valid location");
    // Control: the frame the channel carries holds the marker contiguously,
    // so the detector below would see it if the frame went out unsealed.
    // SAFETY: the marker is valid for its length.
    let clear = unsafe {
        crate::envelope::encode_envelope_frame_from_raw_parts(
            Some(location),
            None,
            crate::envelope::EnvelopeMessage::Service {
                handler: "tests.Marker.handle".into(),
                grant_id: 1,
                kind: crate::request_state::RequestKind::CallWait,
            },
            marker.as_ptr(),
            marker.len(),
            1,
            None,
        )
    }
    .expect("encode envelope");
    assert!(
        contains(&clear, &marker),
        "control: the clear frame shows the marker"
    );

    let before = recorded.lock_or_recover().len();
    // SAFETY: node_a is live; the location and marker are valid for the call.
    let rc = unsafe {
        let mgr = (*node_a.as_ptr()).conn_mgr;
        let conn_id = connection::hew_connmgr_conn_id_for_node(mgr, 731);
        connection::claim_send_to_location(&*mgr, conn_id, location)
            .expect("authenticated target claim")
            .send(&clear)
    };
    assert_eq!(rc, 0, "the send over the Noise channel succeeds");
    poll_until(|_| {
        let crossed = recorded.lock_or_recover().len() >= before + marker.len();
        if !crossed {
            thread::sleep(Duration::from_millis(10));
        }
        crossed
    });
    assert!(
        !contains(&recorded.lock_or_recover(), &marker),
        "the relay must never see the marker"
    );

    // SAFETY: both nodes are live until dropped.
    unsafe {
        assert_eq!(hew_node_stop(node_a.as_ptr()), 0);
        assert_eq!(hew_node_stop(node_b.as_ptr()), 0);
    }
}

/// The handshake can authenticate a pinned peer whose durable session is older
/// than the cluster's retained session. Refusing publication must reach the
/// caller of Node.connect instead of reporting an unusable connection.
#[test]
fn tcp_connect_reports_publication_refusal() {
    let _guard = crate::runtime_test_guard();
    let (node_a, _port_a, node_b, port_b) = start_authorized_tcp_pair(740, 741);
    let publication_sync = Arc::new(Mutex::new(()));
    let publication_removed = Arc::new(AtomicBool::new(false));
    // SAFETY: both test nodes stay alive until the test finishes. Retain a newer
    // peer session in the cluster before the older authenticated peer dials.
    unsafe {
        assert_eq!(
            crate::cluster::hew_cluster_notify_connection_established_for_token_if_not_removed(
                (*node_a.as_ptr()).cluster,
                741,
                2,
                1,
                &publication_sync,
                &publication_removed,
            ),
            1,
        );
        let target = CString::new(format!("741@127.0.0.1:{port_b}")).expect("connect target");
        let status = hew_node_connect(node_a.as_ptr(), target.as_ptr());
        let detail = last_error();
        assert_eq!(
            connection::hew_connmgr_count((*node_a.as_ptr()).conn_mgr),
            0,
            "refused publication must leave no installed connection"
        );
        assert_eq!(
            status,
            hew_cabi::node::NodeFailure::Refused as i32,
            "publication refusal must reach Node.connect: {detail}"
        );
        assert!(
            detail.contains("the cluster rejected session 1"),
            "Node.connect must retain the publication refusal: {detail}"
        );
        assert_eq!(hew_node_stop(node_a.as_ptr()), 0);
        assert_eq!(hew_node_stop(node_b.as_ptr()), 0);
    }
}
