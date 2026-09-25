//! Two-process registry gossip and remote-ask tests.

use super::*;

#[cfg(feature = "encryption")]
fn run_registry_gossip_server_helper() {
    reset_two_process_delivery();
    // Install the runtime before touching the name registry: in this helper
    // subprocess no `runtime_test_guard` is held, so the registry's
    // `rt_current()` resolver has nothing to read until init runs.
    let _real_sched = init_real_scheduler();
    crate::registry::hew_registry_clear();

    let (node, port, _client_identity) = start_authorized_tcp_node(
        TWO_PROCESS_REGISTRY_SERVER_NODE,
        TWO_PROCESS_REGISTRY_CLIENT_NODE,
    );
    crate::pid::hew_pid_set_local_node(TWO_PROCESS_REGISTRY_SERVER_NODE);

    let worker = spawn_remote_test_actor(two_process_registry_dispatch);
    inbound_test_request_codec(two_process_registry_dispatch, TWO_PROCESS_REGISTRY_MSG_TYPE);
    assert!(!worker.is_null(), "server worker spawn failed");
    // SAFETY: actor was just spawned successfully.
    let worker_pid = unsafe { (*worker).id };
    assert_eq!(
        crate::pid::hew_pid_node(worker_pid),
        TWO_PROCESS_REGISTRY_SERVER_NODE,
        "server worker PID must encode the server node id"
    );

    let name = CString::new(TWO_PROCESS_REGISTRY_NAME).expect("valid registry name");
    // SAFETY: node/name/worker_pid are valid in this helper process.
    let register_rc = unsafe { hew_node_register(node.as_ptr(), name.as_ptr(), worker_pid) };
    assert_eq!(register_rc, 0, "server register");

    let ready_file = std::env::var(TWO_PROCESS_READY_FILE_ENV).expect("ready file env");
    std::fs::write(&ready_file, port.to_string()).expect("write ready file");

    let delivered = wait_for_two_process_delivery(Duration::from_secs(30));

    // SAFETY: actor and node are owned by this helper process.
    unsafe {
        let _ = crate::actor::hew_actor_free(worker);
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
    assert!(delivered, "server did not observe two-process send");
}

#[cfg(feature = "encryption")]
fn run_registry_gossip_client_helper() {
    // Install the runtime before touching the name registry (helper
    // subprocess holds no `runtime_test_guard`).
    let _real_sched = init_real_scheduler();
    crate::registry::hew_registry_clear();

    let server_port = std::env::var(TWO_PROCESS_SERVER_PORT_ENV)
        .expect("server port env")
        .parse::<u16>()
        .expect("server port");
    let (node, _client_port, server_identity) = start_authorized_tcp_node(
        TWO_PROCESS_REGISTRY_CLIENT_NODE,
        TWO_PROCESS_REGISTRY_SERVER_NODE,
    );

    let connect_addr = CString::new(format!(
        "{TWO_PROCESS_REGISTRY_SERVER_NODE}@127.0.0.1:{server_port}"
    ))
    .expect("valid connect addr");
    // SAFETY: node and connect_addr are valid.
    unsafe { connect_with_retry(node.as_ptr(), &connect_addr) };
    assert!(
        wait_for_single_connection(node.as_ptr(), Duration::from_secs(5)),
        "client connection did not become active"
    );

    let name = CString::new(TWO_PROCESS_REGISTRY_NAME).expect("valid registry name");
    let remote_pid = wait_for_remote_lookup(
        node.as_ptr(),
        name.as_ptr(),
        server_identity,
        Duration::from_secs(30),
    )
    .expect("client lookup did not resolve remote registry gossip");

    // SAFETY: remote_pid was resolved from registry gossip and the request
    // is the registered member's u32 payload.
    let rc = unsafe {
        hew_node_send_location(
            node.as_ptr(),
            &raw const remote_pid,
            inbound_test_request_codec(
                two_process_registry_dispatch,
                TWO_PROCESS_REGISTRY_MSG_TYPE,
            ),
            TWO_PROCESS_REGISTRY_MSG_TYPE,
            TEST_U32_REQUEST.cast(),
            std::mem::size_of::<u32>(),
        )
    };
    assert_eq!(rc, 0, "client remote send");

    // SAFETY: node is owned by this helper process.
    unsafe {
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// Poll for a marker file's existence up to `timeout`. Used from inside a
/// two-process helper subprocess body, which has no `ManagedChild` handle
/// on its peer to detect an early exit; the orchestrating parent's own
/// bounded `wait_output` is the backstop that surfaces a hang as a timeout
/// with both processes' captured output.
#[cfg(feature = "encryption")]
fn wait_for_marker_file(path: &std::path::Path, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while !path.exists() {
        if Instant::now() >= deadline {
            return false;
        }
        thread::sleep(Duration::from_millis(20));
    }
    true
}

/// Server half of the dead-actor cross-process ask case: registers a
/// worker, lets the client resolve it over gossip, then fully frees it
/// (stop + free — not merely stop) before the client asks it, reproducing
/// a target that is genuinely gone rather than merely stopped-but-tracked.
#[cfg(feature = "encryption")]
fn run_two_process_ask_dead_server_helper() {
    register_test_u32_codec(dispatch_key(noop_dispatch), TWO_PROCESS_REGISTRY_MSG_TYPE);
    let _real_sched = init_real_scheduler();
    crate::registry::hew_registry_clear();

    let (node, port, _client_identity) = start_authorized_tcp_node(
        TWO_PROCESS_REGISTRY_SERVER_NODE,
        TWO_PROCESS_REGISTRY_CLIENT_NODE,
    );
    crate::pid::hew_pid_set_local_node(TWO_PROCESS_REGISTRY_SERVER_NODE);
    let worker = spawn_remote_test_actor(noop_dispatch);
    assert!(!worker.is_null(), "dead-actor server worker spawn failed");
    // SAFETY: actor was just spawned successfully.
    let worker_pid = unsafe { (*worker).id };
    assert_eq!(
        crate::pid::hew_pid_node(worker_pid),
        TWO_PROCESS_REGISTRY_SERVER_NODE
    );

    let name = CString::new(TWO_PROCESS_ASK_DEAD_NAME).expect("valid ask registry name");
    // SAFETY: node/name/worker_pid are valid in this helper process.
    let register_rc = unsafe { hew_node_register(node.as_ptr(), name.as_ptr(), worker_pid) };
    assert_eq!(register_rc, 0, "dead-actor server register");

    let ready_file = std::env::var(TWO_PROCESS_READY_FILE_ENV).expect("ready file env");
    std::fs::write(&ready_file, port.to_string()).expect("write ready file");

    let resolved_file = std::env::var(TWO_PROCESS_RESOLVED_FILE_ENV).expect("resolved file env");
    assert!(
        wait_for_marker_file(
            std::path::Path::new(&resolved_file),
            Duration::from_secs(30)
        ),
        "client did not confirm registry resolution before the actor was freed"
    );

    // SAFETY: actor and node are owned by this helper process.
    unsafe {
        crate::actor::hew_actor_stop(worker);
        let _ = crate::actor::hew_actor_free(worker);
    }
    // `hew_actor_free` returns only once the actor is fully untracked, so
    // the client is safe to ask now — signal it rather than let the ask
    // race the free (see `TWO_PROCESS_FREED_FILE_ENV`'s doc comment).
    let freed_file = std::env::var(TWO_PROCESS_FREED_FILE_ENV).expect("freed file env");
    std::fs::write(&freed_file, "1").expect("write freed file");

    // Wait for the client's confirmed round trip rather than a fixed
    // sleep: stopping the node (which drops the connection) before a
    // slow-but-successful ask arrives would surface `ConnectionDropped`
    // instead of the rejection reason under test.
    let done_file = std::env::var(TWO_PROCESS_DONE_FILE_ENV).expect("done file env");
    assert!(
        wait_for_marker_file(std::path::Path::new(&done_file), Duration::from_secs(30)),
        "client did not confirm its ask resolved before the server timeout"
    );
    // SAFETY: node is owned by this helper process.
    unsafe {
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// Client half of the dead-actor cross-process ask case (see the server
/// half's doc comment).
#[cfg(feature = "encryption")]
fn run_two_process_ask_dead_client_helper() {
    let client = run_two_process_ask_client_setup(
        TWO_PROCESS_REGISTRY_CLIENT_NODE,
        TWO_PROCESS_REGISTRY_SERVER_NODE,
        TWO_PROCESS_ASK_DEAD_NAME,
    );
    let node = &client.node;
    let remote_pid = client.remote_pid;

    let resolved_file = std::env::var(TWO_PROCESS_RESOLVED_FILE_ENV).expect("resolved file env");
    std::fs::write(&resolved_file, "1").expect("write resolved file");

    // Wait for the server's confirmed, complete free rather than asking
    // immediately: an ask that lands mid-teardown (after `hew_actor_stop`
    // but before `hew_actor_free` finishes untracking it) can race the
    // mailbox's orphaned-ask completion — a distinct, pre-existing
    // mailbox-teardown concern this test does not exercise.
    let freed_file = std::env::var(TWO_PROCESS_FREED_FILE_ENV).expect("freed file env");
    assert!(
        wait_for_marker_file(std::path::Path::new(&freed_file), Duration::from_secs(30)),
        "server did not confirm the target actor was freed before the ask"
    );

    let send_value: u32 = 21;
    // SAFETY: remote_pid was resolved from a separate helper process over TCP.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const remote_pid,
            test_dispatch(),
            TWO_PROCESS_REGISTRY_MSG_TYPE,
            (&raw const send_value).cast::<c_void>().cast_mut(),
            std::mem::size_of::<u32>(),
            // The rejection is the outcome, so the ask has no deadline to
            // race it.
            NO_ASK_DEADLINE_MS,
            std::mem::size_of::<u32>(),
        )
    };
    // Signal completion before asserting: the server waits on this file
    // (bounded) rather than a fixed sleep before it stops its node, so a
    // slow-but-successful round trip never races the server tearing down
    // the connection out from under a not-yet-arrived ask.
    let done_file = std::env::var(TWO_PROCESS_DONE_FILE_ENV).expect("done file env");
    std::fs::write(&done_file, "1").expect("write done file");
    assert!(
        status != AskError::None as i32,
        "ask to a dead remote actor unexpectedly returned a reply"
    );
    assert_eq!(
        status,
        AskError::ActorStopped as i32,
        "a remote ask to a dead actor must report ActorStopped, matching a local dead \
         target's reason rather than DecodeFailure"
    );
    assert_eq!(
        crate::internal::types::hew_ask_error_translate_for_public_result(status),
        3,
        "a dead remote actor must surface the same public ActorError.Dead ordinal (3) \
         the local ask route reports (D526)"
    );

    // SAFETY: node is owned by this helper process.
    unsafe {
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
fn run_two_process_ask_server_helper(
    node_id: u16,
    name: &str,
    dispatch: unsafe extern "C-unwind" fn(
        *mut crate::execution_context::HewExecutionContext,
        *mut c_void,
        i32,
        *mut c_void,
        usize,
        i32,
    ) -> *mut c_void,
) {
    // The inbound-ask path decodes the request and encodes the reply via the
    // registered codec (fail-closed), keyed by the target actor's dispatch.
    // Register under the SAME dispatch fn this server spawns its echo actor
    // with so the inbound decode resolves the matching codec.
    register_test_u32_codec(dispatch_key(dispatch), TWO_PROCESS_REGISTRY_MSG_TYPE);
    reset_two_process_ask_observed();
    // Install the runtime before touching the name registry (helper
    // subprocess holds no `runtime_test_guard`).
    let _real_sched = init_real_scheduler();
    crate::registry::hew_registry_clear();

    let (node, port, _client_identity) =
        start_authorized_tcp_node(node_id, TWO_PROCESS_REGISTRY_CLIENT_NODE);
    crate::pid::hew_pid_set_local_node(node_id);
    let worker = spawn_remote_test_actor(dispatch);
    assert!(!worker.is_null(), "ask server worker spawn failed");
    // SAFETY: actor was just spawned successfully.
    let worker_pid = unsafe { (*worker).id };
    assert_eq!(crate::pid::hew_pid_node(worker_pid), node_id);

    let name = CString::new(name).expect("valid ask registry name");
    // SAFETY: node/name/worker_pid are valid in this helper process.
    let register_rc = unsafe { hew_node_register(node.as_ptr(), name.as_ptr(), worker_pid) };
    assert_eq!(register_rc, 0, "ask server register");

    let ready_file = std::env::var(TWO_PROCESS_READY_FILE_ENV).expect("ready file env");
    std::fs::write(&ready_file, port.to_string()).expect("write ready file");

    assert!(
        wait_for_two_process_ask_observed(Duration::from_secs(30)),
        "ask server did not observe remote ask"
    );
    // Hold the connection until the client has its outcome, so stopping this
    // node can never turn a late reply or a late Timeout into a dropped
    // connection.
    let done_file = std::env::var(TWO_PROCESS_DONE_FILE_ENV).expect("done file env");
    assert!(
        wait_for_marker_file(std::path::Path::new(&done_file), Duration::from_secs(30)),
        "ask client never reported its outcome"
    );

    // SAFETY: actor and node are owned by this helper process.
    unsafe {
        let _ = crate::actor::hew_actor_free(worker);
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
struct TwoProcessAskClient {
    node: TestNode,
    remote_pid: HewRemotePid,
    _real_sched: RealSchedulerGuard,
}

#[cfg(feature = "encryption")]
fn run_two_process_ask_echo_client_helper() {
    let client = run_two_process_ask_client_setup(
        TWO_PROCESS_REGISTRY_CLIENT_NODE,
        TWO_PROCESS_REGISTRY_SERVER_NODE,
        TWO_PROCESS_ASK_ECHO_NAME,
    );
    let node = &client.node;
    let remote_pid = client.remote_pid;
    let send_value: u32 = 21;
    // SAFETY: remote_pid was resolved from a separate helper process over TCP.
    let (reply, status) = unsafe {
        ask_for_test(
            &raw const remote_pid,
            test_dispatch(),
            TWO_PROCESS_REGISTRY_MSG_TYPE,
            (&raw const send_value).cast::<c_void>().cast_mut(),
            std::mem::size_of::<u32>(),
            NO_ASK_DEADLINE_MS,
            std::mem::size_of::<u32>(),
        )
    };
    let done_file = std::env::var(TWO_PROCESS_DONE_FILE_ENV).expect("done file env");
    std::fs::write(&done_file, "1").expect("write done file");
    assert!(
        status == AskError::None as i32,
        "two-process echo ask returned null"
    );
    let reply_value = u32::from_ne_bytes(reply[..4].try_into().expect("u32 reply"));
    assert_eq!(
        reply_value, 42,
        "two-process echo-double ask must return 42"
    );
    assert_eq!(status, AskError::None as i32);
    // SAFETY: node is owned by this helper process.
    unsafe {
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
fn run_two_process_ask_timeout_client_helper() {
    let client = run_two_process_ask_client_setup(
        TWO_PROCESS_REGISTRY_CLIENT_NODE,
        TWO_PROCESS_REGISTRY_SERVER_NODE,
        TWO_PROCESS_ASK_TIMEOUT_NAME,
    );
    let node = &client.node;
    let remote_pid = client.remote_pid;
    let send_value: u32 = 21;
    // SAFETY: remote_pid was resolved from a separate helper process over TCP.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const remote_pid,
            test_dispatch(),
            TWO_PROCESS_REGISTRY_MSG_TYPE,
            (&raw const send_value).cast::<c_void>().cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        )
    };
    let done_file = std::env::var(TWO_PROCESS_DONE_FILE_ENV).expect("done file env");
    std::fs::write(&done_file, "1").expect("write done file");
    // The deterministic invariant is the typed OUTCOME: a server that never
    // replies (the timeout-server holds until the client has its outcome) must surface
    // `AskError::Timeout` with a null reply. Elapsed wall time is NOT asserted
    // — under load the 250ms ask deadline can fire later, but the OUTCOME is
    // load-independent. The ask's own timeout is the hang ceiling; a genuine
    // never-resolving ask is caught by nextest's slow-timeout, not a window.
    assert!(
        status != AskError::None as i32,
        "timeout ask unexpectedly returned a reply"
    );
    assert_eq!(status, AskError::Timeout as i32);
    // SAFETY: node is owned by this helper process.
    unsafe {
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
fn run_two_process_ask_client_setup(
    client_node_id: u16,
    server_node_id: u16,
    registry_name: &str,
) -> TwoProcessAskClient {
    // The cross-node send/ask path requires a registered codec for the
    // payload's msg_type (fail-closed). Register the test u32 codec.
    register_test_u32_codec(test_dispatch(), TWO_PROCESS_REGISTRY_MSG_TYPE);
    // Install the runtime before touching the name registry (helper
    // subprocess holds no `runtime_test_guard`).
    let real_sched = init_real_scheduler();
    crate::registry::hew_registry_clear();

    let server_port = std::env::var(TWO_PROCESS_SERVER_PORT_ENV)
        .expect("server port env")
        .parse::<u16>()
        .expect("server port");
    let (node, _client_port, server_identity) =
        start_authorized_tcp_node(client_node_id, server_node_id);

    let connect_addr = CString::new(format!("{server_node_id}@127.0.0.1:{server_port}"))
        .expect("valid connect addr");
    // SAFETY: node and connect_addr are valid.
    unsafe { connect_with_retry(node.as_ptr(), &connect_addr) };
    assert!(
        wait_for_single_connection(node.as_ptr(), Duration::from_secs(5)),
        "ask client connection did not become active"
    );

    let name = CString::new(registry_name).expect("valid registry name");
    let remote_pid = wait_for_remote_lookup(
        node.as_ptr(),
        name.as_ptr(),
        server_identity,
        Duration::from_secs(30),
    )
    .expect("ask client lookup did not resolve remote registry gossip");
    assert_eq!(
        Location::try_from(remote_pid).unwrap().node(),
        server_identity,
        "ask test must resolve to the authenticated server identity"
    );
    TwoProcessAskClient {
        node,
        remote_pid,
        _real_sched: real_sched,
    }
}

#[cfg(feature = "encryption")]
#[test]
fn registry_gossip_two_process_server_helper() {
    if !matches!(
        std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
        Ok("server")
    ) {
        return;
    }
    run_registry_gossip_server_helper();
}

#[cfg(feature = "encryption")]
#[test]
fn registry_gossip_two_process_client_helper() {
    if !matches!(
        std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
        Ok("client")
    ) {
        return;
    }
    run_registry_gossip_client_helper();
}

#[cfg(feature = "encryption")]
#[test]
fn remote_ask_two_process_echo_server_helper() {
    if !matches!(
        std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
        Ok("ask_echo_server")
    ) {
        return;
    }
    run_two_process_ask_server_helper(
        TWO_PROCESS_REGISTRY_SERVER_NODE,
        TWO_PROCESS_ASK_ECHO_NAME,
        ask_probe_dispatch,
    );
}

#[cfg(feature = "encryption")]
#[test]
fn remote_ask_two_process_echo_client_helper() {
    if !matches!(
        std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
        Ok("ask_echo_client")
    ) {
        return;
    }
    run_two_process_ask_echo_client_helper();
}

#[cfg(feature = "encryption")]
#[test]
fn remote_ask_two_process_timeout_server_helper() {
    if !matches!(
        std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
        Ok("ask_timeout_server")
    ) {
        return;
    }
    run_two_process_ask_server_helper(
        TWO_PROCESS_REGISTRY_SERVER_NODE,
        TWO_PROCESS_ASK_TIMEOUT_NAME,
        blocked_ask_probe_dispatch,
    );
}

#[cfg(feature = "encryption")]
#[test]
fn remote_ask_two_process_timeout_client_helper() {
    if !matches!(
        std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
        Ok("ask_timeout_client")
    ) {
        return;
    }
    run_two_process_ask_timeout_client_helper();
}

#[cfg(feature = "encryption")]
#[test]
fn remote_ask_two_process_dead_server_helper() {
    if !matches!(
        std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
        Ok("ask_dead_server")
    ) {
        return;
    }
    run_two_process_ask_dead_server_helper();
}

#[cfg(feature = "encryption")]
#[test]
fn remote_ask_two_process_dead_client_helper() {
    if !matches!(
        std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
        Ok("ask_dead_client")
    ) {
        return;
    }
    run_two_process_ask_dead_client_helper();
}

/// Broker a real Noise key exchange for the two-process tests (issue #2652,
/// D110). Mints both nodes' stable identities up-front in the shared temp
/// dir and returns `(server_keyfile, server_pubkey_hex, client_keyfile,
/// client_pubkey_hex)`. Each helper is handed its own keyfile (which it
/// re-loads to the identical identity) plus the *peer's* pubkey, so both
/// sides bind each other before connecting — no test-only posture promotion.
#[cfg(feature = "encryption")]
fn broker_two_process_noise_keys(dir: &std::path::Path) -> (String, String, String, String) {
    use crate::peer_binding::hex_lower;
    let server_keyfile = dir.join("server.key");
    let client_keyfile = dir.join("client.key");
    let server_id = crate::encryption::noise_identity_load_or_create(&server_keyfile)
        .expect("mint 2p server identity");
    let client_id = crate::encryption::noise_identity_load_or_create(&client_keyfile)
        .expect("mint 2p client identity");
    (
        server_keyfile.to_string_lossy().into_owned(),
        hex_lower(&server_id.public()),
        client_keyfile.to_string_lossy().into_owned(),
        hex_lower(&client_id.public()),
    )
}

#[cfg(feature = "encryption")]
#[test]
fn two_process_registry_gossip_lookup_then_tell() {
    let _guard = crate::runtime_test_guard();
    let ready_dir = tempfile::tempdir().expect("ready tempdir");
    let ready_file = ready_dir.path().join("server-ready");
    let ready_file_s = ready_file.to_string_lossy().into_owned();

    // Broker a real Noise key exchange (D110) so both nodes admit Strict.
    let (server_keyfile, server_pub_hex, client_keyfile, client_pub_hex) =
        broker_two_process_noise_keys(ready_dir.path());

    let mut server = spawn_registry_gossip_helper(
        "hew_node::tests::two_process::registry_gossip_two_process_server_helper",
        "server",
        &[
            (TWO_PROCESS_READY_FILE_ENV, ready_file_s),
            (TWO_PROCESS_KEYFILE_ENV, server_keyfile),
            (TWO_PROCESS_PEER_PUBKEY_ENV, client_pub_hex),
        ],
    );
    let server_port = wait_for_ready_port(&ready_file, &mut server, Duration::from_secs(10));

    let mut client = spawn_registry_gossip_helper(
        "hew_node::tests::two_process::registry_gossip_two_process_client_helper",
        "client",
        &[
            (TWO_PROCESS_SERVER_PORT_ENV, server_port.to_string()),
            (TWO_PROCESS_KEYFILE_ENV, client_keyfile),
            (TWO_PROCESS_PEER_PUBKEY_ENV, server_pub_hex),
        ],
    );
    let client_output = client.wait_output(Duration::from_secs(40));
    assert_child_success("client", &client_output);

    let server_output = server.wait_output(Duration::from_secs(40));
    assert_child_success("server", &server_output);
}

#[cfg(feature = "encryption")]
fn run_two_process_remote_ask_case(
    server_helper: &'static str,
    server_role: &'static str,
    client_helper: &'static str,
    client_role: &'static str,
) {
    let _guard = crate::runtime_test_guard();
    let ready_dir = tempfile::tempdir().expect("ready tempdir");
    let ready_file = ready_dir.path().join("ask-server-ready");
    let ready_file_s = ready_file.to_string_lossy().into_owned();
    let done_file = ready_dir.path().join("ask-client-done");
    let done_file_s = done_file.to_string_lossy().into_owned();

    // Broker a real Noise key exchange (D110) so both nodes admit Strict.
    let (server_keyfile, server_pub_hex, client_keyfile, client_pub_hex) =
        broker_two_process_noise_keys(ready_dir.path());

    let mut server = spawn_registry_gossip_helper(
        server_helper,
        server_role,
        &[
            (TWO_PROCESS_READY_FILE_ENV, ready_file_s),
            (TWO_PROCESS_DONE_FILE_ENV, done_file_s.clone()),
            (TWO_PROCESS_KEYFILE_ENV, server_keyfile),
            (TWO_PROCESS_PEER_PUBKEY_ENV, client_pub_hex),
        ],
    );
    let server_port = wait_for_ready_port(&ready_file, &mut server, Duration::from_secs(10));

    let mut client = spawn_registry_gossip_helper(
        client_helper,
        client_role,
        &[
            (TWO_PROCESS_SERVER_PORT_ENV, server_port.to_string()),
            (TWO_PROCESS_DONE_FILE_ENV, done_file_s),
            (TWO_PROCESS_KEYFILE_ENV, client_keyfile),
            (TWO_PROCESS_PEER_PUBKEY_ENV, server_pub_hex),
        ],
    );
    let client_output = client.wait_output(Duration::from_secs(40));
    assert_child_success(client_role, &client_output);

    let server_output = server.wait_output(Duration::from_secs(40));
    assert_child_success(server_role, &server_output);
}

#[cfg(feature = "encryption")]
#[test]
fn two_process_remote_ask_echo_double_returns_42() {
    run_two_process_remote_ask_case(
        "hew_node::tests::two_process::remote_ask_two_process_echo_server_helper",
        "ask_echo_server",
        "hew_node::tests::two_process::remote_ask_two_process_echo_client_helper",
        "ask_echo_client",
    );
}

#[cfg(feature = "encryption")]
#[test]
fn two_process_remote_ask_timeout_returns_timeout() {
    run_two_process_remote_ask_case(
        "hew_node::tests::two_process::remote_ask_two_process_timeout_server_helper",
        "ask_timeout_server",
        "hew_node::tests::two_process::remote_ask_two_process_timeout_client_helper",
        "ask_timeout_client",
    );
}

/// Orchestrates the dead-actor cross-process ask case. Distinct from
/// [`run_two_process_remote_ask_case`]: the server and client rendezvous
/// through an extra marker file so the actor is freed only after the
/// client has resolved it over gossip (`hew_actor_free` unregisters the
/// name and emits a gossip-remove, which would otherwise race the lookup).
#[cfg(feature = "encryption")]
fn run_two_process_dead_ask_case() {
    let _guard = crate::runtime_test_guard();
    let ready_dir = tempfile::tempdir().expect("ready tempdir");
    let ready_file = ready_dir.path().join("ask-dead-server-ready");
    let ready_file_s = ready_file.to_string_lossy().into_owned();
    let resolved_file = ready_dir.path().join("ask-dead-resolved");
    let resolved_file_s = resolved_file.to_string_lossy().into_owned();
    let freed_file = ready_dir.path().join("ask-dead-freed");
    let freed_file_s = freed_file.to_string_lossy().into_owned();
    let done_file = ready_dir.path().join("ask-dead-done");
    let done_file_s = done_file.to_string_lossy().into_owned();

    // Broker a real Noise key exchange (D110) so both nodes admit Strict.
    let (server_keyfile, server_pub_hex, client_keyfile, client_pub_hex) =
        broker_two_process_noise_keys(ready_dir.path());

    let mut server = spawn_registry_gossip_helper(
        "hew_node::tests::two_process::remote_ask_two_process_dead_server_helper",
        "ask_dead_server",
        &[
            (TWO_PROCESS_READY_FILE_ENV, ready_file_s),
            (TWO_PROCESS_RESOLVED_FILE_ENV, resolved_file_s.clone()),
            (TWO_PROCESS_FREED_FILE_ENV, freed_file_s.clone()),
            (TWO_PROCESS_DONE_FILE_ENV, done_file_s.clone()),
            (TWO_PROCESS_KEYFILE_ENV, server_keyfile),
            (TWO_PROCESS_PEER_PUBKEY_ENV, client_pub_hex),
        ],
    );
    let server_port = wait_for_ready_port(&ready_file, &mut server, Duration::from_secs(10));

    let mut client = spawn_registry_gossip_helper(
        "hew_node::tests::two_process::remote_ask_two_process_dead_client_helper",
        "ask_dead_client",
        &[
            (TWO_PROCESS_SERVER_PORT_ENV, server_port.to_string()),
            (TWO_PROCESS_RESOLVED_FILE_ENV, resolved_file_s),
            (TWO_PROCESS_FREED_FILE_ENV, freed_file_s),
            (TWO_PROCESS_DONE_FILE_ENV, done_file_s),
            (TWO_PROCESS_KEYFILE_ENV, client_keyfile),
            (TWO_PROCESS_PEER_PUBKEY_ENV, server_pub_hex),
        ],
    );
    let client_output = client.wait_output(Duration::from_secs(40));
    assert_child_success("ask_dead_client", &client_output);

    let server_output = server.wait_output(Duration::from_secs(40));
    assert_child_success("ask_dead_server", &server_output);
}

/// A remote ask targeting an actor that has been fully freed on its own
/// node, across a genuine OS-process boundary over TCP, must report the
/// same `Dead` reason a local ask to a dead target reports (D526), not
/// `DecodeFailure`. Complements the in-process QUIC-mesh coverage above
/// with a real two-process reproduction of the reported symptom.
#[cfg(feature = "encryption")]
#[test]
fn two_process_remote_ask_dead_actor_reports_dead() {
    run_two_process_dead_ask_case();
}
