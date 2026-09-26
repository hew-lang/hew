use super::*;
use crate::peer_binding::{ConfigState, PeerCredential, PEER_AUTH_STATE};
use std::process::{Child, Command, Output, Stdio};
use std::sync::atomic::{AtomicU32, AtomicUsize};
use std::time::{Duration, Instant};

mod api;
mod dist_probes;
mod inbound;
mod lifecycle;
mod remote_wire;
mod reply;
mod swim;
mod two_process;

/// Ask deadline for tests whose outcome IS the timeout: no reply can ever
/// arrive, so a late-firing deadline on a loaded host still reports Timeout.
const TEST_REMOTE_ASK_TIMEOUT_MS: u64 = 250;

/// Call `ready` with an attempt count until it reports true. The pass
/// condition is the polled event; the test runner's timeout is the hang
/// guard.
fn poll_until(mut ready: impl FnMut(u32) -> bool) {
    let mut attempt = 0;
    while !ready(attempt) {
        attempt += 1;
    }
}

/// Ask deadline for tests whose outcome is a reply or a typed rejection: it
/// never fires, so that outcome cannot turn into Timeout on a loaded host.
/// The test runner's timeout is the hang guard.
const NO_ASK_DEADLINE_MS: u64 = u64::MAX;

fn test_node_id(route_slot: u16) -> crate::node_identity::NodeId {
    let mut bytes = [0_u8; 16];
    bytes[14..].copy_from_slice(&route_slot.to_be_bytes());
    crate::node_identity::NodeId::from_bytes(bytes)
}

fn test_location(route_slot: u16, actor_slot: u64) -> Location {
    Location::new(test_node_id(route_slot), actor_slot, 1).unwrap()
}

fn remote_pid_for_node(node: &TestNode, actor_id: u64) -> HewRemotePid {
    // SAFETY: `TestNode` owns a live, non-null node pointer for this test.
    let node = unsafe { node.as_ptr().as_ref() }.expect("test node must be live");
    assert_eq!(crate::pid::hew_pid_node(actor_id), node.route_slot);
    let location = Location::new(
        node.auth
            .node_identity()
            .expect("authorized test node must have an identity"),
        crate::pid::hew_pid_serial(actor_id),
        node.auth
            .session_incarnation()
            .expect("authorized test node must have a session"),
    )
    .expect("test actor must produce a valid location");
    HewRemotePid::from(location)
}

unsafe fn lookup_exact(node: *mut HewNode, name: *const c_char) -> Option<Location> {
    let mut found = HewRemotePid::default();
    // SAFETY: caller guarantees node/name validity and `found` is writable.
    if unsafe { hew_node_lookup_location(node, name, &raw mut found) } == 0 {
        Location::try_from(found).ok()
    } else {
        None
    }
}

unsafe fn install_test_auth(node: *mut HewNode, route_slot: u16) {
    let snapshot = PeerAuthSnapshot::for_test(test_node_id(route_slot), []);
    // SAFETY: caller owns a stopped test node.
    assert_eq!(unsafe { hew_node_set_auth_snapshot(node, snapshot) }, 0);
}

/// Initialise a real, worker-backed scheduler for a node test (delegates to
/// the scheduler-side helper, which sees the module-private `stealers` and
/// safely retires a `runtime_test_guard()` placeholder before init).
#[must_use]
fn init_real_scheduler() -> RealSchedulerGuard {
    crate::scheduler::init_real_scheduler_for_test();
    RealSchedulerGuard
}

struct RealSchedulerGuard;

impl Drop for RealSchedulerGuard {
    fn drop(&mut self) {
        crate::scheduler::hew_sched_shutdown();
        crate::scheduler::hew_runtime_cleanup();
    }
}

// ── Test-only u32 codec ──────────────────────────────────────────────
//
// The two-process remote-send/ask tests transmit a controlled `u32` rather
// than a Hew-compiled struct, so no codegen-emitted codec exists for their
// `TWO_PROCESS_REGISTRY_MSG_TYPE`. The cross-node send path now requires a
// registered codec (fail-closed: no raw-byte escape). These thunks provide
// the trivial u32 serialize/deserialize the tests need, matching the codec
// ABI exactly (ser: (value_ptr, out_len) -> bytes; deser: (data, len,
// out_struct_size) -> value). They register for the request AND, for asks,
// the reply (both sides of an echo are u32).

unsafe extern "C" fn test_u32_serialize(
    value_ptr: *const std::ffi::c_void,
    out_len: *mut usize,
) -> *mut u8 {
    // SAFETY: value_ptr points to a live u32 from the test.
    let v = unsafe { *value_ptr.cast::<u32>() };
    let buf = crate::cbor_serial::hew_cbor_ser_new();
    // SAFETY: buf is a fresh live CborSerBuf handle.
    unsafe { crate::cbor_serial::hew_cbor_ser_u64(buf, u64::from(v)) };
    // SAFETY: buf is consumed by finish; out_len is a valid pointer.
    unsafe { crate::cbor_serial::hew_cbor_ser_finish(buf, out_len) }
}

unsafe extern "C" fn test_u32_deserialize(
    data: *const u8,
    len: usize,
    out_struct_size: *mut usize,
) -> *mut std::ffi::c_void {
    // SAFETY: data is valid for len bytes.
    let reader = unsafe { crate::cbor_serial::hew_cbor_de_new(data, len) };
    // SAFETY: reader is a live handle.
    let v64 = unsafe { crate::cbor_serial::hew_cbor_de_u64(reader) };
    #[allow(
        clippy::cast_possible_truncation,
        reason = "test payload is always a u32 round-tripped through the u64 primitive"
    )]
    let v = v64 as u32;
    // SAFETY: reader is a live handle.
    let failed = unsafe { crate::cbor_serial::hew_cbor_de_failed(reader) };
    // SAFETY: reader is a live handle.
    unsafe { crate::cbor_serial::hew_cbor_de_free(reader) };
    if failed != 0 {
        if !out_struct_size.is_null() {
            // SAFETY: out_struct_size validated non-null.
            unsafe { *out_struct_size = 0 };
        }
        return std::ptr::null_mut();
    }
    // SAFETY: allocate a u32-sized value the caller owns via buf_free.
    let dst = crate::mem::buf_try_alloc(std::mem::size_of::<u32>()).cast::<u32>();
    if dst.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: dst is a valid u32 allocation.
    unsafe { *dst = v };
    if !out_struct_size.is_null() {
        // SAFETY: out_struct_size validated non-null.
        unsafe { *out_struct_size = std::mem::size_of::<u32>() };
    }
    dst.cast::<std::ffi::c_void>()
}

/// The codec-registry key `(dispatch, msg_type)` uses the TARGET actor
/// TYPE's dispatch function pointer. In these tests the echo actor is spawned
/// with a concrete dispatch fn (e.g. `ask_probe_dispatch`); the inbound
/// decode resolves THAT pointer from the target actor, so the codec must be
/// registered under the SAME dispatch and the originating ask must pass it.
/// This helper casts a dispatch fn to the opaque `*const c_void` key.
fn dispatch_key(f: crate::internal::types::HewDispatchFn) -> *const c_void {
    f as *const c_void
}

/// Canonical codec key for the single-process two-node round-trip tests and
/// the two-process client side: the echo actor is spawned with
/// `ask_probe_dispatch`, so its inbound decode resolves that pointer. The
/// register, the ask FFI arg, and the spawned-actor dispatch all use this.
fn test_dispatch() -> *const c_void {
    dispatch_key(ask_probe_dispatch)
}

/// Spawn a native actor, the only kind compiled Hew publishes, around a
/// test dispatch. Its one-byte state owns nothing.
fn spawn_remote_test_actor(
    dispatch: crate::internal::types::HewDispatchFn,
) -> *mut crate::actor::HewActor {
    spawn_bounded_test_actor(dispatch, 0)
}

/// As [`spawn_remote_test_actor`], with a mailbox of `capacity` messages
/// that refuses new ones when full (unbounded when zero).
fn spawn_bounded_test_actor(
    dispatch: crate::internal::types::HewDispatchFn,
    capacity: i32,
) -> *mut crate::actor::HewActor {
    unsafe extern "C" fn release_state(_state: *mut c_void) {}
    unsafe extern "C-unwind" fn clone_state(_state: *const c_void) -> *mut c_void {
        crate::mem::buf_alloc(1)
    }
    let mut fault = ptr::null_mut();
    // SAFETY: the state is a fresh one-byte allocation the spawn consumes,
    // and the callbacks live for the process.
    let token = unsafe {
        crate::actor::hew_actor_spawn_native(
            crate::mem::buf_alloc(1),
            1,
            dispatch,
            release_state,
            clone_state,
            None,
            capacity,
            crate::internal::types::HewOverflowPolicy::DropNew as i32,
            0,
            ptr::null(),
            0,
            None,
            None,
            &raw mut fault,
            None,
            0,
            None,
            ptr::null_mut(),
        )
    };
    assert!(fault.is_null(), "test actor spawn faulted");
    crate::lifetime::local_handles::resolve_current_actor(token)
        .and_then(crate::lifetime::live_actors::get_actor_ptr_by_id)
        .unwrap_or(ptr::null_mut())
}

/// One `u32` request the test codec encodes.
const TEST_U32_REQUEST: *const c_void = std::ptr::from_ref(&7_u32).cast();

/// Register the `u32` request codec for message 1 under `dispatch`'s key,
/// with a reply of `reply_size` bytes (zero for a unit member).
fn inbound_test_codec(
    dispatch: crate::internal::types::HewDispatchFn,
    reply_size: usize,
) -> *const c_void {
    let key = dispatch_key(dispatch);
    // SAFETY: the thunks match the codec ABI.
    unsafe {
        crate::xnode_serial::hew_xnode_register_codec(
            key,
            1,
            test_u32_serialize,
            test_u32_deserialize,
            Some(test_u32_drop),
            std::mem::size_of::<u32>(),
        );
        crate::xnode_serial::hew_xnode_register_reply_codec(
            key,
            1,
            test_u32_serialize,
            test_u32_deserialize,
            Some(test_u32_drop),
            reply_size,
        );
    }
    key
}

/// Register the `u32` request codec for `msg_type` under `dispatch`'s key.
fn inbound_test_request_codec(
    dispatch: crate::internal::types::HewDispatchFn,
    msg_type: i32,
) -> *const c_void {
    let key = dispatch_key(dispatch);
    // SAFETY: the thunks match the codec ABI.
    unsafe {
        crate::xnode_serial::hew_xnode_register_codec(
            key,
            msg_type,
            test_u32_serialize,
            test_u32_deserialize,
            Some(test_u32_drop),
            std::mem::size_of::<u32>(),
        );
    }
    key
}

/// Drive one coroutine remote call to its outcome on this thread: the
/// taken reply bytes and the `AskError` tag.
unsafe fn ask_for_test(
    target: *const HewRemotePid,
    dispatch: *const c_void,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: u64,
    reply_size: usize,
) -> (Vec<u8>, i32) {
    let (readiness, waker) = crate::wake::blocking::Readiness::new();
    // SAFETY: the caller's request satisfies the call; the waker is live.
    let call = unsafe {
        hew_remote_call_new(
            target,
            dispatch,
            msg_type,
            data,
            size,
            reply_size,
            timeout_ms,
            waker.descriptor(),
        )
    };
    // SAFETY: this thread exclusively drives the live call.
    while unsafe { hew_remote_call_poll(call) } == -1 {
        readiness.wait();
    }
    let mut reply = vec![0_u8; reply_size];
    // SAFETY: the call is ready and `reply` holds its reply size.
    let status = unsafe { hew_remote_call_take(call, reply.as_mut_ptr().cast()) };
    // SAFETY: the call is released once after its take.
    unsafe { hew_remote_call_free(call) };
    (reply, status)
}

/// A `u32` owns nothing, so releasing it is a no-op.
unsafe extern "C" fn test_u32_drop(_value: *mut c_void) {}

/// Register the test u32 codec for `(dispatch, msg_type)` as both the
/// request and reply codec, so the send/ask paths can serialize their
/// controlled payloads. `dispatch` MUST be the dispatch fn the target echo
/// actor is spawned with (the inbound decode resolves it from the actor).
/// Idempotent across helper processes (each registers in its own process).
fn register_test_u32_codec(dispatch: *const c_void, msg_type: i32) {
    // SAFETY: the thunks match the codec ABI.
    unsafe {
        crate::xnode_serial::hew_xnode_register_codec(
            dispatch,
            msg_type,
            test_u32_serialize,
            test_u32_deserialize,
            Some(test_u32_drop),
            std::mem::size_of::<u32>(),
        );
        crate::xnode_serial::hew_xnode_register_reply_codec(
            dispatch,
            msg_type,
            test_u32_serialize,
            test_u32_deserialize,
            Some(test_u32_drop),
            std::mem::size_of::<u32>(),
        );
    }
}

struct ResetCurrentNode(usize);

impl Drop for ResetCurrentNode {
    fn drop(&mut self) {
        with_current_node(|current| {
            *current = self.0;
        });
    }
}

struct TestNode(*mut HewNode);

impl TestNode {
    unsafe fn new(node_id: u16, bind_addr: &CString) -> Self {
        // SAFETY: Caller guarantees bind_addr is a valid C string.
        let node = unsafe { hew_node_new(node_id, bind_addr.as_ptr()) };
        #[cfg(feature = "encryption")]
        if !node.is_null() {
            let dir = tempfile::tempdir().expect("test node identity directory");
            let path = dir.path().join("node.key");
            let identity = crate::encryption::noise_identity_load_or_create(&path)
                .expect("mint test node identity");
            let mut config = crate::peer_binding::PeerAuthConfig::default();
            config.local_route_slot = std::num::NonZeroU16::new(node_id);
            config.node_identity = Some(crate::node_identity::NodeId::from_noise_static_key(
                &identity.public(),
            ));
            config.identity_path = Some(path);
            config.noise_identity = Some(identity);
            let snapshot = config
                .snapshot_for_start()
                .expect("acquire test node session");
            // SAFETY: the node was just allocated and is still stopped.
            assert_eq!(unsafe { hew_node_set_auth_snapshot(node, snapshot) }, 0);
        }
        Self(node)
    }

    fn as_ptr(&self) -> *mut HewNode {
        self.0
    }
}

impl Drop for TestNode {
    fn drop(&mut self) {
        if !self.0.is_null() {
            // SAFETY: TestNode owns the pointer returned by hew_node_new.
            unsafe { hew_node_free(self.0) };
            self.0 = ptr::null_mut();
        }
    }
}

#[cfg(feature = "encryption")]
struct PublicApiTestIdentity {
    dir: tempfile::TempDir,
    saved_transport: Option<std::ffi::OsString>,
}

#[cfg(feature = "encryption")]
impl Drop for PublicApiTestIdentity {
    fn drop(&mut self) {
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: ENV_LOCK serializes process-global environment mutation.
            unsafe {
                if let Some(value) = &self.saved_transport {
                    std::env::set_var("HEW_TRANSPORT", value);
                } else {
                    std::env::remove_var("HEW_TRANSPORT");
                }
            }
        });
    }
}

/// Stage a real stable TCP identity for a public `Node::start` test and keep
/// its tempfile-backed key path alive until the public node is shut down.
#[cfg(feature = "encryption")]
fn stage_public_api_test_identity() -> PublicApiTestIdentity {
    let saved_transport = crate::env::ENV_LOCK.read_access(|()| std::env::var_os("HEW_TRANSPORT"));
    let identity = PublicApiTestIdentity {
        dir: tempfile::tempdir().expect("public API identity directory"),
        saved_transport,
    };
    let tcp = CString::new("tcp").expect("valid transport name");
    // SAFETY: tcp is a valid C string for this call.
    assert_eq!(unsafe { hew_node_api_set_transport(tcp.as_ptr()) }, 0);

    let key = identity.dir.path().join("node.key");
    let key = CString::new(key.to_str().expect("UTF-8 tempfile key path"))
        .expect("valid tempfile key path");
    // SAFETY: key is a valid C string and the directory remains live in the guard.
    assert_eq!(unsafe { hew_node_api_load_keys(key.as_ptr()) }, 0);

    identity
}

fn start_tcp_test_listener_node(node_id: u16) -> (TestNode, u16) {
    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this helper.
    let node = unsafe { TestNode::new(node_id, &bind_addr) };
    assert!(!node.as_ptr().is_null(), "test node allocation failed");
    // SAFETY: the node pointer came from TestNode::new and stays valid until drop.
    let rc = unsafe { hew_node_start(node.as_ptr()) };
    assert_eq!(
        rc,
        0,
        "hew_node_start({node_id}) failed: {:?}",
        crate::stream_error::take_last_error()
    );
    // SAFETY: the node was started successfully and uses the default TCP transport in these tests.
    let port =
        unsafe { crate::transport::hew_transport_tcp_bound_port((*node.as_ptr()).transport) }
            .expect("started TCP test node must expose its bound listener port");
    (node, port)
}

/// Environment key naming the pre-generated Noise keyfile a two-process
/// helper loads its stable identity from (brokered by the parent test).
#[cfg(feature = "encryption")]
const TWO_PROCESS_KEYFILE_ENV: &str = "HEW_2P_KEYFILE";

/// Environment key carrying the peer's Noise static pubkey (lowercase hex)
/// for the helper to bind via its per-node snapshot before connecting.
#[cfg(feature = "encryption")]
const TWO_PROCESS_PEER_PUBKEY_ENV: &str = "HEW_2P_PEER_PUBKEY";

/// Start a **credentialed, Strict-authorized** TCP-Noise listener node for a
/// two-process helper (issue #2652, D110).
///
/// Reads the pre-generated Noise keyfile (`HEW_2P_KEYFILE`) and the peer's
/// Noise static pubkey (`HEW_2P_PEER_PUBKEY`, lowercase hex) — the parent
/// test brokered both out-of-band, mirroring a real key exchange. Installs a
/// per-node snapshot with the stable Noise identity + a
/// `peer_node → NoiseKey(peer_pub)` binding, so the TCP-Noise handshake
/// authenticates the peer and the claim machine binds its `NodeId`. This is a
/// genuine authorized connection — there is no test-only posture promotion:
/// a peer presenting an unbound Noise key fails the pre-gate and admission.
#[cfg(feature = "encryption")]
fn start_authorized_tcp_node(
    node_id: u16,
    peer_node: u16,
) -> (TestNode, u16, crate::node_identity::NodeId) {
    use crate::peer_binding::{PeerAuthConfig, NOISE_KEY_LEN};

    let keyfile = std::env::var(TWO_PROCESS_KEYFILE_ENV).expect("2p keyfile env");
    let peer_pub_hex = std::env::var(TWO_PROCESS_PEER_PUBKEY_ENV).expect("2p peer pubkey env");

    let identity = crate::encryption::noise_identity_load_or_create(std::path::Path::new(&keyfile))
        .expect("load 2p noise identity");
    let peer_pub_bytes = decode_hex(&peer_pub_hex).expect("2p peer pubkey must be hex");
    assert_eq!(
        peer_pub_bytes.len(),
        NOISE_KEY_LEN,
        "2p peer pubkey must be a 32-byte Noise static key"
    );
    let mut peer_pub = [0u8; NOISE_KEY_LEN];
    peer_pub.copy_from_slice(&peer_pub_bytes);
    let peer_identity = crate::node_identity::NodeId::from_noise_static_key(&peer_pub);

    let mut config = PeerAuthConfig::default();
    config.local_route_slot = std::num::NonZeroU16::new(node_id);
    config.node_identity = Some(crate::node_identity::NodeId::from_noise_static_key(
        &identity.public(),
    ));
    config.identity_path = Some(std::path::PathBuf::from(&keyfile));
    config.noise_identity = Some(identity);
    config
        .pin_peer(peer_node, PeerCredential::NoiseKey(peer_pub))
        .expect("distinct two-process peer pin");
    let snapshot = config.snapshot_for_start().expect("acquire 2p session");

    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this helper.
    let node = unsafe { TestNode::new(node_id, &bind_addr) };
    assert!(!node.as_ptr().is_null(), "authorized tcp node alloc failed");
    // SAFETY: node is freshly created (STOPPED); install the snapshot before start.
    let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
    assert_eq!(set_rc, 0, "install 2p auth snapshot on node {node_id}");
    // SAFETY: node pointer is valid; start selects TCP from the snapshot and
    // reads the installed strict bindings.
    let rc = unsafe { hew_node_start(node.as_ptr()) };
    assert_eq!(
        rc,
        0,
        "authorized tcp start({node_id}) failed: {:?}",
        crate::stream_error::take_last_error()
    );
    // SAFETY: node started successfully on the TCP transport.
    let port =
        unsafe { crate::transport::hew_transport_tcp_bound_port((*node.as_ptr()).transport) }
            .expect("authorized tcp node must expose its bound listener port");
    (node, port, peer_identity)
}

/// Start two mutually-authenticated in-process nodes on the native TCP
/// (Noise) transport. Each mints a stable Noise identity and pins the
/// other's real static public key to the peer's `NodeId`, so the loopback
/// handshake admits `Strict` with a published claim (issue #2652 —
/// `posture_for` returns `Strict` whenever bindings exist, regardless of
/// loopback). This is a genuine credentialed harness, never a test-only
/// posture promotion; it mirrors [`start_authorized_quic_mesh_pair`] for the
/// cases that must exercise TCP-specific pending-ask / connection behaviour
/// now that an unverified outbound ask fails closed before it is ever sent.
#[cfg(feature = "encryption")]
fn start_authorized_tcp_pair(id_a: u16, id_b: u16) -> (TestNode, u16, TestNode, u16) {
    use crate::peer_binding::{PeerAuthConfig, StableNoiseIdentity, NOISE_KEY_LEN};

    let dir = tempfile::tempdir().expect("authorized tcp pair keydir");
    let path_a = dir.path().join("node-a.key");
    let path_b = dir.path().join("node-b.key");
    let identity_a = crate::encryption::noise_identity_load_or_create(&path_a)
        .expect("mint node-a noise identity");
    let identity_b = crate::encryption::noise_identity_load_or_create(&path_b)
        .expect("mint node-b noise identity");
    let pub_a = identity_a.public();
    let pub_b = identity_b.public();

    let start_one = |node_id: u16,
                     peer_id: u16,
                     identity: StableNoiseIdentity,
                     identity_path: &std::path::Path,
                     peer_pub: [u8; NOISE_KEY_LEN]|
     -> (TestNode, u16) {
        let mut config = PeerAuthConfig::default();
        config.local_route_slot = std::num::NonZeroU16::new(node_id);
        config.node_identity = Some(crate::node_identity::NodeId::from_noise_static_key(
            &identity.public(),
        ));
        config.identity_path = Some(identity_path.to_path_buf());
        config.noise_identity = Some(identity);
        config
            .pin_peer(peer_id, PeerCredential::NoiseKey(peer_pub))
            .expect("distinct authorized TCP peer pin");
        let snapshot = config
            .snapshot_for_start()
            .expect("acquire authorized TCP session");
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this closure.
        let node = unsafe { TestNode::new(node_id, &bind_addr) };
        assert!(
            !node.as_ptr().is_null(),
            "authorized tcp node {node_id} alloc failed"
        );
        // SAFETY: node is freshly created (STOPPED); install before start.
        let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
        assert_eq!(set_rc, 0, "install auth snapshot on node {node_id}");
        // SAFETY: node pointer is valid; start selects TCP + Noise from the
        // snapshot and reads the installed strict bindings.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(
            rc,
            0,
            "authorized tcp start({node_id}) failed: {:?}",
            crate::stream_error::take_last_error()
        );
        // SAFETY: node started successfully on the TCP transport.
        let port =
            unsafe { crate::transport::hew_transport_tcp_bound_port((*node.as_ptr()).transport) }
                .expect("authorized tcp node must expose its bound listener port");
        (node, port)
    };

    let (node_a, port_a) = start_one(id_a, id_b, identity_a, &path_a, pub_b);
    let (node_b, port_b) = start_one(id_b, id_a, identity_b, &path_b, pub_a);
    (node_a, port_a, node_b, port_b)
}

const TWO_PROCESS_REGISTRY_SERVER_NODE: u16 = 620;

const TWO_PROCESS_REGISTRY_CLIENT_NODE: u16 = 621;

const TWO_PROCESS_REGISTRY_MSG_TYPE: i32 = 123;

const TWO_PROCESS_REGISTRY_NAME: &str = "two-process-registry-worker";

const TWO_PROCESS_ASK_ECHO_NAME: &str = "two-process-ask-echo-worker";

const TWO_PROCESS_ASK_TIMEOUT_NAME: &str = "two-process-ask-timeout-worker";

const TWO_PROCESS_ASK_DEAD_NAME: &str = "two-process-ask-dead-worker";

const TWO_PROCESS_HELPER_ENV: &str = "HEW_REGISTRY_GOSSIP_HELPER";

const TWO_PROCESS_READY_FILE_ENV: &str = "HEW_REGISTRY_GOSSIP_READY_FILE";

const TWO_PROCESS_SERVER_PORT_ENV: &str = "HEW_REGISTRY_GOSSIP_SERVER_PORT";

/// Client writes this file once it has resolved the dead-actor test's
/// registered pid over gossip, so the server knows it is safe to free the
/// actor: `hew_actor_free` unregisters the name and emits a gossip-remove,
/// which would make a too-early free leave the lookup unresolved forever
/// instead of exercising a genuinely dead-actor ask.
const TWO_PROCESS_RESOLVED_FILE_ENV: &str = "HEW_REGISTRY_GOSSIP_RESOLVED_FILE";

/// Client writes this file once its ask has resolved, so the server knows
/// it is safe to stop its node: stopping (and dropping the connection)
/// before a slow-but-successful round trip arrives would surface
/// `ConnectionDropped` instead of the rejection reason under test.
const TWO_PROCESS_DONE_FILE_ENV: &str = "HEW_REGISTRY_GOSSIP_DONE_FILE";

/// Server writes this file once `hew_actor_free` has fully returned, so
/// the client sends its ask only after the target is unreachably dead.
/// Without this barrier, an ask that lands while the actor is mid-`stop`
/// (submitted just after `hew_actor_stop` but before `hew_actor_free`
/// untracks it) can race the mailbox teardown's orphaned-ask completion
/// and hang rather than resolve — a distinct, pre-existing mailbox-
/// teardown race, not what this fix addresses. Serializing the ask after
/// a *complete* free reproduces the reported dead-actor symptom without
/// that unrelated race's flakiness.
const TWO_PROCESS_FREED_FILE_ENV: &str = "HEW_REGISTRY_GOSSIP_FREED_FILE";

static TWO_PROCESS_REGISTRY_DELIVERY: (Mutex<bool>, Condvar) = (Mutex::new(false), Condvar::new());

static TWO_PROCESS_ASK_OBSERVED: (Mutex<bool>, Condvar) = (Mutex::new(false), Condvar::new());

struct ManagedChild {
    name: &'static str,
    child: Option<Child>,
}

impl ManagedChild {
    fn new(name: &'static str, child: Child) -> Self {
        Self {
            name,
            child: Some(child),
        }
    }

    fn try_wait(&mut self) -> Option<std::process::ExitStatus> {
        self.child
            .as_mut()
            .expect("child already waited")
            .try_wait()
            .expect("child try_wait failed")
    }

    fn wait_output(&mut self, timeout: Duration) -> Output {
        let child = self.child.take().expect("child already waited");
        wait_child_output(self.name, child, timeout)
    }
}

impl Drop for ManagedChild {
    fn drop(&mut self) {
        let Some(mut child) = self.child.take() else {
            return;
        };
        if child.try_wait().ok().flatten().is_none() {
            let _ = child.kill();
        }
        let _ = child.wait();
    }
}

fn wait_child_output(name: &str, mut child: Child, timeout: Duration) -> Output {
    let deadline = Instant::now() + timeout;
    loop {
        if child.try_wait().expect("child try_wait failed").is_some() {
            return child.wait_with_output().expect("child output failed");
        }
        if Instant::now() >= deadline {
            let pid = child.id();
            let _ = child.kill();
            let output = child.wait_with_output().expect("timed-out child output");
            panic!(
                "{name} helper process {pid} timed out\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
        }
        thread::sleep(Duration::from_millis(20));
    }
}

fn assert_child_success(name: &str, output: &Output) {
    assert!(
        output.status.success(),
        "{name} helper failed with status {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn spawn_registry_gossip_helper(
    helper_name: &'static str,
    role: &'static str,
    envs: &[(&str, String)],
) -> ManagedChild {
    let mut command = Command::new(std::env::current_exe().expect("current test binary"));
    command
        .args(["--exact", helper_name, "--nocapture"])
        .env("RUST_TEST_THREADS", "1")
        .env(TWO_PROCESS_HELPER_ENV, role)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    for (key, value) in envs {
        command.env(key, value);
    }
    ManagedChild::new(role, command.spawn().expect("spawn helper process"))
}

fn wait_for_ready_port(
    ready_file: &std::path::Path,
    server: &mut ManagedChild,
    timeout: Duration,
) -> u16 {
    let deadline = Instant::now() + timeout;
    loop {
        if let Ok(text) = std::fs::read_to_string(ready_file) {
            if let Ok(port) = text.trim().parse::<u16>() {
                return port;
            }
        }
        if server.try_wait().is_some() {
            let output = server.wait_output(Duration::from_secs(1));
            panic!(
                "server exited before writing readiness file\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
        }
        assert!(
            Instant::now() < deadline,
            "server did not write readiness file before timeout"
        );
        thread::sleep(Duration::from_millis(20));
    }
}

fn wait_for_single_connection(node: *mut HewNode, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    loop {
        // SAFETY: caller passes a live node pointer for this bounded wait.
        if unsafe { connection::hew_connmgr_count((*node).conn_mgr) > 0 } {
            return true;
        }
        if Instant::now() >= deadline {
            return false;
        }
        thread::sleep(Duration::from_millis(20));
    }
}

fn wait_for_remote_lookup(
    node: *mut HewNode,
    name: *const c_char,
    expected_node_id: crate::node_identity::NodeId,
    timeout: Duration,
) -> Option<HewRemotePid> {
    let deadline = Instant::now() + timeout;
    loop {
        // SAFETY: node/name are valid for this bounded wait.
        let location = unsafe { lookup_exact(node, name) };
        if let Some(location) = location {
            if location.node() == expected_node_id {
                return Some(HewRemotePid::from(location));
            }
        }
        if Instant::now() >= deadline {
            return None;
        }
        thread::sleep(Duration::from_millis(20));
    }
}

fn reset_two_process_delivery() {
    let mut delivered = TWO_PROCESS_REGISTRY_DELIVERY
        .0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *delivered = false;
}

fn wait_for_two_process_delivery(timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    let mut delivered = TWO_PROCESS_REGISTRY_DELIVERY
        .0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    while !*delivered {
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return false;
        }
        let (guard, result) = TWO_PROCESS_REGISTRY_DELIVERY
            .1
            .wait_timeout(delivered, remaining)
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        delivered = guard;
        if result.timed_out() && !*delivered {
            return false;
        }
    }
    true
}

fn reset_two_process_ask_observed() {
    let mut observed = TWO_PROCESS_ASK_OBSERVED
        .0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *observed = false;
}

fn mark_two_process_ask_observed() {
    let mut observed = TWO_PROCESS_ASK_OBSERVED
        .0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *observed = true;
    TWO_PROCESS_ASK_OBSERVED.1.notify_all();
}

fn wait_for_two_process_ask_observed(timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    let mut observed = TWO_PROCESS_ASK_OBSERVED
        .0
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    while !*observed {
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return false;
        }
        let (guard, result) = TWO_PROCESS_ASK_OBSERVED
            .1
            .wait_timeout(observed, remaining)
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        observed = guard;
        if result.timed_out() && !*observed {
            return false;
        }
    }
    true
}

unsafe extern "C-unwind" fn two_process_registry_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if msg_type == TWO_PROCESS_REGISTRY_MSG_TYPE {
        let mut delivered = TWO_PROCESS_REGISTRY_DELIVERY
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *delivered = true;
        TWO_PROCESS_REGISTRY_DELIVERY.1.notify_all();
    }

    std::ptr::null_mut()
}

/// Start a node whose transport has been pre-allocated as a `quic_mesh`
/// transport with the supplied [`MeshTls`] config. The override path
/// bypasses `HEW_TRANSPORT`'s default self-signed allowlist so that two
/// in-process nodes can mutually pin each other's SPKIs.
///
/// Returns the [`TestNode`] handle and the bound UDP port.
///
/// Mirrors [`start_tcp_test_listener_node`] but for the native `quic_mesh`
/// transport. Used by the cross-node `quic_mesh` integration tests below.
#[cfg(feature = "quic")]
fn start_quic_mesh_test_listener_node(
    node_id: u16,
    tls: crate::quic_mesh::MeshTls,
) -> (TestNode, u16) {
    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this helper.
    let node = unsafe { TestNode::new(node_id, &bind_addr) };
    assert!(!node.as_ptr().is_null(), "test node allocation failed");

    // SAFETY: hew_transport_quic_mesh_new returns an owned transport
    // pointer (or null on runtime build failure).
    let transport = unsafe { crate::quic_mesh::hew_transport_quic_mesh_new() };
    assert!(
        !transport.is_null(),
        "quic_mesh transport allocation failed: {:?}",
        crate::stream_error::take_last_error()
    );
    // SAFETY: transport pointer was just allocated by the constructor.
    let rc = unsafe { crate::quic_mesh::hew_transport_quic_mesh_set_tls_override(transport, tls) };
    assert_eq!(rc, 0, "set TLS override on quic_mesh transport");

    // SAFETY: node owns the previously null transport slot; we replace it
    // with the pre-allocated quic_mesh transport before start.
    unsafe {
        (*node.as_ptr()).transport = transport;
    }

    // SAFETY: node and transport pointers are valid; start consumes the
    // injected transport instead of selecting from HEW_TRANSPORT.
    let rc = unsafe { hew_node_start(node.as_ptr()) };
    assert_eq!(
        rc,
        0,
        "hew_node_start({node_id}) on quic_mesh failed: {:?}",
        crate::stream_error::take_last_error()
    );
    // SAFETY: node started successfully and is using the quic_mesh transport.
    let port =
        unsafe { crate::quic_mesh::hew_transport_quic_mesh_bound_port((*node.as_ptr()).transport) }
            .expect("started quic_mesh test node must expose its bound listener port");
    (node, port)
}

/// Start a **credentialed, Strict-authorized** quic-mesh listener node
/// (issue #2652, D110).
///
/// Unlike [`start_quic_mesh_test_listener_node`] (which leaves `node.auth`
/// unconfigured → `Unverified` posture, delivery-only), this installs a real
/// per-node [`PeerAuthSnapshot`] before start:
///  - `node_id = Some(node_id)` — the operator-pinned identity;
///  - `bindings = {peer_id → Spki(peer_spki)}` — the peer's *actual* leaf
///    SPKI (from [`make_mutually_pinned_mesh_tls`]), so the claim machine
///    binds the claimed `NodeId` to the authenticated key.
///
/// The mutually-pinned `tls` (which already carries `with_peer_spki`) drives
/// the mTLS handshake; admission extracts the *presented* leaf SPKI and
/// matches it against the binding. This is a genuine authorized connection —
/// there is no test-only posture promotion (D110): a peer presenting a
/// different cert fails both the TLS pin and the claim-machine binding.
#[cfg(feature = "quic")]
fn start_authorized_quic_mesh_node(
    node_id: u16,
    tls: crate::quic_mesh::MeshTls,
    local_spki: Vec<u8>,
    peer_id: u16,
    peer_spki: Vec<u8>,
) -> (TestNode, u16) {
    use crate::peer_binding::PeerAuthConfig;

    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this helper.
    let node = unsafe { TestNode::new(node_id, &bind_addr) };
    assert!(!node.as_ptr().is_null(), "test node allocation failed");

    // Install the credentialed snapshot: local route slot plus the peer's
    // real SPKI pinned to its receiver-local route slot.
    let mut config = PeerAuthConfig::default();
    config.local_route_slot = std::num::NonZeroU16::new(node_id);
    config.transport = Some(crate::peer_binding::TransportSelection::QuicMesh);
    config.node_identity = Some(crate::node_identity::NodeId::from_spki(&local_spki));
    let identity_dir = tempfile::tempdir().expect("authorized mesh identity dir");
    config.identity_path = Some(identity_dir.path().join("node.pem"));
    config.mesh_identity = Some(crate::peer_binding::MeshIdentityMaterial::from_der(
        tls.cert_chain
            .iter()
            .map(|certificate| certificate.as_ref().to_vec())
            .collect(),
        tls.private_key_pkcs8.clone(),
        local_spki,
    ));
    config
        .pin_peer(peer_id, PeerCredential::Spki(peer_spki))
        .expect("distinct authorized mesh peer pin");
    let snapshot = config
        .snapshot_for_start()
        .expect("acquire authorized mesh session");
    // SAFETY: node is freshly created (STOPPED); installing a snapshot is valid.
    let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
    assert_eq!(set_rc, 0, "install auth snapshot on node {node_id}");

    // SAFETY: hew_transport_quic_mesh_new returns an owned transport pointer.
    let transport = unsafe { crate::quic_mesh::hew_transport_quic_mesh_new() };
    assert!(
        !transport.is_null(),
        "quic_mesh transport allocation failed: {:?}",
        crate::stream_error::take_last_error()
    );
    // SAFETY: transport pointer was just allocated by the constructor.
    let rc = unsafe { crate::quic_mesh::hew_transport_quic_mesh_set_tls_override(transport, tls) };
    assert_eq!(rc, 0, "set TLS override on quic_mesh transport");

    // SAFETY: node owns the previously null transport slot; replace it with
    // the pre-allocated quic_mesh transport before start.
    unsafe {
        (*node.as_ptr()).transport = transport;
    }

    // SAFETY: node and transport pointers are valid; start consumes the
    // injected transport and reads the installed strict snapshot.
    let rc = unsafe { hew_node_start(node.as_ptr()) };
    assert_eq!(
        rc,
        0,
        "hew_node_start({node_id}) authorized quic_mesh failed: {:?}",
        crate::stream_error::take_last_error()
    );
    // SAFETY: node started successfully on the quic_mesh transport.
    let port =
        unsafe { crate::quic_mesh::hew_transport_quic_mesh_bound_port((*node.as_ptr()).transport) }
            .expect("started quic_mesh test node must expose its bound listener port");
    (node, port)
}

/// Start a mutually-authorized quic-mesh node **pair** (issue #2652, D110).
///
/// Returns `(node_a, port_a, node_b, port_b)`. `node_a` is started first (so
/// it becomes `CURRENT_NODE` / initiator); both nodes carry cross-bound
/// `Spki → NodeId` credentials so a connection between them admits Strict.
#[cfg(feature = "quic")]
fn start_authorized_quic_mesh_pair(id_a: u16, id_b: u16) -> (TestNode, u16, TestNode, u16) {
    let (tls_a, tls_b, spki_a, spki_b) =
        make_mutually_pinned_mesh_tls(&format!("node-{id_a}"), &format!("node-{id_b}"));
    let (node_a, port_a) =
        start_authorized_quic_mesh_node(id_a, tls_a, spki_a.clone(), id_b, spki_b.clone());
    let (node_b, port_b) = start_authorized_quic_mesh_node(id_b, tls_b, spki_b, id_a, spki_a);
    (node_a, port_a, node_b, port_b)
}

unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    std::ptr::null_mut()
}

/// A coroutine remote call registered on `key`, with a deadline far
/// beyond any test and a readiness latch standing in for its coroutine.
fn remote_call_for_test(
    key: ConnectionKey,
) -> (
    u64,
    Arc<crate::wake::blocking::Readiness>,
    *mut HewRemoteCall,
) {
    let (readiness, waker) = crate::wake::blocking::Readiness::new();
    // SAFETY: the waker descriptor is live for the call.
    let timer = unsafe { crate::coro_sleep::hew_coro_sleep_new(i64::MAX, waker.descriptor()) };
    let (id, pending) = reply_table().register_with_waker(key, Some(waker), PendingReplyKind::Ask);
    let call = Box::into_raw(Box::new(HewRemoteCall {
        route: RemoteCallRoute::Wire(pending),
        timer,
        timed_out: false,
        dispatch: test_dispatch(),
        msg_type: 7,
        reply_size: 0,
    }));
    (id, readiness, call)
}

// ── Distributed multi-node integration tests ──────────────────────────
//
// These tests exercise the real TCP transport path end-to-end in a
// single process using two HewNode instances:
//
//  • `two_node_remote_send_delivery` — proves fire-and-forget message
//    delivery from node1 to an actor registered on node2.
//  • `two_node_remote_ask_reply` — proves the full ask/reply round-trip
//    over the real transport: node1 sends an ask, node2's actor replies,
//    and node1 receives the reply.
//
// Both tests run under the shared runtime test lock to ensure this runtime's
// CURRENT_NODE / local_node_id pair is not perturbed by concurrent node tests.

// ── Shared helpers for distributed node tests ─────────────────────────

/// Connect `initiator` to `responder_addr` with retry back-off.
unsafe fn connect_with_retry(initiator: *mut HewNode, responder_addr: &CString) {
    let mut backoff = Duration::from_millis(25);
    for _ in 0..20 {
        // SAFETY: initiator and responder_addr are valid for this call.
        if unsafe { hew_node_connect(initiator, responder_addr.as_ptr()) } == 0 {
            return;
        }
        thread::sleep(backoff);
        backoff = (backoff * 2).min(Duration::from_millis(200));
    }
    let error = {
        let error = crate::hew_last_error();
        if error.is_null() {
            None
        } else {
            // SAFETY: `hew_last_error` returns a live NUL-terminated string.
            Some(
                unsafe { CStr::from_ptr(error) }
                    .to_string_lossy()
                    .into_owned(),
            )
        }
    };
    panic!("could not connect initiator to responder: {error:?}");
}

/// Poll until both connection managers report at least one active connection.
unsafe fn wait_for_handshake(node1: *mut HewNode, node2: *mut HewNode) {
    poll_until(|i| {
        // SAFETY: node1 and node2 pointers are valid for the duration of the test.
        let ready = unsafe {
            let mgr1 = &*(*node1).conn_mgr;
            let mgr2 = &*(*node2).conn_mgr;
            let conn1 =
                connection::hew_connmgr_conn_id_for_node((*node1).conn_mgr, (*node2).route_slot);
            let conn2 =
                connection::hew_connmgr_conn_id_for_node((*node2).conn_mgr, (*node1).route_slot);
            conn1 >= 0
                && conn2 >= 0
                && connection::authenticated_peer_node_id_for_conn(mgr1, conn1)
                    == (*node2).route_slot
                && connection::authenticated_peer_node_id_for_conn(mgr2, conn2)
                    == (*node1).route_slot
        };
        if !ready {
            let ms = if i < 20 {
                25
            } else if i < 50 {
                50
            } else {
                100
            };
            thread::sleep(Duration::from_millis(ms));
        }
        ready
    });
}

/// Poll node `observer`'s SWIM view of `subject` until it reaches at least
/// `min_state`, where membership states are ordered
/// `MEMBER_ALIVE` (0) < `MEMBER_SUSPECT` (1) < `MEMBER_DEAD` (2).
///
/// This synchronises on an asynchronous SWIM transition instead of racing
/// it; the test runner's timeout is the hang guard for a transition that
/// never happens.
unsafe fn wait_for_member_state_at_least(observer: *mut HewNode, subject: u16, min_state: i32) {
    poll_until(|i| {
        // SAFETY: observer's cluster is live for the duration of the test.
        let state =
            unsafe { crate::cluster::hew_cluster_member_state((*observer).cluster, subject) };
        if state >= min_state {
            return true;
        }
        let ms = if i < 40 { 10 } else { 25 };
        thread::sleep(Duration::from_millis(ms));
        false
    });
}

// ── Test: fire-and-forget remote message delivery ─────────────────────

/// Stores the `msg_type` of the most-recently received remote message.
static SEND_PROBE_MSG_TYPE: AtomicU32 = AtomicU32::new(0);

unsafe extern "C-unwind" fn send_probe_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    #[expect(
        clippy::cast_sign_loss,
        reason = "msg_type is a non-negative tag in this test"
    )]
    SEND_PROBE_MSG_TYPE.store(msg_type as u32, Ordering::Release);

    std::ptr::null_mut()
}

// ── Test: fire-and-forget remote message delivery (quic_mesh) ─────────
//
// Mirrors `two_node_remote_send_delivery` but routes the payload over the
// native `quic_mesh` transport (mTLS-pinned per-actor-pair streams) rather
// than TCP. Two in-process nodes mutually pin each other's SPKIs in their
// per-node auth snapshots and inject the matching TLS identities.

/// Stores the `msg_type` of the most-recently received remote message on
/// the `quic_mesh` path.  Separate from `SEND_PROBE_MSG_TYPE` so the two
/// tests can run independently without static-state cross-talk.
#[cfg(feature = "quic")]
static SEND_PROBE_MSG_TYPE_QM: AtomicU32 = AtomicU32::new(0);

#[cfg(feature = "quic")]
unsafe extern "C-unwind" fn send_probe_dispatch_qm(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    #[expect(
        clippy::cast_sign_loss,
        reason = "msg_type is a non-negative tag in this test"
    )]
    SEND_PROBE_MSG_TYPE_QM.store(msg_type as u32, Ordering::Release);

    std::ptr::null_mut()
}

/// Build a mutually-pinned `(MeshTls, MeshTls)` pair so two in-process
/// `quic_mesh` nodes can complete the handshake. Returns
/// `(tls_a, tls_b, spki_a, spki_b)`. The two SPKIs are returned so
/// fail-closed tests can construct asymmetric trust configurations.
#[cfg(feature = "quic")]
fn make_mutually_pinned_mesh_tls(
    sni_a: &str,
    sni_b: &str,
) -> (
    crate::quic_mesh::MeshTls,
    crate::quic_mesh::MeshTls,
    Vec<u8>,
    Vec<u8>,
) {
    use crate::quic_mesh::MeshTls;
    let (tls_a, spki_a) = MeshTls::self_signed(vec![sni_a.into()]).expect("tls_a self_signed");
    let (tls_b, spki_b) = MeshTls::self_signed(vec![sni_b.into()]).expect("tls_b self_signed");
    let tls_a = tls_a.with_peer_spki(spki_b.clone());
    let tls_b = tls_b.with_peer_spki(spki_a.clone());
    (tls_a, tls_b, spki_a, spki_b)
}

// ── Test: remote ask / reply round-trip ───────────────────────────────

/// Echo-double dispatch: reads a u32 from `data`, replies with `value * 2`.
unsafe extern "C-unwind" fn ask_probe_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    data: *mut c_void,
    size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if size < std::mem::size_of::<u32>() {
        return std::ptr::null_mut();
    }
    // SAFETY: data is valid for at least size_of::<u32>() bytes.
    let value = unsafe { *(data.cast::<u32>()) };
    let mut reply_value: u32 = value.wrapping_mul(2);

    let ch = crate::scheduler::hew_get_reply_channel();
    if ch.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: ch is the current thread-local reply channel; reply_value is valid.
    unsafe {
        let _ = crate::reply_channel::hew_reply(
            ch.cast(),
            (&raw mut reply_value).cast::<c_void>(),
            std::mem::size_of::<u32>(),
        );
    }
    // Mark observed only AFTER the reply has been handed to the channel, so
    // the server helper (which tears down on observe) cannot drop the
    // connection before the reply flushes to the asking node.
    mark_two_process_ask_observed();

    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn void_ask_probe_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    let ch = crate::scheduler::hew_get_reply_channel();
    if ch.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: the reply channel comes from the scheduler and is valid for a void reply here.
    unsafe {
        let _ = crate::reply_channel::hew_reply(ch.cast(), ptr::null_mut(), 0);
    }

    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn orphaned_void_ask_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    crate::actor::hew_actor_self_stop();

    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn blocked_ask_probe_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    mark_two_process_ask_observed();

    std::ptr::null_mut()
}

// ── SWIM failure-detection (driven detector) integration tests ──────

/// RAII helper that sets the fast SWIM-timing env vars for a test and
/// removes them on drop, all under `ENV_LOCK` exclusive access.
///
/// The base fast-env budgets (40 ms protocol period, 120 ms suspect
/// timeout) are set as environment variables so `cluster_config_from_env`
/// picks them up at node-start time.  These tests run under simulated time
/// (enabled before any node starts so the SWIM driver picks up the sim
/// clock): the driver never reads real wall time during the test.
struct SwimTimingEnv;

/// Protocol period used by the fast-test SWIM clock (ms).
const SWIM_TEST_PERIOD_MS: u64 = 40;

/// Suspect timeout used by the fast-test SWIM clock (ms).
const SWIM_TEST_SUSPECT_TIMEOUT_MS: u64 = 120;

impl SwimTimingEnv {
    /// Enable simulated time and set fast SWIM timing env vars.
    ///
    /// Simtime MUST be enabled before any node starts so that the SWIM
    /// driver captures the sim clock at spawn time (the choice is made
    /// once in [`crate::swim_driver::start_swim_driver`]).
    fn fast() -> Self {
        // Enable simtime first: SWIM drivers started after this call will
        // use the sim clock.  Start at T=0.
        crate::deterministic::hew_simtime_enable(0);
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: ENV_LOCK provides exclusive write access to the environ.
            unsafe {
                std::env::set_var(
                    "HEW_SWIM_PROTOCOL_PERIOD_MS",
                    SWIM_TEST_PERIOD_MS.to_string(),
                );
                std::env::set_var("HEW_SWIM_PING_TIMEOUT_MS", SWIM_TEST_PERIOD_MS.to_string());
                std::env::set_var(
                    "HEW_SWIM_SUSPECT_TIMEOUT_MS",
                    SWIM_TEST_SUSPECT_TIMEOUT_MS.to_string(),
                );
            }
        });
        Self
    }
}

impl Drop for SwimTimingEnv {
    fn drop(&mut self) {
        // Disable simtime first so any still-running threads switch back to
        // the real clock before the env vars are removed.
        crate::deterministic::hew_simtime_disable();
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: ENV_LOCK provides exclusive access to the environ.
            unsafe {
                std::env::remove_var("HEW_SWIM_PROTOCOL_PERIOD_MS");
                std::env::remove_var("HEW_SWIM_PING_TIMEOUT_MS");
                std::env::remove_var("HEW_SWIM_SUSPECT_TIMEOUT_MS");
            }
        });
    }
}
