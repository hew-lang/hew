//! Native attachment callback lifecycle over actual loopback TCP sockets.
//! Compiler-backed source cases separately prove typed actor message delivery.
#![cfg(not(target_arch = "wasm32"))]

use std::ffi::{c_void, CString};
use std::io::{Read, Write};
use std::net::TcpStream;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

use hew_runtime::actor::{hew_actor_free, hew_actor_spawn_opts, HewActorOpts};
use hew_runtime::transport::{
    hew_tcp_accept, hew_tcp_attach_native, hew_tcp_listen, NativeActorToken,
};

static RECEIVED: OnceLock<Mutex<Vec<u8>>> = OnceLock::new();
static CLOSED: AtomicBool = AtomicBool::new(false);

unsafe extern "C" fn capture_data(_: usize, data: *const u8, len: usize) -> i32 {
    // SAFETY: native attachment callbacks borrow exactly len readable bytes.
    let bytes = unsafe { std::slice::from_raw_parts(data, len) };
    RECEIVED
        .get_or_init(|| Mutex::new(Vec::new()))
        .lock()
        .unwrap()
        .extend_from_slice(bytes);
    0
}

unsafe extern "C" fn capture_close(_: usize, _: *const u8, _: usize) -> i32 {
    CLOSED.store(true, Ordering::Release);
    0
}

unsafe extern "C-unwind" fn idle_dispatch(
    _: *mut hew_runtime::execution_context::HewExecutionContext,
    _: *mut c_void,
    _: i32,
    _: *mut c_void,
    _: usize,
    _: i32,
) -> *mut c_void {
    ptr::null_mut()
}

fn spawn_target() -> *mut hew_runtime::actor::HewActor {
    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(idle_dispatch),
        mailbox_capacity: 0,
        overflow: 0,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        message_drop_fn: None,
        budget: 0,
        arena_cap_bytes: 0,
        cycle_capable: 0,
    };
    // SAFETY: the runtime copies these complete options and owns the actor state.
    let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
    assert!(!actor.is_null());
    actor
}

#[test]
fn native_attachment_delivers_bytes_and_closes_on_refusal_or_actor_free() {
    assert_eq!(hew_runtime::scheduler::hew_sched_init(), 0);
    let address = CString::new("127.0.0.1:0").unwrap();
    // SAFETY: the address is a live NUL-terminated string during listen.
    let listener = unsafe { hew_tcp_listen(address.as_ptr()) };
    assert!(listener > 0);
    let port = hew_runtime::transport::tcp_listener_local_port(listener).unwrap();

    let mut client = TcpStream::connect(("127.0.0.1", port)).unwrap();
    let conn = hew_tcp_accept(listener);
    let actor = spawn_target();
    // SAFETY: the actor and connection are live, and both callbacks obey the ABI.
    assert_eq!(
        // SAFETY: the test owns the live connection and destination.
        unsafe { hew_tcp_attach_native(conn, (*actor).local_pid_id, capture_data, capture_close) },
        0
    );
    client.write_all(b"hello\0native-\xc3\xa9").unwrap();
    drop(client);
    let deadline = Instant::now() + Duration::from_secs(3);
    while !CLOSED.load(Ordering::Acquire) {
        assert!(Instant::now() < deadline, "missing remote close callback");
        std::thread::sleep(Duration::from_millis(5));
    }
    assert_eq!(
        RECEIVED.get().unwrap().lock().unwrap().as_slice(),
        b"hello\0native-\xc3\xa9"
    );
    // SAFETY: this test owns the actor and releases it exactly once.
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);

    let mut client = TcpStream::connect(("127.0.0.1", port)).unwrap();
    let conn = hew_tcp_accept(listener);
    // A refused attachment still consumes its connection and closes the peer.
    // SAFETY: conn is valid; the invalid token is a supported refusal input.
    assert_eq!(
        // SAFETY: the live connection is consumed; invalid tokens are refused.
        unsafe {
            hew_tcp_attach_native(conn, NativeActorToken::INVALID, capture_data, capture_close)
        },
        -1
    );
    client
        .set_read_timeout(Some(Duration::from_secs(2)))
        .unwrap();
    assert_eq!(client.read(&mut [0; 1]).unwrap(), 0);

    // Exercise queued registration and in-flight input while actor teardown
    // removes the attachment. Deterministic registry race tests cover each seam.
    for _ in 0..32 {
        let mut client = TcpStream::connect(("127.0.0.1", port)).unwrap();
        let conn = hew_tcp_accept(listener);
        let actor = spawn_target();
        // SAFETY: the newly spawned actor and connection are live.
        assert_eq!(
            // SAFETY: both the fresh actor and connection are still test-owned.
            unsafe {
                hew_tcp_attach_native(conn, (*actor).local_pid_id, capture_data, capture_close)
            },
            0
        );
        let _ = client.write_all(b"pending");
        // SAFETY: actor free synchronously detaches its exact incarnation.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }
    hew_runtime::transport::hew_tcp_close(listener);
    hew_runtime::scheduler::hew_sched_shutdown();
    hew_runtime::scheduler::hew_runtime_cleanup();
}
