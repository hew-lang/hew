//! End-to-end oracle for the await-suspension network I/O reactor (NEW-1).
//!
//! Proves worker-free `await conn.read()` suspend/resume at the runtime level:
//! a real TCP connection is registered for a suspending read via
//! `hew_conn_await_read`, the calling "handler" then parks (no scheduler worker
//! blocks on the socket), and the reactor — on its OWN thread — reads the
//! peer's bytes and deposits the result into the handler's read slot, exactly
//! as a resumed continuation would observe. The test plays the role of the
//! resumed handler: it polls the slot and takes the bytes the reactor handed
//! off. At no point does this test (or any scheduler worker) call a blocking
//! `read()` on the server socket — the read is performed entirely by the
//! reactor thread, which is the worker-free suspend/resume invariant.
//!
//! Gated to native targets: the reactor needs a platform readiness poller
//! (epoll/kqueue/IOCP) and is fail-closed on WASM. On Windows the reactor is
//! backed by the IOCP + `AFD_POLL` readiness bridge.
#![cfg(not(target_arch = "wasm32"))]
#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI e2e test asserts raw ABI behaviour directly"
)]

use std::ffi::CString;
use std::io::Write;
use std::net::TcpStream;
use std::ptr;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use hew_runtime::actor::{hew_actor_free, hew_actor_spawn_opts, HewActorOpts};
use hew_runtime::read_slot::{
    hew_read_slot_cancel, hew_read_slot_free, hew_read_slot_new, hew_read_slot_status,
    hew_read_slot_take, ReadStatus,
};
use hew_runtime::scheduler::hew_sched_init;
use hew_runtime::transport::{hew_conn_await_read, hew_tcp_accept, hew_tcp_listen};

// Serialises tests that drive the process-wide scheduler + reactor.
static E2E_MUTEX: Mutex<()> = Mutex::new(());

fn spawn_idle_actor() -> *mut hew_runtime::actor::HewActor {
    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: None,
        mailbox_capacity: 0,
        overflow: 0,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        budget: 0,
        arena_cap_bytes: 0,
        cycle_capable: 0,
    };
    unsafe { hew_actor_spawn_opts(&raw const opts) }
}

/// Listen on an ephemeral loopback port, connect a client, and accept the server
/// side. Returns `(server_conn_handle, client_stream)`.
fn loopback_pair() -> (i32, TcpStream) {
    let addr = CString::new("127.0.0.1:0").unwrap();
    let listener = unsafe { hew_tcp_listen(addr.as_ptr()) };
    assert!(listener > 0, "listen should succeed");
    let port = hew_runtime::transport::tcp_listener_local_port(listener).expect("bound port");
    let client = TcpStream::connect(("127.0.0.1", port)).expect("client connect");
    let server_conn = hew_tcp_accept(listener);
    assert!(server_conn > 0, "accept should succeed");
    (server_conn, client)
}

fn poll_status_until(slot: *mut hew_runtime::read_slot::HewReadSlot, want: ReadStatus, what: &str) {
    let deadline = Instant::now() + Duration::from_secs(3);
    loop {
        let status = unsafe { hew_read_slot_status(slot) };
        if status == want as i32 {
            return;
        }
        assert!(
            Instant::now() < deadline,
            "{what}: slot status did not reach {want:?} within 3s (last = {status})"
        );
        std::thread::sleep(Duration::from_millis(10));
    }
}

// The scheduler is a process-wide init-once singleton, so all scenarios run
// under a single test that inits it once and never tears it down.
#[test]
fn await_suspend_io_end_to_end() {
    let _guard = E2E_MUTEX
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert_eq!(hew_sched_init(), 0, "scheduler init");

    scenario_reactor_deposits_read_bytes_worker_free();
    scenario_peer_close_deposits_eof();
    scenario_cancelled_await_drops_late_readiness();
}

/// The core worker-free suspend/resume proof: register a suspending read, write
/// from the peer, and observe the reactor deposit the exact bytes into the read
/// slot — with no worker ever performing a blocking socket read.
fn scenario_reactor_deposits_read_bytes_worker_free() {
    let (server_conn, mut client) = loopback_pair();
    let actor = spawn_idle_actor();
    assert!(!actor.is_null(), "spawn idle actor");

    // The "suspending handler" creates a read slot (held across its suspend) and
    // registers the await; on success the reactor takes its own slot ref.
    let slot = hew_read_slot_new();
    let rc = unsafe { hew_conn_await_read(server_conn, actor, slot) };
    assert_eq!(
        rc, 0,
        "hew_conn_await_read should register the suspending read"
    );

    // Peer writes. The reactor (its own thread) reads + deposits into the slot.
    // This test thread NEVER reads the server socket.
    client
        .write_all(b"await-suspend-bytes")
        .expect("client write");
    client.flush().ok();

    poll_status_until(slot, ReadStatus::Data, "read deposit");

    // Play the resumed continuation: take the bytes the reactor handed off.
    let triple = unsafe { hew_read_slot_take(slot) };
    assert!(
        !triple.ptr.is_null() && triple.len > 0,
        "deposited bytes present"
    );
    let bytes = unsafe {
        std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
    };
    assert_eq!(
        bytes, b"await-suspend-bytes",
        "the reactor must deposit the exact peer-written bytes"
    );
    unsafe { hew_runtime::bytes::hew_bytes_drop(triple.ptr) };

    // Release the creator ref (one-shot registration already dropped the reactor
    // ref on deposit), then tear down the actor.
    unsafe { hew_read_slot_free(slot) };
    drop(client);
    let _ = unsafe { hew_actor_free(actor) };
}

/// A suspending read whose peer closes with no buffered bytes resumes with an
/// EOF status (the empty-`bytes` resume edge), driven worker-free by the reactor.
fn scenario_peer_close_deposits_eof() {
    let (server_conn, client) = loopback_pair();
    let actor = spawn_idle_actor();
    assert!(!actor.is_null());

    let slot = hew_read_slot_new();
    let rc = unsafe { hew_conn_await_read(server_conn, actor, slot) };
    assert_eq!(rc, 0);

    // Peer closes without writing → the reactor reads EOF and deposits Eof.
    drop(client);

    poll_status_until(slot, ReadStatus::Eof, "eof deposit");

    unsafe { hew_read_slot_free(slot) };
    let _ = unsafe { hew_actor_free(actor) };
}

/// A cancelled await must not receive a deposit: after the handler abandons the
/// read (`hew_read_slot_cancel`), a late peer write is dropped by the reactor
/// (cancelled-flag check) — the slot stays Cancelled, never Data.
fn scenario_cancelled_await_drops_late_readiness() {
    let (server_conn, mut client) = loopback_pair();
    let actor = spawn_idle_actor();
    assert!(!actor.is_null());

    let slot = hew_read_slot_new();
    let rc = unsafe { hew_conn_await_read(server_conn, actor, slot) };
    assert_eq!(rc, 0);

    // The handler abandons the read before readiness fires.
    unsafe { hew_read_slot_cancel(slot) };
    assert_eq!(
        unsafe { hew_read_slot_status(slot) },
        ReadStatus::Cancelled as i32,
        "cancel must publish the Cancelled status"
    );

    // A late peer write: the reactor's deposit must be dropped (cancelled flag),
    // so the slot never transitions to Data.
    client.write_all(b"too-late").expect("client write");
    client.flush().ok();
    std::thread::sleep(Duration::from_millis(200));
    assert_eq!(
        unsafe { hew_read_slot_status(slot) },
        ReadStatus::Cancelled as i32,
        "a cancelled await must never receive a Data deposit from late readiness"
    );

    unsafe { hew_read_slot_free(slot) };
    drop(client);
    let _ = unsafe { hew_actor_free(actor) };
}
