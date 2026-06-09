//! End-to-end oracle for the active-mode network I/O reactor.
//!
//! Proves "I/O completion as a mailbox message" at the runtime level: a real
//! TCP connection is attached to a real actor via `hew_tcp_attach`; when the
//! peer writes bytes, the reactor (on its own thread) reads them and delivers
//! them to the actor's `on_data` handler — no blocking read on a scheduler
//! worker. This is the substrate beneath the active-mode echo/web/redis
//! servers; it must RUN, not merely type-check.
//!
//! Gated to Unix: the active-mode reactor uses epoll/kqueue over raw socket
//! fds and is fail-closed (stubbed) on non-Unix targets, where `hew_tcp_attach`
//! intentionally returns an error. Windows active-mode I/O is a punch-list item.
#![cfg(unix)]
#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI e2e test asserts raw ABI behaviour directly"
)]

use std::ffi::{c_void, CString};
use std::io::{Read, Write};
use std::net::TcpStream;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

use hew_runtime::actor::{hew_actor_free, hew_actor_spawn_opts, HewActorOpts};
use hew_runtime::bytes::BytesTriple;
use hew_runtime::scheduler::hew_sched_init;
use hew_runtime::transport::{
    hew_actor_ref_local, hew_tcp_accept, hew_tcp_attach, hew_tcp_attach_local, hew_tcp_listen,
};

// Serialises e2e tests that drive the process-wide scheduler + reactor.
static E2E_MUTEX: Mutex<()> = Mutex::new(());

// Captures what the test actor's on_data handler received.
static RECEIVED: OnceLock<Mutex<Vec<u8>>> = OnceLock::new();
static CLOSE_SEEN: AtomicBool = AtomicBool::new(false);
static ON_DATA_TYPE: AtomicI32 = AtomicI32::new(0);
static ON_CLOSE_TYPE: AtomicI32 = AtomicI32::new(0);

fn received() -> &'static Mutex<Vec<u8>> {
    RECEIVED.get_or_init(|| Mutex::new(Vec::new()))
}

/// Dispatch that decodes the `on_data` `BytesTriple` payload and records the
/// bytes, or marks the close message. Mirrors how codegen-emitted dispatch
/// reads a `bytes` argument straight from the payload buffer.
unsafe extern "C-unwind" fn capture_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if msg_type == ON_CLOSE_TYPE.load(Ordering::Acquire) {
        CLOSE_SEEN.store(true, Ordering::Release);
        return std::ptr::null_mut();
    }
    if msg_type == ON_DATA_TYPE.load(Ordering::Acquire) && !data.is_null() {
        // The payload buffer holds a BytesTriple by value (the dispatch ABI
        // loads the `bytes` arg directly from it).
        let triple = unsafe { ptr::read(data.cast::<BytesTriple>()) };
        if !triple.ptr.is_null() && triple.len > 0 {
            let slice = unsafe {
                std::slice::from_raw_parts(
                    triple.ptr.add(triple.offset as usize),
                    triple.len as usize,
                )
            };
            received().lock().unwrap().extend_from_slice(slice);
        }
        // Release the refcount handed off by the reactor (the real codegen
        // path drops the bytes arg when on_data returns).
        unsafe { hew_runtime::bytes::hew_bytes_drop(triple.ptr) };
    }
    std::ptr::null_mut()
}

fn spawn_capture_actor() -> *mut hew_runtime::actor::HewActor {
    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(capture_dispatch),
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

// The scheduler is a process-wide init-once singleton that cannot be
// re-initialized after shutdown, so the two active-mode scenarios run under a
// SINGLE test that inits the scheduler once and never tears it down (mirroring
// the ffi_boundary scheduler-metrics tests). The reactor IS restartable, but
// keeping one process lifecycle avoids any cross-scenario global-state race.
#[test]
fn active_mode_io_end_to_end() {
    let _guard = E2E_MUTEX
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert_eq!(hew_sched_init(), 0, "scheduler init");

    scenario_delivers_socket_bytes_to_on_data();
    scenario_echo_round_trips_through_actor();
    scenario_attach_refuses_eviction_mailbox();
    scenario_attach_local_delivers_socket_bytes_to_on_data();
    scenario_attach_then_immediate_free_is_safe();
    // Deliberately no hew_runtime_cleanup(): the global scheduler is shared and
    // cannot be re-initialized; the process exit reclaims everything.
}

/// Regression for the attach-then-free use-after-free: a handler actor freed
/// while its `conn.attach` is still queued as a `Pending::Add` (i.e. before the
/// reactor's next drain promotes it) must be evicted by the actor-teardown hook.
///
/// Before the runtime fix, `reactor_detach_actor` scanned only the live
/// registry, so the still-queued add survived the free; the reactor then
/// promoted a registration pointing at the freed actor and, when the peer's
/// close surfaced, `hew_actor_ref_is_alive` dereferenced — and `on_close` was
/// delivered to — freed memory.
///
/// This drives the real attach -> free path many times with the peer writing
/// then closing, so a dangling promotion would have a live close event to
/// deliver. The free happens immediately after attach, well inside the
/// reactor's 50 ms poll window, so the add is overwhelmingly still in `pending`
/// at free time — the exact window the fix closes. Run under a hardened
/// allocator (e.g. `MallocScribble=1`, `AddressSanitizer`, or guardmalloc) the
/// pre-fix delivery to freed memory aborts; with the fix every cycle is clean.
fn scenario_attach_then_immediate_free_is_safe() {
    // Distinct msg ids so a stray delivery can't be mistaken for another
    // scenario's traffic.
    const ON_DATA: i32 = 4001;
    const ON_CLOSE: i32 = 4002;

    let addr = CString::new("127.0.0.1:0").unwrap();
    let listener = unsafe { hew_tcp_listen(addr.as_ptr()) };
    assert!(listener > 0, "listen should succeed");
    let port = hew_runtime::transport::tcp_listener_local_port(listener).expect("bound port");

    // Many rapid cycles so any residual interleaving (a drain that races a free)
    // surfaces under load/sanitizer rather than only on a lucky schedule.
    for _ in 0..64 {
        let mut client = TcpStream::connect(("127.0.0.1", port)).expect("client connect");
        let server_conn = hew_tcp_accept(listener);
        assert!(server_conn > 0, "accept should succeed");

        let actor = spawn_capture_actor();
        assert!(!actor.is_null(), "spawn capture actor");
        let actor_ref = unsafe { hew_actor_ref_local(actor) };
        let rc = unsafe { hew_tcp_attach(server_conn, &raw const actor_ref, ON_DATA, ON_CLOSE) };
        assert_eq!(rc, 0, "attach should succeed");

        // Peer sends then closes: a dangling promotion would have both an
        // on_data and an on_close to deliver to the freed actor.
        let _ = client.write_all(b"x");
        client.flush().ok();
        drop(client);

        // Free immediately — the add is still queued in the reactor's pending
        // list. The teardown hook MUST evict it; otherwise the next reactor
        // poll promotes a registration into freed memory.
        let _ = unsafe { hew_actor_free(actor) };
    }

    // Give the reactor several poll cycles to drain. If any cycle had promoted a
    // dangling registration, a close delivery to freed memory would have aborted
    // the process by now (under a hardened allocator) or corrupted the registry.
    std::thread::sleep(Duration::from_millis(200));
}

/// Fail-closed guard: `hew_tcp_attach` MUST refuse an actor whose mailbox can
/// evict a queued node (bounded `DropOld` / `Coalesce`). Active-mode `on_data`
/// is a raw (`envelope == null`) node whose embedded `BytesTriple` refcount is
/// dropped only by the consuming handler; an eviction would free the triple
/// container as plain bytes and leak its refcounted buffer. Refusing the
/// attach is the safe outcome (no leak, no double-free). An unbounded mailbox
/// on the same actor surface attaches fine — proving the guard targets the
/// policy, not the act of attaching.
fn scenario_attach_refuses_eviction_mailbox() {
    let addr = CString::new("127.0.0.1:0").unwrap();
    let listener = unsafe { hew_tcp_listen(addr.as_ptr()) };
    assert!(listener > 0, "listen should succeed");
    let port = hew_runtime::transport::tcp_listener_local_port(listener).expect("bound port");

    // A bounded DropOld (overflow == 2) mailbox is leak-prone → attach refused.
    let drop_old_conn = {
        let _client = TcpStream::connect(("127.0.0.1", port)).expect("client connect");
        let server_conn = hew_tcp_accept(listener);
        assert!(server_conn > 0, "accept should succeed");
        let opts = HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(capture_dispatch),
            mailbox_capacity: 4,
            overflow: 2, // HewOverflowPolicy::DropOld
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            budget: 0,
            arena_cap_bytes: 0,
            cycle_capable: 0,
        };
        let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
        assert!(!actor.is_null(), "spawn DropOld actor");
        let actor_ref = unsafe { hew_actor_ref_local(actor) };
        let rc = unsafe { hew_tcp_attach(server_conn, &raw const actor_ref, 1001, 1002) };
        assert_eq!(
            rc, -1,
            "attach must refuse a bounded DropOld mailbox (leak-prone eviction)"
        );
        let _ = unsafe { hew_actor_free(actor) };
        server_conn
    };
    let _ = drop_old_conn;

    // A bounded Coalesce (overflow == 4) mailbox is likewise refused.
    {
        let _client = TcpStream::connect(("127.0.0.1", port)).expect("client connect");
        let server_conn = hew_tcp_accept(listener);
        assert!(server_conn > 0, "accept should succeed");
        let opts = HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(capture_dispatch),
            mailbox_capacity: 4,
            overflow: 4, // HewOverflowPolicy::Coalesce
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            budget: 0,
            arena_cap_bytes: 0,
            cycle_capable: 0,
        };
        let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
        assert!(!actor.is_null(), "spawn Coalesce actor");
        let actor_ref = unsafe { hew_actor_ref_local(actor) };
        let rc = unsafe { hew_tcp_attach(server_conn, &raw const actor_ref, 1001, 1002) };
        assert_eq!(
            rc, -1,
            "attach must refuse a bounded Coalesce mailbox (leak-prone replace/eviction)"
        );
        let _ = unsafe { hew_actor_free(actor) };
    }

    // Control: a bounded DropNew (overflow == 1) mailbox never evicts a queued
    // node, so the same bounded-capacity attach is accepted.
    {
        let _client = TcpStream::connect(("127.0.0.1", port)).expect("client connect");
        let server_conn = hew_tcp_accept(listener);
        assert!(server_conn > 0, "accept should succeed");
        let opts = HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(capture_dispatch),
            mailbox_capacity: 4,
            overflow: 1, // HewOverflowPolicy::DropNew
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            budget: 0,
            arena_cap_bytes: 0,
            cycle_capable: 0,
        };
        let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
        assert!(!actor.is_null(), "spawn DropNew actor");
        let actor_ref = unsafe { hew_actor_ref_local(actor) };
        let rc = unsafe { hew_tcp_attach(server_conn, &raw const actor_ref, 1001, 1002) };
        assert_eq!(
            rc, 0,
            "attach must accept a bounded DropNew mailbox (no queued-node eviction)"
        );
        // Free the actor WITHOUT a manual detach: `hew_actor_free` runs the
        // actor-teardown hook (`reactor_detach_actor`), which must synchronously
        // evict this actor's reactor registration — whether it has been promoted
        // into the registry or is still queued as a `Pending::Add` — so a later
        // scenario's reactor poll cannot observe the fd's close event and deliver
        // `on_close` to freed memory. Exercising the real teardown path (rather
        // than masking it with an explicit `hew_tcp_detach`) is what proves the
        // attach-then-free UAF is closed.
        let _ = unsafe { hew_actor_free(actor) };
    }
}

fn scenario_delivers_socket_bytes_to_on_data() {
    received().lock().unwrap().clear();
    CLOSE_SEEN.store(false, Ordering::Release);
    // Arbitrary distinct msg_type indices for this scenario's handlers.
    ON_DATA_TYPE.store(1001, Ordering::Release);
    ON_CLOSE_TYPE.store(1002, Ordering::Release);

    // Listen on an ephemeral port and connect a client.
    let addr = CString::new("127.0.0.1:0").unwrap();
    let listener = unsafe { hew_tcp_listen(addr.as_ptr()) };
    assert!(listener > 0, "listen should succeed");
    let port = hew_runtime::transport::tcp_listener_local_port(listener)
        .expect("listener should expose its bound port");

    let mut client = TcpStream::connect(("127.0.0.1", port)).expect("client connect");
    let server_conn = hew_tcp_accept(listener);
    assert!(server_conn > 0, "accept should succeed");

    // Spawn the capture actor and attach the server side in active mode.
    let actor = spawn_capture_actor();
    assert!(!actor.is_null(), "spawn capture actor");
    let actor_ref = unsafe { hew_actor_ref_local(actor) };
    let rc = unsafe {
        hew_tcp_attach(
            server_conn,
            &raw const actor_ref,
            ON_DATA_TYPE.load(Ordering::Acquire),
            ON_CLOSE_TYPE.load(Ordering::Acquire),
        )
    };
    assert_eq!(rc, 0, "attach should succeed");

    // Client writes a payload; the reactor should read it and deliver on_data.
    client
        .write_all(b"hello-active-mode")
        .expect("client write");
    client.flush().ok();

    // Wait (bounded) for the bytes to surface in the actor's on_data.
    let deadline = Instant::now() + Duration::from_secs(3);
    loop {
        if received().lock().unwrap().as_slice() == b"hello-active-mode" {
            break;
        }
        assert!(
            Instant::now() < deadline,
            "active-mode on_data did not receive the bytes within 3s; got {:?}",
            received().lock().unwrap()
        );
        std::thread::sleep(Duration::from_millis(20));
    }

    // Closing the client should drive a single on_close to the actor.
    drop(client);
    let close_deadline = Instant::now() + Duration::from_secs(3);
    while !CLOSE_SEEN.load(Ordering::Acquire) {
        assert!(
            Instant::now() < close_deadline,
            "active-mode on_close was not delivered within 3s"
        );
        std::thread::sleep(Duration::from_millis(20));
    }

    // Detach + free the actor; the reactor unregisters the fd via the
    // actor-teardown hook. Scheduler stays up for the next scenario.
    let _ = unsafe { hew_actor_free(actor) };
}

/// Same delivery proof as `scenario_delivers_socket_bytes_to_on_data`, but
/// driven through `hew_tcp_attach_local` — the entry point the Hew
/// `conn.attach(handler)` surface lowers to. The compiler hands this function
/// the bare `*mut HewActor` from a `LocalPid<Actor>` (no `HewActorRef`
/// wrapper); the runtime constructs the local ref itself. Proving this path
/// RUNS is the runtime-level equivalent of the Hew-source echo oracle, which
/// cannot run end-to-end while the pre-existing D10 `fs.IoError` codegen gap
/// blocks every `std::net` program from reaching the LLVM emitter.
fn scenario_attach_local_delivers_socket_bytes_to_on_data() {
    received().lock().unwrap().clear();
    CLOSE_SEEN.store(false, Ordering::Release);
    ON_DATA_TYPE.store(3001, Ordering::Release);
    ON_CLOSE_TYPE.store(3002, Ordering::Release);

    let addr = CString::new("127.0.0.1:0").unwrap();
    let listener = unsafe { hew_tcp_listen(addr.as_ptr()) };
    assert!(listener > 0, "listen should succeed");
    let port = hew_runtime::transport::tcp_listener_local_port(listener)
        .expect("listener should expose its bound port");

    let mut client = TcpStream::connect(("127.0.0.1", port)).expect("client connect");
    let server_conn = hew_tcp_accept(listener);
    assert!(server_conn > 0, "accept should succeed");

    // Spawn the capture actor and attach via the LocalPid entry point: pass the
    // raw `*mut HewActor` (what a Hew `LocalPid<Actor>` lowers to), not a
    // pre-built HewActorRef. The runtime constructs the local ref internally.
    let actor = spawn_capture_actor();
    assert!(!actor.is_null(), "spawn capture actor");
    let rc = unsafe {
        hew_tcp_attach_local(
            server_conn,
            actor,
            ON_DATA_TYPE.load(Ordering::Acquire),
            ON_CLOSE_TYPE.load(Ordering::Acquire),
        )
    };
    assert_eq!(rc, 0, "hew_tcp_attach_local should succeed");

    client
        .write_all(b"hello-attach-local")
        .expect("client write");
    client.flush().ok();

    let deadline = Instant::now() + Duration::from_secs(3);
    loop {
        if received().lock().unwrap().as_slice() == b"hello-attach-local" {
            break;
        }
        assert!(
            Instant::now() < deadline,
            "hew_tcp_attach_local on_data did not receive the bytes within 3s; got {:?}",
            received().lock().unwrap()
        );
        std::thread::sleep(Duration::from_millis(20));
    }

    drop(client);
    let close_deadline = Instant::now() + Duration::from_secs(3);
    while !CLOSE_SEEN.load(Ordering::Acquire) {
        assert!(
            Instant::now() < close_deadline,
            "hew_tcp_attach_local on_close was not delivered within 3s"
        );
        std::thread::sleep(Duration::from_millis(20));
    }

    let _ = unsafe { hew_actor_free(actor) };
}

// Echo-test globals: an echo actor writes received bytes back out the same
// connection. Hoisted to module scope so the dispatch fn is not an item after
// statements inside the test body.
static ECHO_CONN: AtomicI32 = AtomicI32::new(-1);
static ECHO_ON_DATA: AtomicI32 = AtomicI32::new(2001);

/// Echo dispatch: on `on_data`, write the received bytes back out the same
/// connection via the outbound `hew_tcp_write` (the `conn.send`) path. Proves
/// the full active-mode loop (read → actor → send) RUNS.
unsafe extern "C-unwind" fn echo_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if msg_type != ECHO_ON_DATA.load(Ordering::Acquire) || data.is_null() {
        return std::ptr::null_mut();
    }
    let triple = unsafe { ptr::read(data.cast::<BytesTriple>()) };
    if !triple.ptr.is_null() && triple.len > 0 {
        let conn = ECHO_CONN.load(Ordering::Acquire);
        // hew_tcp_write now takes the triple BY POINTER (borrowing the active
        // region); pass the on_data triple's address — no HewVec bridge.
        unsafe { hew_runtime::transport::hew_tcp_write(conn, std::ptr::addr_of!(triple)) };
    }
    unsafe { hew_runtime::bytes::hew_bytes_drop(triple.ptr) };
    std::ptr::null_mut()
}

fn scenario_echo_round_trips_through_actor() {
    let addr = CString::new("127.0.0.1:0").unwrap();
    let listener = unsafe { hew_tcp_listen(addr.as_ptr()) };
    assert!(listener > 0);
    let port = hew_runtime::transport::tcp_listener_local_port(listener).expect("bound port");

    let mut client = TcpStream::connect(("127.0.0.1", port)).expect("client connect");
    let server_conn = hew_tcp_accept(listener);
    assert!(server_conn > 0);
    ECHO_CONN.store(server_conn, Ordering::Release);

    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(echo_dispatch),
        mailbox_capacity: 0,
        overflow: 0,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        budget: 0,
        arena_cap_bytes: 0,
        cycle_capable: 0,
    };
    let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
    assert!(!actor.is_null());
    let actor_ref = unsafe { hew_actor_ref_local(actor) };
    let rc = unsafe {
        hew_tcp_attach(
            server_conn,
            &raw const actor_ref,
            ECHO_ON_DATA.load(Ordering::Acquire),
            2002,
        )
    };
    assert_eq!(rc, 0, "attach should succeed");

    client.write_all(b"ping").expect("client write");
    client.flush().ok();

    // Read the echoed reply back on the client.
    client
        .set_read_timeout(Some(Duration::from_secs(3)))
        .expect("set client read timeout");
    let mut buf = [0u8; 64];
    let n = client.read(&mut buf).expect("client read echo");
    assert_eq!(&buf[..n], b"ping", "the actor must echo the bytes back");

    drop(client);
    let _ = unsafe { hew_actor_free(actor) };
}
