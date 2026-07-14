//! Consumed-connection ownership tests for the TCP-as-Stream/Sink bridge
//! (`hew_tcp_stream_from_conn`, #2650).
//!
//! `into_stream_sink(self)` consumes its `Connection`: the source-level handle
//! is dead-by-move on every return path, so the runtime is its sole releaser.
//! Before the fix, the two `try_clone` early-returns released nothing — a
//! clone failure (EMFILE/ENFILE under fd pressure) leaked the original socket
//! fd *and* its `TCP_API_STATE` table slot, compounding to process-wide
//! descriptor exhaustion. These tests prove the runtime now fully closes and
//! releases the consumed connection on both clone-failure branches.
//!
//! Determinism comes from the `clone-failure-test` injection seam
//! (`force_next_clone_failures`), which fails a chosen clone *with a live table
//! entry present* — the valid-handle-clone-failed path. rlimit forcing (the
//! validator's repro method) is non-deterministic and cannot target clone #2,
//! so it is not the test vehicle here.
//!
//! Unix-gated to match `tcp_stream_bridge.rs` (fd counting + raw sockets have
//! no portable Windows equivalent); Windows bridge coverage stays on the
//! readiness punch-list. Requires `--features clone-failure-test`.
#![cfg(all(unix, feature = "clone-failure-test"))]

use std::ffi::CString;
use std::io::Write;
use std::net::{TcpListener, TcpStream};

use hew_runtime::stream::{
    hew_stream_next_sized, hew_stream_pair_free, hew_stream_pair_stream_bytes,
    hew_tcp_stream_from_conn,
};
use hew_runtime::transport::{
    clear_clone_failures, force_next_clone_failures, hew_tcp_close, hew_tcp_connect,
    tcp_streams_has_handle_for_test,
};

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Bind a loopback listener, connect via `hew_tcp_connect` (so the handle is
/// registered in `TCP_API_STATE`), and return `(conn_handle, peer_stream)`.
/// Mirrors `tcp_stream_bridge.rs::make_bridge_pair`.
fn make_bridge_pair() -> (i32, TcpStream) {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    let addr = CString::new(format!("127.0.0.1:{port}")).unwrap();
    // SAFETY: addr is a valid NUL-terminated C string.
    let conn = unsafe { hew_tcp_connect(addr.as_ptr()) };
    assert!(conn > 0, "hew_tcp_connect must succeed");
    let (peer, _) = listener.accept().unwrap();
    (conn, peer)
}

/// Count the process's currently-open file descriptors by listing `/dev/fd`
/// (macOS/BSD) or `/proc/self/fd` (Linux). Used only for the leak-slope shape,
/// which asserts a *flat* slope across N iterations rather than an absolute
/// count, so transient fds from listing itself do not matter.
fn open_fd_count() -> usize {
    let dir = if std::path::Path::new("/proc/self/fd").exists() {
        "/proc/self/fd"
    } else {
        "/dev/fd"
    };
    std::fs::read_dir(dir).map(|it| it.count()).unwrap_or(0)
}

/// Drain all items from a `HewStream` into a flat `Vec<u8>`, freeing each
/// malloc'd buffer. Mirrors `tcp_stream_bridge.rs::drain_bytes`.
///
/// # Safety
///
/// `stream` must be a valid, non-null `*mut HewStream` from the runtime.
unsafe fn drain_bytes(stream: *mut hew_runtime::stream::HewStream) -> Vec<u8> {
    let mut out = Vec::new();
    loop {
        let mut size: usize = 0;
        // SAFETY: stream is valid per caller; size is a stack-local.
        let ptr = unsafe { hew_stream_next_sized(stream, std::ptr::addr_of_mut!(size)) };
        if ptr.is_null() {
            break;
        }
        // SAFETY: ptr points to `size` bytes malloc'd by the runtime.
        let slice = unsafe { std::slice::from_raw_parts(ptr.cast::<u8>(), size) };
        out.extend_from_slice(slice);
        // SAFETY: ptr was malloc'd by hew_stream_next_sized; free it here.
        unsafe { libc::free(ptr) };
    }
    out
}

// ── Shape 1: table-ownership probe, first-clone failure ────────────────────────

/// First-clone failure with a valid handle must fully release the consumed
/// connection: the table entry is gone and a follow-up close finds nothing.
#[test]
fn first_clone_failure_releases_table_entry() {
    let (conn, _peer) = make_bridge_pair();
    assert!(
        tcp_streams_has_handle_for_test(conn),
        "precondition: conn is registered in the streams table"
    );

    // Fail clone #1 (the read fd); clone #2 is never reached.
    force_next_clone_failures(true, false);

    // SAFETY: conn is a valid registered handle; the injection forces the
    // first-clone-failure branch.
    let pair = unsafe { hew_tcp_stream_from_conn(conn) };
    clear_clone_failures();

    assert!(pair.is_null(), "clone failure must return a null pair");
    assert!(
        !tcp_streams_has_handle_for_test(conn),
        "the consumed connection must be released from the table on first-clone failure"
    );

    // Control (mirrors the validator's `second_close=-1`): the first close
    // removed a real entry that existed exactly once, so a follow-up close
    // finds nothing.
    assert_eq!(
        hew_tcp_close(conn),
        -1,
        "the entry was already released; a second close must report -1"
    );
}

// ── Shape 2: table-ownership probe, second-clone failure (partial) ─────────────

/// Second-clone failure (first clone succeeds, `read_stream` is a live dup)
/// must still fully release the consumed connection: the surviving dup does not
/// strand an owner and the table entry is gone.
#[test]
fn second_clone_failure_releases_table_entry() {
    let (conn, _peer) = make_bridge_pair();
    assert!(tcp_streams_has_handle_for_test(conn));

    // Clone #1 succeeds; clone #2 (the write fd) fails.
    force_next_clone_failures(false, true);

    // SAFETY: conn is a valid registered handle; the injection forces the
    // second-clone-failure (partial) branch.
    let pair = unsafe { hew_tcp_stream_from_conn(conn) };
    clear_clone_failures();

    assert!(pair.is_null(), "clone failure must return a null pair");
    assert!(
        !tcp_streams_has_handle_for_test(conn),
        "the consumed connection must be released on second-clone (partial) failure"
    );
    assert_eq!(
        hew_tcp_close(conn),
        -1,
        "entry already released; second close reports -1"
    );
}

// ── Shape 3: descriptor-leak slope ─────────────────────────────────────────────

/// Repeated clone failures must not grow the process fd table (flat slope).
/// This is the DoS guard: before the fix each failed conversion leaked the
/// original socket fd, so the slope grew with N. Peers are dropped each
/// iteration so only the *runtime-side* accounting is under test.
#[test]
fn repeated_clone_failure_does_not_leak_fds() {
    const N: usize = 200;

    // Warm up a few iterations so one-time lazy allocations (thread-locals,
    // listener teardown buffers) settle before the baseline sample.
    for _ in 0..8 {
        let (conn, peer) = make_bridge_pair();
        force_next_clone_failures(true, false);
        // SAFETY: conn is valid; forced first-clone failure.
        let pair = unsafe { hew_tcp_stream_from_conn(conn) };
        assert!(pair.is_null());
        drop(peer);
    }
    clear_clone_failures();

    let before = open_fd_count();

    for _ in 0..N {
        let (conn, peer) = make_bridge_pair();
        // Alternate which clone fails so both branches are exercised in bulk.
        force_next_clone_failures(true, false);
        // SAFETY: conn is valid; forced clone failure.
        let pair = unsafe { hew_tcp_stream_from_conn(conn) };
        assert!(pair.is_null(), "each conversion must fail and return null");
        assert!(
            !tcp_streams_has_handle_for_test(conn),
            "each failed conversion must release its table entry"
        );
        drop(peer);
    }
    clear_clone_failures();

    let after = open_fd_count();

    // A leak of one fd per iteration would blow far past this bound; a small
    // slack absorbs unrelated churn (loopback TIME_WAIT sockets do not hold
    // fds, but be generous to avoid CI flakes).
    let slack = 16;
    assert!(
        after <= before + slack,
        "fd count grew across {N} forced clone failures: before={before} after={after} \
         (a per-iteration leak would be ~{N}); consumed connections are not being released"
    );
}

// ── Shape 4: normal success path unaffected ────────────────────────────────────

/// With no injection the bridge succeeds, round-trips bytes, and releases the
/// table entry *without shutdown* (the two clones keep the socket alive).
/// Regression guard that the fix did not change success accounting.
#[test]
fn success_path_releases_entry_without_breaking_roundtrip() {
    clear_clone_failures();
    let payload = b"hello ownership";
    let (conn, mut peer) = make_bridge_pair();

    // SAFETY: conn is valid; no injection ⇒ normal success path.
    let pair = unsafe { hew_tcp_stream_from_conn(conn) };
    assert!(!pair.is_null(), "success path must return a non-null pair");

    // The table entry is released on success (without shutdown; clones live).
    assert!(
        !tcp_streams_has_handle_for_test(conn),
        "success path releases the table entry (unchanged accounting)"
    );

    // SAFETY: pair is valid; extract the stream half.
    let stream_ptr = unsafe { hew_stream_pair_stream_bytes(pair) };
    assert!(!stream_ptr.is_null());

    peer.write_all(payload).unwrap();
    drop(peer);

    // SAFETY: stream_ptr is valid.
    let received = unsafe { drain_bytes(stream_ptr) };
    assert_eq!(
        received, payload,
        "bytes must round-trip on the success path"
    );

    // SAFETY: pair is valid; stream slot already extracted.
    unsafe { hew_stream_pair_free(pair) };
}
