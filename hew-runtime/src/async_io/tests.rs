use super::*;
use std::ffi::c_void;
use std::io::{Read, Write};
use std::sync::{Barrier, Condvar};
use std::time::{Duration, Instant};

use hew_cabi::string::{string_as_str, string_release};

#[derive(Default)]
struct ReadySignal {
    notifications: Mutex<usize>,
    ready: Condvar,
}

unsafe extern "C" fn retain(context: *mut c_void) {
    // SAFETY: the test descriptor owns an Arc::into_raw reference to ReadySignal.
    unsafe { Arc::increment_strong_count(context.cast::<ReadySignal>()) };
}

unsafe extern "C" fn release(context: *mut c_void) {
    // SAFETY: one creator/OwnedWaker reference is consumed by each callback.
    unsafe { Arc::decrement_strong_count(context.cast::<ReadySignal>()) };
}

unsafe extern "C" fn notify(context: *mut c_void) {
    // SAFETY: OwnedWaker retains the context through this notification.
    let signal = unsafe { &*context.cast::<ReadySignal>() };
    *signal.notifications.lock().unwrap() += 1;
    signal.ready.notify_all();
}

fn descriptor(signal: &Arc<ReadySignal>) -> HewWaker {
    HewWaker {
        context: Arc::as_ptr(signal).cast_mut().cast(),
        wake: notify,
        retain,
        release,
    }
}

fn await_ready(signal: &ReadySignal) {
    let deadline = Instant::now() + Duration::from_secs(5);
    let mut notifications = signal.notifications.lock().unwrap();
    while *notifications == 0 {
        let remaining = deadline.saturating_duration_since(Instant::now());
        assert!(!remaining.is_zero(), "operation did not notify readiness");
        notifications = signal
            .ready
            .wait_timeout(notifications, remaining)
            .unwrap()
            .0;
    }
}

#[test]
fn resume_restores_owned_errors_and_clears_stale_success_errors() {
    // SAFETY: a null waker is permitted for a manually completed operation.
    let operation = unsafe { HewAsyncIo::new(ptr::null()) };
    let producer = Arc::clone(&operation);
    std::thread::spawn(move || {
        crate::stream_error::set_last_error("unrelated worker error".into());
        producer.complete(Err(IoFailure::from_io(
            "read file",
            &io::Error::new(io::ErrorKind::NotFound, "owned operation failure"),
        )));
    })
    .join()
    .unwrap();
    crate::stream_error::set_last_error("unrelated caller error".into());
    // SAFETY: the Arc retains operation, and the returned error string is owned.
    unsafe {
        assert_eq!(
            hew_async_io_restore_error(Arc::as_ptr(&operation)),
            AsyncIoStatus::Error as i32
        );
        assert_eq!(
            crate::stream_error::hew_stream_last_error_kind(),
            crate::stream_error::IO_ERROR_KIND_NOT_FOUND
        );
        assert_eq!(crate::stream_error::hew_stream_last_errno(), libc::EIO);
        let message = crate::stream_error::hew_stream_last_error();
        assert_eq!(string_as_str(message), "read file: owned operation failure");
        string_release(message);
    }
    // SAFETY: null waker is valid; the Arc owns this pending operation.
    let success = unsafe { HewAsyncIo::new(ptr::null()) };
    crate::stream_error::set_last_error("keep while pending".into());
    // SAFETY: success remains live throughout the status/restore calls.
    unsafe {
        assert_eq!(
            hew_async_io_restore_error(Arc::as_ptr(&success)),
            AsyncIoStatus::Pending as i32
        );
    }
    assert_eq!(
        crate::stream_error::take_last_error().as_deref(),
        Some("keep while pending")
    );
    success.complete(Ok(IoValue::Bytes(Vec::new())));
    crate::stream_error::set_last_error_with_errno("stale".into(), libc::EIO);
    // SAFETY: success remains live and ready.
    unsafe {
        assert_eq!(
            hew_async_io_restore_error(Arc::as_ptr(&success)),
            AsyncIoStatus::Success as i32
        );
    }
    assert!(crate::stream_error::take_last_error().is_none());
    assert_eq!(crate::stream_error::take_last_errno(), 0);
}

#[test]
fn string_file_write_owns_its_inputs_after_submission() {
    let _runtime = crate::runtime_test_guard();
    let directory = tempfile::tempdir().unwrap();
    let destination = directory.path().join("text.txt");
    let path = string_from_str(destination.to_str().unwrap());
    let expected = "snow 雪\0tail";
    let content = string_from_str(expected);
    let signal = Arc::new(ReadySignal::default());
    // SAFETY: submission copies both live strings and retains the waker.
    let operation = unsafe {
        let operation = hew_async_file_write_string(path, content, &descriptor(&signal));
        string_release(path);
        string_release(content);
        operation
    };
    await_ready(&signal);
    let mut count = -1;
    // SAFETY: the operation is live; count is writable; free releases it.
    unsafe {
        assert_eq!(
            hew_async_io_take_count(operation, &raw mut count),
            AsyncIoStatus::Success as i32
        );
        hew_async_io_free(operation);
    }
    assert_eq!(count, i64::try_from(expected.len()).unwrap());
    assert_eq!(std::fs::read(destination).unwrap(), expected.as_bytes());
}

#[test]
fn file_read_write_own_inputs_and_preserve_binary_contents() {
    let _runtime = crate::runtime_test_guard();
    let directory = tempfile::tempdir().unwrap();
    let path = string_from_str(directory.path().join("data.bin").to_str().unwrap());
    let contents = b"prefix\0\xffsuffix";
    let written = Arc::new(ReadySignal::default());
    // SAFETY: the test supplies managed inputs and a live retained waker target.
    let write = unsafe {
        let bytes = crate::bytes::hew_bytes_from_static(
            contents.as_ptr(),
            u32::try_from(contents.len()).unwrap(),
        );
        let operation = hew_async_file_write(path, bytes, &descriptor(&written));
        crate::bytes::hew_bytes_drop(bytes.ptr);
        string_release(path);
        operation
    };
    await_ready(&written);
    let mut count = -1;
    // SAFETY: write is live; count is writable; free consumes its creator ref.
    unsafe {
        assert_eq!(
            hew_async_io_take_count(write, &raw mut count),
            AsyncIoStatus::Success as i32
        );
        assert_eq!(count, i64::try_from(contents.len()).unwrap());
        count = -2;
        assert_eq!(
            hew_async_io_take_count(write, &raw mut count),
            AsyncIoStatus::Taken as i32
        );
        assert_eq!(count, -2);
        hew_async_io_free(write);
    }

    let read_signal = Arc::new(ReadySignal::default());
    let path = string_from_str(directory.path().join("data.bin").to_str().unwrap());
    // SAFETY: the start call copies the managed path before it is released.
    let read = unsafe {
        let operation = hew_async_file_read(path, &descriptor(&read_signal));
        string_release(path);
        operation
    };
    await_ready(&read_signal);
    let mut bytes = BytesTriple {
        ptr: ptr::null_mut(),
        offset: 0,
        len: 0,
    };
    // SAFETY: read owns its result; the successful take initializes this triple.
    unsafe {
        assert_eq!(
            hew_async_io_take_bytes(read, &raw mut bytes),
            AsyncIoStatus::Success as i32
        );
        hew_async_io_free(read);
        assert_eq!(
            std::slice::from_raw_parts(bytes.ptr.add(bytes.offset as usize), bytes.len as usize),
            contents
        );
        crate::bytes::hew_bytes_drop(bytes.ptr);
    }
}

#[test]
fn file_errors_are_owned_across_threads_and_outlive_the_operation() {
    let _runtime = crate::runtime_test_guard();
    let directory = tempfile::tempdir().unwrap();
    let path = string_from_str(directory.path().join("missing.bin").to_str().unwrap());
    let signal = Arc::new(ReadySignal::default());
    crate::stream_error::set_last_error("caller's unrelated diagnostic".into());
    // SAFETY: start borrows the inputs only until return.
    let operation = unsafe {
        let operation = hew_async_file_read(path, &descriptor(&signal));
        string_release(path);
        operation
    };
    await_ready(&signal);
    let sentinel = BytesTriple {
        ptr: ptr::null_mut(),
        offset: 71,
        len: 19,
    };
    let mut out = sentinel;
    // SAFETY: operation remains live for accessors and the failed take.
    unsafe {
        assert_eq!(hew_async_io_status(operation), AsyncIoStatus::Error as i32);
        assert_eq!(
            hew_async_io_error_kind(operation),
            crate::stream_error::IO_ERROR_KIND_NOT_FOUND
        );
        assert_ne!(hew_async_io_errno(operation), 0);
        assert_eq!(
            hew_async_io_take_bytes(operation, &raw mut out),
            AsyncIoStatus::Error as i32
        );
        assert_eq!(
            (out.ptr, out.offset, out.len),
            (sentinel.ptr, sentinel.offset, sentinel.len)
        );
        let message = hew_async_io_error(operation);
        hew_async_io_free(operation);
        assert!(string_as_str(message).contains("read file"));
        string_release(message);
    }
    assert_eq!(
        crate::stream_error::take_last_error().as_deref(),
        Some("caller's unrelated diagnostic")
    );
}

#[test]
fn empty_file_is_success_and_invalid_paths_never_reach_the_pool() {
    let _runtime = crate::runtime_test_guard();
    let directory = tempfile::tempdir().unwrap();
    let file = directory.path().join("empty.bin");
    std::fs::write(&file, []).unwrap();
    let signal = Arc::new(ReadySignal::default());
    let path = string_from_str(file.to_str().unwrap());
    // SAFETY: inputs are live; start retains the waker and copies the path.
    let operation = unsafe { hew_async_file_read(path, &descriptor(&signal)) };
    await_ready(&signal);
    let mut out = BytesTriple {
        ptr: ptr::null_mut(),
        offset: 11,
        len: 22,
    };
    // SAFETY: the take initializes writable out; both owned inputs are released.
    unsafe {
        assert_eq!(
            hew_async_io_take_bytes(operation, &raw mut out),
            AsyncIoStatus::Success as i32
        );
        assert!(out.ptr.is_null());
        assert_eq!((out.offset, out.len), (0, 0));
        hew_async_io_free(operation);
        string_release(path);
    }
    for path in ["", "before\0after"] {
        let path = string_from_str(path);
        // SAFETY: each path is managed; null waker requests polling only.
        unsafe {
            let operation = hew_async_file_read(path, ptr::null());
            assert_eq!(hew_async_io_status(operation), AsyncIoStatus::Error as i32);
            assert_eq!(hew_async_io_errno(operation), libc::EINVAL);
            hew_async_io_free(operation);
            string_release(path);
        }
    }
}

#[test]
fn late_accept_after_abandonment_closes_its_connection_without_waking() {
    let _runtime = crate::runtime_test_guard();
    let signal = Arc::new(ReadySignal::default());
    // SAFETY: descriptor's creator Arc lives through operation construction.
    let producer = unsafe { HewAsyncIo::new(&descriptor(&signal)) };
    let creator = Arc::into_raw(Arc::clone(&producer));
    // SAFETY: the creator reference is consumed; producer has an independent ref.
    unsafe { hew_async_io_free(creator) };
    assert_eq!(
        Arc::strong_count(&signal),
        1,
        "cancel detaches the retained waker"
    );
    let (handle, _peer) = crate::transport::tcp_socketpair_conn_for_test();
    producer.complete(Ok(IoValue::Connection(AcceptedConnection(handle))));
    assert!(crate::transport::tcp_conn_raw_fd(handle).is_none());
    assert_eq!(*signal.notifications.lock().unwrap(), 0);
}

#[test]
fn accepted_handle_has_one_owner_before_and_after_take() {
    let _runtime = crate::runtime_test_guard();
    for take in [false, true] {
        let (handle, _peer) = crate::transport::tcp_socketpair_conn_for_test();
        // SAFETY: null waker needs no retained context.
        let operation = unsafe { HewAsyncIo::new(ptr::null()) };
        operation.complete(Ok(IoValue::Connection(AcceptedConnection(handle))));
        let operation = Arc::into_raw(operation);
        let mut result = -1;
        // SAFETY: operation is live, result is writable, and free consumes creator.
        unsafe {
            if take {
                assert_eq!(
                    hew_async_io_take_handle(operation, &raw mut result),
                    AsyncIoStatus::Success as i32
                );
                assert_eq!(result, i64::from(handle));
            }
            hew_async_io_free(operation);
        }
        assert_eq!(crate::transport::tcp_conn_raw_fd(handle).is_some(), take);
        if take {
            crate::transport::tcp_close_orphan_conn(handle);
        }
    }
}

#[test]
fn completion_and_cancel_race_publishes_at_most_one_result_and_wake() {
    for _ in 0..64 {
        let signal = Arc::new(ReadySignal::default());
        // SAFETY: the descriptor is backed by signal for the full call.
        let operation = unsafe { HewAsyncIo::new(&descriptor(&signal)) };
        let barrier = Arc::new(Barrier::new(2));
        let worker_operation = Arc::clone(&operation);
        let worker_barrier = Arc::clone(&barrier);
        let worker = std::thread::spawn(move || {
            worker_barrier.wait();
            worker_operation.complete(Ok(IoValue::Count(37)));
        });
        barrier.wait();
        let cancelled = operation.cancel();
        worker.join().unwrap();
        let mut count = 99;
        // SAFETY: Arc keeps operation live and count is writable during take.
        let status = unsafe { hew_async_io_take_count(Arc::as_ptr(&operation), &raw mut count) };
        if cancelled {
            assert_eq!(status, AsyncIoStatus::Cancelled as i32);
            assert_eq!(count, 99);
            assert_eq!(*signal.notifications.lock().unwrap(), 0);
        } else {
            assert_eq!(status, AsyncIoStatus::Success as i32);
            assert_eq!(count, 37);
            assert_eq!(*signal.notifications.lock().unwrap(), 1);
        }
        assert_eq!(
            Arc::strong_count(&signal),
            1,
            "terminal path released the waker"
        );
    }
}

type WorkerGate = Arc<(Mutex<bool>, Condvar)>;
type GateJob = (WorkerGate, std::sync::mpsc::Sender<()>);

struct ReleaseWorkers(WorkerGate);

impl Drop for ReleaseWorkers {
    fn drop(&mut self) {
        *self.0 .0.lock().unwrap() = true;
        self.0 .1.notify_all();
    }
}

unsafe extern "C" fn block_worker(context: *mut c_void) {
    // SAFETY: the test transfers one boxed gate job to this sole pool callback.
    let (gate, entered) = *unsafe { Box::from_raw(context.cast::<GateJob>()) };
    entered.send(()).unwrap();
    let mut open = gate.0.lock().unwrap();
    while !*open {
        open = gate.1.wait(open).unwrap();
    }
}

#[test]
fn queued_file_cancellation_never_blocks_submission_or_writes_after_abandonment() {
    let _runtime = crate::runtime_test_guard();
    let directory = tempfile::tempdir().unwrap();
    let destination = directory.path().join("cancelled.bin");
    let pool = crate::blocking_pool::shared_blocking_pool_opt().unwrap();
    let gate: WorkerGate = Arc::new((Mutex::new(false), Condvar::new()));
    // Even a failed assertion releases workers before the runtime joins them.
    let release_workers = ReleaseWorkers(Arc::clone(&gate));
    let (entered, workers) = std::sync::mpsc::channel::<()>();
    for _ in 0..crate::blocking_pool::HEW_BLOCKING_POOL_SIZE {
        let job = Box::into_raw(Box::new((Arc::clone(&gate), entered.clone())));
        // SAFETY: the runtime owns pool; each admitted callback owns one gate box.
        assert_eq!(
            // SAFETY: the runtime owns pool and this callback owns its gate box.
            unsafe {
                crate::blocking_pool::hew_blocking_pool_submit(pool, block_worker, job.cast())
            },
            0
        );
    }
    for _ in 0..crate::blocking_pool::HEW_BLOCKING_POOL_SIZE {
        workers.recv_timeout(Duration::from_secs(5)).unwrap();
    }
    let signal = Arc::new(ReadySignal::default());
    let path = string_from_str(destination.to_str().unwrap());
    // SAFETY: managed inputs are valid for submission and deliberately released
    // before any pool worker can run the operation.
    let operation = unsafe {
        let data = crate::bytes::hew_bytes_from_static(b"data".as_ptr(), 4);
        let operation = hew_async_file_write(path, data, &descriptor(&signal));
        string_release(path);
        crate::bytes::hew_bytes_drop(data.ptr);
        assert_eq!(
            hew_async_io_status(operation),
            AsyncIoStatus::Pending as i32
        );
        operation
    };
    let cleanup = Arc::new(ReadySignal::default());
    // SAFETY: cancellation keeps the creator reference live for cleanup polling.
    unsafe {
        assert_eq!(hew_async_io_cancel(operation), 1);
        assert_eq!(
            hew_async_io_status(operation),
            AsyncIoStatus::Cancelled as i32
        );
        assert_eq!(
            hew_async_io_cleanup_status(operation, &descriptor(&cleanup)),
            0
        );
    }
    assert_eq!(*cleanup.notifications.lock().unwrap(), 0);
    assert_eq!(Arc::strong_count(&signal), 1);
    drop(release_workers);
    await_ready(&cleanup);
    // SAFETY: a cleanup wake permits polling and releasing the creator reference.
    unsafe {
        assert_eq!(hew_async_io_cleanup_status(operation, ptr::null()), 1);
        hew_async_io_free(operation);
    }
    assert!(!destination.exists());
    assert_eq!(*signal.notifications.lock().unwrap(), 0);
}

struct StopReactor;

impl Drop for StopReactor {
    fn drop(&mut self) {
        crate::reactor::reactor_shutdown();
    }
}

unsafe fn take_read(operation: *const HewAsyncIo) -> Vec<u8> {
    let mut bytes = BytesTriple {
        ptr: ptr::null_mut(),
        offset: 0,
        len: 0,
    };
    // SAFETY: caller owns operation; the take initializes writable bytes.
    unsafe {
        assert_eq!(
            hew_async_io_take_bytes(operation, &raw mut bytes),
            AsyncIoStatus::Success as i32
        );
        let result = if bytes.len == 0 {
            Vec::new()
        } else {
            std::slice::from_raw_parts(bytes.ptr.add(bytes.offset as usize), bytes.len as usize)
                .to_vec()
        };
        crate::bytes::hew_bytes_drop(bytes.ptr);
        hew_async_io_free(operation);
        result
    }
}

#[test]
fn tcp_read_rearms_after_each_result_and_preserves_eof() {
    let _runtime = crate::runtime_test_guard();
    let _reactor = StopReactor;
    let (handle, mut peer) = crate::transport::tcp_socketpair_conn_for_test();
    let payload: Vec<u8> = (0..15_000)
        .map(|index| u8::try_from(index % 251).unwrap())
        .collect();
    let first = Arc::new(ReadySignal::default());
    // SAFETY: handle and waker are live across the pending operation.
    let operation = unsafe { hew_async_tcp_read(handle, &descriptor(&first)) };
    peer.write_all(&payload).unwrap();
    await_ready(&first);
    // SAFETY: a successful read owns bytes and its operation reference.
    let mut received = unsafe { take_read(operation) };
    while received.len() < payload.len() {
        let signal = Arc::new(ReadySignal::default());
        // SAFETY: handle remains live; the new read retains signal's descriptor.
        let operation = unsafe { hew_async_tcp_read(handle, &descriptor(&signal)) };
        await_ready(&signal);
        // SAFETY: readiness makes the owned result available to this sole take.
        received.extend(unsafe { take_read(operation) });
    }
    assert_eq!(received, payload);
    peer.shutdown(std::net::Shutdown::Write).unwrap();
    let eof = Arc::new(ReadySignal::default());
    // SAFETY: the local handle remains live after the peer closes its write half.
    let operation = unsafe { hew_async_tcp_read(handle, &descriptor(&eof)) };
    await_ready(&eof);
    // SAFETY: EOF is a successful empty bytes result, not an error.
    assert!(unsafe { take_read(operation) }.is_empty());
    assert_eq!(crate::transport::hew_tcp_close(handle), 0);
}

#[test]
fn tcp_cancel_detaches_before_rearming_the_same_handle() {
    let _runtime = crate::runtime_test_guard();
    let _reactor = StopReactor;
    let (handle, mut peer) = crate::transport::tcp_socketpair_conn_for_test();
    let cancelled = Arc::new(ReadySignal::default());
    // SAFETY: no peer data can complete this read before cancellation.
    unsafe {
        let operation = hew_async_tcp_read(handle, &descriptor(&cancelled));
        assert_eq!(hew_async_io_cancel(operation), 1);
        hew_async_io_free(operation);
    }
    let signal = Arc::new(ReadySignal::default());
    // SAFETY: the original resource remains live and starts a new borrow.
    let operation = unsafe { hew_async_tcp_read(handle, &descriptor(&signal)) };
    peer.write_all(b"after cancellation").unwrap();
    await_ready(&signal);
    // SAFETY: the new operation is ready and solely owned by this consumer.
    assert_eq!(unsafe { take_read(operation) }, b"after cancellation");
    assert_eq!(*cancelled.notifications.lock().unwrap(), 0);
    assert_eq!(Arc::strong_count(&cancelled), 1);
    assert_eq!(crate::transport::hew_tcp_close(handle), 0);
}

#[test]
fn tcp_accept_untaken_result_closes_the_peer_and_taken_result_stays_owned() {
    let _runtime = crate::runtime_test_guard();
    let _reactor = StopReactor;
    for take in [false, true] {
        let (listener, mut peer) = crate::transport::tcp_listener_with_pending_conn_for_test();
        peer.set_read_timeout(Some(Duration::from_secs(5))).unwrap();
        let signal = Arc::new(ReadySignal::default());
        // SAFETY: listener remains live through readiness and the operation owns
        // its accepted connection until a take transfers that authority.
        let operation = unsafe { hew_async_tcp_accept(listener, &descriptor(&signal)) };
        await_ready(&signal);
        // SAFETY: creator reference is live; inspection records the exact socket
        // whose external close/table lifetime the test checks below.
        let accepted = unsafe {
            let state = (*operation).state.lock_or_recover();
            let State::Ready(Ok(IoValue::Connection(connection))) = &*state else {
                panic!("accept failed: {:?}", state.status());
            };
            connection.0
        };
        let mut handle = -1;
        // SAFETY: output is writable, and free consumes the operation creator.
        unsafe {
            if take {
                assert_eq!(
                    hew_async_io_take_handle(operation, &raw mut handle),
                    AsyncIoStatus::Success as i32
                );
                assert_eq!(handle, i64::from(accepted));
            }
            hew_async_io_free(operation);
        }
        assert_eq!(
            crate::transport::tcp_streams_has_handle_for_test(accepted),
            take
        );
        if take {
            assert_eq!(crate::transport::hew_tcp_close(accepted), 0);
        }
        assert_eq!(peer.read(&mut [0u8; 1]).unwrap(), 0);
        assert_eq!(crate::transport::hew_tcp_listener_close(listener), 0);
    }
}

#[test]
fn tcp_busy_and_invalid_handle_errors_do_not_replace_the_pending_owner() {
    let _runtime = crate::runtime_test_guard();
    let _reactor = StopReactor;
    let (handle, mut peer) = crate::transport::tcp_socketpair_conn_for_test();
    let signal = Arc::new(ReadySignal::default());
    // SAFETY: the connection is live; the first operation remains pending.
    let first = unsafe { hew_async_tcp_read(handle, &descriptor(&signal)) };
    // SAFETY: the negative probes own separate operations and use no callbacks.
    unsafe {
        let duplicate = hew_async_tcp_read(handle, ptr::null());
        assert_eq!(hew_async_io_status(duplicate), AsyncIoStatus::Error as i32);
        assert_eq!(hew_async_io_errno(duplicate), libc::EBUSY);
        hew_async_io_free(duplicate);
        let invalid = hew_async_tcp_accept(-1, ptr::null());
        assert_eq!(hew_async_io_status(invalid), AsyncIoStatus::Error as i32);
        assert_eq!(hew_async_io_errno(invalid), libc::EBADF);
        hew_async_io_free(invalid);
    }
    peer.write_all(b"original owner").unwrap();
    await_ready(&signal);
    // SAFETY: admission refusal must leave this first operation intact.
    assert_eq!(unsafe { take_read(first) }, b"original owner");
    assert_eq!(crate::transport::hew_tcp_close(handle), 0);
}

#[test]
fn reactor_shutdown_cancels_and_notifies_a_standalone_read() {
    let _runtime = crate::runtime_test_guard();
    let (handle, _peer) = crate::transport::tcp_socketpair_conn_for_test();
    let signal = Arc::new(ReadySignal::default());
    // SAFETY: no data is sent; shutdown must resolve the pending operation.
    let operation = unsafe { hew_async_tcp_read(handle, &descriptor(&signal)) };
    crate::reactor::reactor_shutdown();
    await_ready(&signal);
    // SAFETY: the creator reference outlives shutdown and is consumed once.
    unsafe {
        assert_eq!(
            hew_async_io_status(operation),
            AsyncIoStatus::Cancelled as i32
        );
        assert_eq!(hew_async_io_errno(operation), libc::ECANCELED);
        hew_async_io_free(operation);
    }
    assert_eq!(*signal.notifications.lock().unwrap(), 1);
    assert_eq!(crate::transport::hew_tcp_close(handle), 0);
}

#[test]
fn cleanup_waits_for_the_last_readiness_snapshot_and_discards_late_accept() {
    let _runtime = crate::runtime_test_guard();
    let cleanup = Arc::new(ReadySignal::default());
    // SAFETY: a null readiness descriptor requests polling.
    let operation = unsafe { HewAsyncIo::new(ptr::null()) };
    let registration = IoProducer::new(Arc::clone(&operation));
    let snapshot = registration.clone();
    let operation = Arc::into_raw(operation);
    // SAFETY: the creator remains owned throughout cancellation and cleanup.
    unsafe {
        assert_eq!(hew_async_io_cancel(operation), 1);
        assert_eq!(
            hew_async_io_cleanup_status(operation, &descriptor(&cleanup)),
            0
        );
    }
    drop(registration);
    assert_eq!(*cleanup.notifications.lock().unwrap(), 0);
    let (handle, _peer) = crate::transport::tcp_socketpair_conn_for_test();
    snapshot.complete(Ok(IoValue::Connection(AcceptedConnection(handle))));
    assert!(crate::transport::tcp_conn_raw_fd(handle).is_none());
    drop(snapshot);
    await_ready(&cleanup);
    // SAFETY: cleanup is finished; free consumes the sole creator reference.
    unsafe {
        assert_eq!(hew_async_io_cleanup_status(operation, ptr::null()), 1);
        hew_async_io_free(operation);
    }
    assert_eq!(*cleanup.notifications.lock().unwrap(), 1);
    assert_eq!(Arc::strong_count(&cleanup), 1);
}

#[cfg(unix)]
#[test]
fn cancelled_running_file_read_drains_before_its_cleanup_wake() {
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::OpenOptionsExt;

    let _runtime = crate::runtime_test_guard();
    let directory = tempfile::tempdir().unwrap();
    let fifo = directory.path().join("running-read");
    let raw_path = std::ffi::CString::new(fifo.as_os_str().as_bytes()).unwrap();
    // SAFETY: the C path is terminated and lives through creation of this FIFO.
    assert_eq!(unsafe { libc::mkfifo(raw_path.as_ptr(), 0o600) }, 0);
    let path = string_from_str(fifo.to_str().unwrap());
    let readiness = Arc::new(ReadySignal::default());
    // SAFETY: start copies path and retains the heap readiness descriptor.
    let operation = unsafe {
        let operation = hew_async_file_read(path, &descriptor(&readiness));
        string_release(path);
        operation
    };
    let deadline = Instant::now() + Duration::from_secs(5);
    let mut writer = loop {
        match std::fs::OpenOptions::new()
            .write(true)
            .custom_flags(libc::O_NONBLOCK)
            .open(&fifo)
        {
            Ok(writer) => break writer,
            Err(error) if error.raw_os_error() == Some(libc::ENXIO) => {
                assert!(Instant::now() < deadline, "file worker never opened FIFO");
                std::thread::yield_now();
            }
            Err(error) => panic!("open FIFO writer: {error}"),
        }
    };
    // Opening the writer proves the reader entered the OS call. Keeping it
    // open prevents read-to-end from finishing, independently of scheduling.
    writer.write_all(b"partial contents").unwrap();
    let cleanup = Arc::new(ReadySignal::default());
    // SAFETY: the creator remains live until the producer has finished.
    unsafe {
        assert_eq!(hew_async_io_cancel(operation), 1);
        assert_eq!(
            hew_async_io_status(operation),
            AsyncIoStatus::Cancelled as i32
        );
        assert_eq!(
            hew_async_io_cleanup_status(operation, &descriptor(&cleanup)),
            0
        );
    }
    assert_eq!(*cleanup.notifications.lock().unwrap(), 0);
    drop(writer);
    await_ready(&cleanup);
    // SAFETY: cleanup notification proves the producer released the FIFO read.
    unsafe {
        assert_eq!(hew_async_io_cleanup_status(operation, ptr::null()), 1);
        assert_eq!(
            hew_async_io_status(operation),
            AsyncIoStatus::Cancelled as i32
        );
        hew_async_io_free(operation);
    }
    assert_eq!(*readiness.notifications.lock().unwrap(), 0);
    assert_eq!(*cleanup.notifications.lock().unwrap(), 1);
}
