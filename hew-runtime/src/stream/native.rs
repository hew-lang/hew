//! Owned native stream operations. Queue observations are withdrawn on drop;
//! content I/O keeps its source loan until the blocking producer is quiescent.

use std::ffi::c_void;
use std::sync::Arc;

use hew_cabi::vec::{HewTypeOwnershipKind, HewValueLayout};

use super::{HewSink, HewStream};
#[cfg(not(target_arch = "wasm32"))]
use crate::async_io::{self, HewAsyncIo, IoFailure};
use crate::channel_common::{
    decode_elem_envelope, drop_elem_envelope, move_elem_envelope, move_elem_layout_witness,
};
use crate::channel_core::ChannelCore;
use crate::wake::{HewWaker, OwnedWaker};

/// What one stream operation waits on.
///
/// A pipe waits on the shared queue core. A file or socket waits on the I/O
/// reactor's request, which only the targets that compile the reactor have -
/// `filesystem-streams` and `tcp-networking` are manifest rejects on wasm32, so
/// an admitted wasm32 program never reaches that backing. A write that began on
/// an already finished sink waits on nothing: the poll reports the closed
/// status and `Drop` releases the element the operation took.
enum Backing {
    Pipe(Arc<ChannelCore>),
    #[cfg(not(target_arch = "wasm32"))]
    Io(*const HewAsyncIo),
    Finished,
}

pub struct HewNativeStream {
    layout: HewValueLayout,
    waker: Arc<OwnedWaker>,
    backing: Backing,
    envelope: Option<Vec<u8>>,
}

impl Drop for HewNativeStream {
    fn drop(&mut self) {
        if let Some(envelope) = self.envelope.take() {
            drop_elem_envelope(Some(&self.layout), envelope, "native stream drop");
        }
        #[cfg(not(target_arch = "wasm32"))]
        if let Backing::Io(io) = self.backing {
            // SAFETY: this operation owns the creator reference; generated
            // cleanup waits for producer quiescence before releasing borrowed
            // handles.
            unsafe { async_io::hew_async_io_free(io) };
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn content_layout(layout: &HewValueLayout) {
    if !matches!(
        layout.ownership_kind,
        HewTypeOwnershipKind::String | HewTypeOwnershipKind::Bytes
    ) {
        crate::channel_common::abort_elem_witness(
            "native content stream",
            "content backing requires a string or bytes witness",
        );
    }
}

/// # Safety
/// The exclusive source loan survives this operation and its producer drain.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn read_content(stream: *mut HewStream) -> Result<Option<Vec<u8>>, IoFailure> {
    let _ = super::take_last_error();
    // SAFETY: the producer owns the exclusive heap-handle loan.
    let item = unsafe { (*stream).inner.next() };
    match super::take_last_error() {
        Some(message) => Err(IoFailure::invalid(&message)),
        None => Ok(item),
    }
}

/// # Safety
/// The exclusive sink loan survives this operation and its producer drain.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn write_content(sink: *mut HewSink, data: &[u8]) -> Result<(), IoFailure> {
    let _ = super::take_last_error();
    // SAFETY: the producer owns the exclusive heap-handle loan.
    unsafe { (*sink).write_item(data) };
    match super::take_last_error() {
        Some(message) => Err(IoFailure::invalid(&message)),
        None => Ok(()),
    }
}

/// Begin one receive without blocking a scheduler worker.
///
/// # Safety
/// The stream is exclusively borrowed until cleanup reports quiescence;
/// layout is static and waker is live during this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_read_start_native(
    stream: *mut HewStream,
    waker: *const HewWaker,
    layout: *const HewValueLayout,
) -> *mut HewNativeStream {
    // SAFETY: compiler-provided live operands satisfy the start contract.
    let layout = unsafe { *move_elem_layout_witness(layout, "native stream read") };
    // SAFETY: stream is a live exclusive loan.
    let backing = match unsafe { (*stream).channel.clone() } {
        Some(core) => Backing::Pipe(core),
        #[cfg(not(target_arch = "wasm32"))]
        None => {
            content_layout(&layout);
            // SAFETY: the exclusive stream loan remains live during inspection.
            let io = if let Some(connection) = unsafe { (*stream).inner.native_connection() } {
                // SAFETY: generated cleanup retains the transport loan through quiescence.
                unsafe { async_io::hew_async_tcp_read(connection, waker) }
            } else {
                // SAFETY: generated cleanup retains stream through producer quiescence.
                unsafe { async_io::start_stream_read(stream, waker) }
            };
            Backing::Io(io)
        }
        #[cfg(target_arch = "wasm32")]
        None => reactor_backing_unreachable("read"),
    };
    Box::into_raw(Box::new(HewNativeStream {
        layout,
        // SAFETY: waker is borrowed during this call.
        waker: Arc::new(unsafe { OwnedWaker::retain(&*waker) }),
        backing,
        envelope: None,
    }))
}

/// Begin one write, taking the value before parking on bounded capacity.
///
/// # Safety
/// The sink loan survives through quiescence. Data is one owned element;
/// layout is static and waker is live during this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_write_start_native(
    sink: *mut HewSink,
    waker: *const HewWaker,
    data: *mut c_void,
    layout: *const HewValueLayout,
) -> *mut HewNativeStream {
    // SAFETY: the compiler provides a static descriptor and a live owned value.
    let layout = unsafe { *move_elem_layout_witness(layout, "native stream write") };
    // SAFETY: the source value transfers exactly once at operation construction.
    let envelope = unsafe { move_elem_envelope(data, &layout, "native stream write") };
    // A finished sink has released its backing, so there is nothing left to
    // inspect for a queue or a transport. Report the closed status the write
    // poll already carries; `Drop` releases the element this operation took.
    // SAFETY: sink is a live exclusive loan.
    if unsafe { (*sink).is_closed() } {
        return Box::into_raw(Box::new(HewNativeStream {
            layout,
            // SAFETY: waker is borrowed during this call.
            waker: Arc::new(unsafe { OwnedWaker::retain(&*waker) }),
            backing: Backing::Finished,
            envelope: Some(envelope),
        }));
    }
    // SAFETY: sink is a live exclusive loan.
    let raw = unsafe { (*sink).channel_core_ptr().cast::<ChannelCore>() };
    let core = if raw.is_null() {
        None
    } else {
        // SAFETY: the sink's backing owns the Arc to which this pointer refers.
        unsafe { Arc::increment_strong_count(raw) };
        // SAFETY: the increment above creates this operation's independent owner.
        Some(unsafe { Arc::from_raw(raw) })
    };
    let (backing, envelope) = match core {
        Some(core) => {
            core.stamp_elem_layout(&layout);
            (Backing::Pipe(core), Some(envelope))
        }
        #[cfg(not(target_arch = "wasm32"))]
        None => {
            content_layout(&layout);
            // SAFETY: the exclusive sink loan remains live during inspection.
            let io = if let Some(connection) = unsafe { (*sink).native_connection() } {
                // SAFETY: generated cleanup retains the transport loan through quiescence.
                unsafe { async_io::start_tcp_stream_write(connection, envelope, waker) }
            } else {
                // SAFETY: generated cleanup retains sink through producer quiescence.
                unsafe { async_io::start_sink_write(sink, envelope, waker) }
            };
            (Backing::Io(io), None)
        }
        #[cfg(target_arch = "wasm32")]
        None => reactor_backing_unreachable("write"),
    };
    Box::into_raw(Box::new(HewNativeStream {
        layout,
        // SAFETY: waker is borrowed during this call.
        waker: Arc::new(unsafe { OwnedWaker::retain(&*waker) }),
        backing,
        envelope,
    }))
}

/// Poll a receive: 0 pending, 1 value, 2 EOF, 3 failure.
///
/// # Safety
/// Operation is a live exclusive read operation; out is one writable element.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_read_poll_native(operation: *mut HewNativeStream) -> i32 {
    // SAFETY: the caller lends the live exclusive operation.
    let operation = unsafe { &mut *operation };
    let (status, item) = match &operation.backing {
        Backing::Pipe(core) => core.poll_recv_observed(&operation.waker),
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => {
            // SAFETY: the operation owns this live async request.
            unsafe { async_io::take_stream_item(*io) }
        }
        // A read never begins on a finished backing; report end of data.
        Backing::Finished => (2, None),
    };
    if status == 1 {
        operation.envelope = item;
    }
    status
}

/// Transfer one ready element after the producer is quiescent.
///
/// # Safety
/// Operation has a ready read and out is one writable element slot.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_read_take_native(
    operation: *mut HewNativeStream,
    out: *mut c_void,
) -> i32 {
    // SAFETY: the caller lends the live exclusive operation.
    let operation = unsafe { &mut *operation };
    let item = operation.envelope.take();
    // SAFETY: out receives the descriptor's sole owned value.
    unsafe { decode_elem_envelope(item, out, &operation.layout, "native stream read") }
}

/// Poll a write: 0 pending, 1 transferred, 2 closed, 3 failure.
///
/// # Safety
/// Operation is a live exclusive write operation.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_write_poll_native(operation: *mut HewNativeStream) -> i32 {
    // SAFETY: the caller lends the live exclusive operation.
    let operation = unsafe { &mut *operation };
    match &operation.backing {
        Backing::Pipe(core) => {
            let Some(envelope) = operation.envelope.take() else {
                return 1;
            };
            let (status, envelope) = core.poll_send_observed(&operation.waker, envelope);
            operation.envelope = envelope;
            status
        }
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => {
            // SAFETY: this operation owns the live async request.
            match unsafe { async_io::hew_async_io_status(*io) } {
                0 => 0,
                1 => 1,
                _ => 3,
            }
        }
        Backing::Finished => 2,
    }
}

/// # Safety
/// Operation is live; cancellation precedes draining a pending operation.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_cancel_native(operation: *mut HewNativeStream) {
    // SAFETY: the caller lends the live exclusive operation.
    match unsafe { &(*operation).backing } {
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => {
            // SAFETY: this operation owns the async request.
            unsafe { async_io::hew_async_io_cancel(*io) };
        }
        // A pipe registration is withdrawn by the read slot the await entry
        // parked on, not here.
        Backing::Pipe(_) | Backing::Finished => {}
    }
}

/// # Safety
/// Operation is live and waker obeys the borrowed descriptor contract.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_cleanup_status_native(
    operation: *mut HewNativeStream,
    waker: *const HewWaker,
) -> i32 {
    // SAFETY: the caller lends the live exclusive operation.
    match unsafe { &(*operation).backing } {
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => {
            // SAFETY: this operation owns the async request.
            unsafe { async_io::hew_async_io_cleanup_status(*io, waker) }
        }
        // Nothing borrowed a handle beyond this call, so the operation is
        // already quiescent.
        Backing::Pipe(_) | Backing::Finished => {
            let _ = waker;
            1
        }
    }
}

/// # Safety
/// Operation is uniquely owned and its producer cleanup status is ready.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_operation_free_native(operation: *mut HewNativeStream) {
    // SAFETY: caller transfers the drained operation's unique Box.
    drop(unsafe { Box::from_raw(operation) });
}

/// A stream with no queue core is a file or a socket, and both are manifest
/// rejects on wasm32. Reaching here means a lowering defect rather than a
/// program outcome, so the module fails closed.
#[cfg(target_arch = "wasm32")]
fn reactor_backing_unreachable(operation: &str) -> ! {
    eprintln!("hew: fail-closed: wasm32 stream {operation} with no pipe backing");
    std::process::abort();
}

#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn retain_thread(context: *mut c_void) {
    // SAFETY: the synchronous adapter lends a live Arc<Thread> descriptor.
    unsafe { Arc::increment_strong_count(context.cast::<std::thread::Thread>()) };
}

#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn release_thread(context: *mut c_void) {
    // SAFETY: this consumes one reference retained by the operation.
    unsafe { Arc::decrement_strong_count(context.cast::<std::thread::Thread>()) };
}

#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn notify_thread(context: *mut c_void) {
    // SAFETY: the operation retains the thread through notification.
    unsafe { &*context.cast::<std::thread::Thread>() }.unpark();
}

/// Synchronous C ABI stream entries wait on the same readiness operation.
/// Native coroutine entries above retain their ordinary suspend/resume path.
#[cfg(not(target_arch = "wasm32"))]
fn wait_tcp(start: impl FnOnce(&HewWaker) -> *const HewAsyncIo) -> *const HewAsyncIo {
    let thread = Arc::new(std::thread::current());
    let waker = HewWaker {
        context: Arc::as_ptr(&thread).cast_mut().cast(),
        wake: notify_thread,
        retain: retain_thread,
        release: release_thread,
    };
    let operation = start(&waker);
    // SAFETY: the start closure transfers its live creator reference; the
    // blocking caller retains the backing until both readiness and quiescence.
    unsafe {
        while async_io::hew_async_io_status(operation) == 0 {
            std::thread::park();
        }
        while async_io::hew_async_io_cleanup_status(operation, &raw const waker) == 0 {
            std::thread::park();
        }
        async_io::hew_async_io_restore_error(operation);
    }
    operation
}

#[cfg(not(target_arch = "wasm32"))]
pub(super) fn blocking_tcp_read(connection: i32) -> Option<Vec<u8>> {
    let operation = wait_tcp(|waker| {
        // SAFETY: the synchronous backing retains its borrowed connection.
        unsafe { async_io::hew_async_tcp_read(connection, waker) }
    });
    // SAFETY: wait_tcp transfers a ready, drained operation reference.
    unsafe {
        let (_, item) = async_io::take_stream_item(operation);
        async_io::hew_async_io_free(operation);
        item
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub(super) fn blocking_tcp_write(connection: i32, content: &[u8]) {
    let operation = wait_tcp(|waker| {
        // SAFETY: the synchronous backing retains its borrowed connection.
        unsafe { async_io::start_tcp_stream_write(connection, content.to_vec(), waker) }
    });
    // SAFETY: wait_tcp transfers a ready, drained operation reference.
    unsafe { async_io::hew_async_io_free(operation) };
}

#[cfg(test)]
pub(super) struct TestReactor;

#[cfg(test)]
impl TestReactor {
    pub(super) fn new() -> Self {
        // The worker-less runtime test guard does not run scheduler startup,
        // which reopens admission after an earlier runtime's shutdown.
        crate::reactor::reset_listener_admission();
        Self
    }
}

#[cfg(test)]
impl Drop for TestReactor {
    fn drop(&mut self) {
        crate::reactor::reactor_shutdown();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::mpsc;
    use std::time::Duration;

    unsafe extern "C" fn retain(context: *mut c_void) {
        // SAFETY: each test descriptor starts with a live Arc sender.
        unsafe { Arc::increment_strong_count(context.cast::<mpsc::Sender<()>>()) };
    }

    unsafe extern "C" fn release(context: *mut c_void) {
        // SAFETY: every release consumes one retained callback reference.
        unsafe { Arc::decrement_strong_count(context.cast::<mpsc::Sender<()>>()) };
    }

    unsafe extern "C" fn notify(context: *mut c_void) {
        // SAFETY: OwnedWaker retains this sender through notification.
        let sender = unsafe { &*context.cast::<mpsc::Sender<()>>() };
        let _ = sender.send(());
    }

    fn waker(sender: &Arc<mpsc::Sender<()>>) -> HewWaker {
        HewWaker {
            context: Arc::as_ptr(sender).cast_mut().cast(),
            wake: notify,
            retain,
            release,
        }
    }

    unsafe extern "C-unwind" fn drop_owned(slot: *mut c_void) {
        // SAFETY: the descriptor describes one owned Box of a shared counter.
        let pointer = unsafe { *slot.cast::<*mut Arc<AtomicUsize>>() };
        // SAFETY: the envelope is this Box's sole owner.
        let counter = unsafe { Box::from_raw(pointer) };
        counter.fetch_add(1, Ordering::SeqCst);
    }

    fn owned_layout() -> HewValueLayout {
        HewValueLayout {
            size: size_of::<usize>(),
            align: align_of::<usize>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: Some(drop_owned),
            visit_close: None,
            release_start: None,
        }
    }

    #[test]
    fn cancelled_pipe_write_releases_unsent_owner_and_observation() {
        let (signal, notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let drops = Arc::new(AtomicUsize::new(0));
        let layout = owned_layout();
        let mut sink = std::ptr::null_mut();
        // SAFETY: every slot/handle remains live until its sole owner is released.
        unsafe {
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            let mut first = Box::into_raw(Box::new(Arc::clone(&drops)));
            let first_op = hew_stream_write_start_native(
                sink,
                &raw const wake,
                (&raw mut first).cast(),
                &raw const layout,
            );
            assert_eq!(hew_stream_write_poll_native(first_op), 1);
            hew_stream_operation_free_native(first_op);
            let mut second = Box::into_raw(Box::new(Arc::clone(&drops)));
            let second_op = hew_stream_write_start_native(
                sink,
                &raw const wake,
                (&raw mut second).cast(),
                &raw const layout,
            );
            assert_eq!(hew_stream_write_poll_native(second_op), 0);
            hew_stream_cancel_native(second_op);
            assert_eq!(
                hew_stream_cleanup_status_native(second_op, &raw const wake),
                1
            );
            hew_stream_operation_free_native(second_op);
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            assert_eq!(Arc::strong_count(&signal), 1);
            drop(Box::from_raw(stream));
            drop(Box::from_raw(sink));
        }
        assert_eq!(drops.load(Ordering::SeqCst), 2);
        assert!(
            notifications.try_recv().is_err(),
            "abandoned producer must not wake"
        );
    }

    #[derive(Debug)]
    struct GatedContent {
        entered: mpsc::Sender<()>,
        release: mpsc::Receiver<()>,
        closed: Arc<AtomicUsize>,
    }

    impl super::super::StreamBacking for GatedContent {
        fn next(&mut self) -> Option<Vec<u8>> {
            self.entered.send(()).unwrap();
            self.release.recv().unwrap();
            Some(Vec::new())
        }
        fn close(&mut self) {
            self.closed.fetch_add(1, Ordering::SeqCst);
        }
        fn is_closed(&self) -> bool {
            false
        }
    }

    struct ReleaseContent(mpsc::Sender<()>);
    impl Drop for ReleaseContent {
        fn drop(&mut self) {
            let _ = self.0.send(());
        }
    }

    unsafe extern "C-unwind" fn drop_string(slot: *mut c_void) {
        // SAFETY: this descriptor's slot contains one managed string owner.
        unsafe { hew_cabi::string::string_release(*slot.cast()) };
    }

    #[test]
    fn cancelled_content_read_retains_loan_until_worker_quiescence() {
        let _runtime = crate::runtime_test_guard();
        let (entered, started) = mpsc::channel();
        let (release, released) = mpsc::channel();
        let release = ReleaseContent(release);
        let closed = Arc::new(AtomicUsize::new(0));
        let stream = super::super::into_stream_ptr(GatedContent {
            entered,
            release: released,
            closed: Arc::clone(&closed),
        });
        let (signal, notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let layout = HewValueLayout {
            ownership_kind: HewTypeOwnershipKind::String,
            drop_fn: Some(drop_string),
            ..owned_layout()
        };
        // SAFETY: the exclusive stream loan survives every producer and operation.
        unsafe {
            let operation =
                hew_stream_read_start_native(stream, &raw const wake, &raw const layout);
            started.recv_timeout(Duration::from_secs(5)).unwrap();
            assert_eq!(hew_stream_read_poll_native(operation), 0);
            hew_stream_cancel_native(operation);
            assert_eq!(
                hew_stream_cleanup_status_native(operation, &raw const wake),
                0
            );
            assert_eq!(closed.load(Ordering::SeqCst), 0);
            drop(release);
            while hew_stream_cleanup_status_native(operation, &raw const wake) == 0 {
                notifications.recv_timeout(Duration::from_secs(5)).unwrap();
            }
            hew_stream_operation_free_native(operation);
            assert_eq!(closed.load(Ordering::SeqCst), 0);
            drop(Box::from_raw(stream));
        }
        assert_eq!(closed.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn cancelled_tcp_read_preserves_socket_for_the_next_receive() {
        use std::io::Write;
        let _runtime = crate::runtime_test_guard();
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let mut peer = std::net::TcpStream::connect(listener.local_addr().unwrap()).unwrap();
        let (socket, _) = listener.accept().unwrap();
        // SAFETY: the fresh stream transfers its sole allocation to this test.
        let mut stream = unsafe {
            Box::from_raw(super::super::into_stream_ptr(
                super::super::TcpStreamBacking::new(socket),
            ))
        };
        // Join readiness work before the stream loan or runtime can be dropped,
        // including when an assertion unwinds this test.
        let _reactor = TestReactor::new();
        let (signal, notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let layout = HewValueLayout {
            ownership_kind: HewTypeOwnershipKind::String,
            drop_fn: Some(drop_string),
            ..owned_layout()
        };
        // SAFETY: every receive exclusively borrows the same live stream until
        // its readiness registration is detached and its producer drains.
        unsafe {
            let mut abandoned = Box::from_raw(hew_stream_read_start_native(
                &raw mut *stream,
                &raw const wake,
                &raw const layout,
            ));
            assert_eq!(hew_stream_read_poll_native(&raw mut *abandoned), 0);
            hew_stream_cancel_native(&raw mut *abandoned);
            while hew_stream_cleanup_status_native(&raw mut *abandoned, &raw const wake) == 0 {
                notifications.recv_timeout(Duration::from_secs(5)).unwrap();
            }
            drop(abandoned);

            let mut next = Box::from_raw(hew_stream_read_start_native(
                &raw mut *stream,
                &raw const wake,
                &raw const layout,
            ));
            peer.write_all(&[0]).unwrap();
            loop {
                match hew_stream_read_poll_native(&raw mut *next) {
                    0 => {
                        notifications.recv_timeout(Duration::from_secs(5)).unwrap();
                    }
                    1 => break,
                    status => panic!("live socket was invalidated by cancellation: {status}"),
                }
            }
            while hew_stream_cleanup_status_native(&raw mut *next, &raw const wake) == 0 {
                notifications.recv_timeout(Duration::from_secs(5)).unwrap();
            }
            let mut output: *mut hew_cabi::string::HewString = std::ptr::null_mut();
            assert_eq!(
                hew_stream_read_take_native(&raw mut *next, (&raw mut output).cast()),
                1
            );
            let bytes = hew_cabi::string::string_as_bytes(output).to_vec();
            hew_cabi::string::string_release(output);
            assert_eq!(bytes, &[0]);
        }
    }
}
