//! TCP operations on reactor slots.
//!
//! The syscall runs on the waiting task's worker. Submission tries it at once;
//! only a `WouldBlock` stores the operation as the slot's waiter and arms one
//! readiness report. After the report, the task's next status poll tries
//! again, so a spurious report costs one attempt and a re-arm.

use std::cell::RefCell;
use std::ffi::c_int;
use std::io;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

use super::{AcceptedConnection, HewAsyncIo, IoFailure, IoValue};
use crate::reactor::{Direction, Slot};
use crate::util::MutexExt;
use crate::wake::HewWaker;

/// The readiness half of an operation: its slot, what it does when the
/// socket is ready, and whether the reactor has reported readiness.
pub(super) struct NetOp {
    pub(super) slot: Arc<Slot>,
    direction: Direction,
    action: Mutex<Action>,
    pub(super) ready: AtomicBool,
    /// Started by work that shutdown drains, not by a process root.
    pub(super) holds_drain: bool,
}

enum Action {
    Read,
    /// Completes once a read would make progress, consuming nothing: a
    /// selection's watch on a socket stream it must not read.
    Readable,
    Accept,
    /// One line of standard input, taken from the process buffer.
    StdinLine,
    Write {
        data: WriteData,
        written: usize,
    },
}

enum WriteData {
    Owned(Vec<u8>),
    /// The caller's bytes, borrowed until the operation is freed. Only the
    /// owning task writes from them, inside its own status poll.
    Borrowed {
        ptr: *const u8,
        len: usize,
    },
}

// SAFETY: the borrowed region is read only by the owning task, which keeps it
// live and unchanged until it frees the operation.
unsafe impl Send for WriteData {}

impl WriteData {
    fn bytes(&self) -> &[u8] {
        match self {
            Self::Owned(bytes) => bytes,
            // SAFETY: see the variant's contract.
            Self::Borrowed { ptr, len } => unsafe { std::slice::from_raw_parts(*ptr, *len) },
        }
    }
}

enum Attempt {
    Done(Result<IoValue, IoFailure>),
    /// The syscall would block; `progressed` reports a partial write.
    Blocked {
        progressed: bool,
    },
}

/// The most one read takes from the kernel.
const READ_CHUNK: usize = 64 * 1024;

/// The most one nonblocking write offers the kernel. Windows accepts a whole
/// write of any size while its send buffer is not yet full, so an unbounded
/// write to a peer that never reads completes at once and no backpressure is
/// ever observed; bounded writes fill the buffer and meet `WouldBlock` on every
/// platform, with Windows overcommitting at most one chunk.
const WRITE_CHUNK: usize = 1024 * 1024;

thread_local! {
    static READ_BUFFER: RefCell<Box<[u8]>> = RefCell::new(vec![0; READ_CHUNK].into_boxed_slice());
}

fn closed(operation: &str) -> Attempt {
    Attempt::Done(Err(IoFailure::from_io(
        operation,
        &io::Error::from_raw_os_error(libc::EBADF),
    )))
}

/// A failed syscall; only socket errors count toward the TCP counters.
fn failed(tcp: bool, operation: &str, error: &io::Error) -> Attempt {
    if tcp {
        crate::transport::record_tcp_error_kind(error.kind());
    }
    Attempt::Done(Err(IoFailure::from_io(operation, error)))
}

fn attempt(slot: &Slot, action: &mut Action) -> Attempt {
    if slot.is_closed() {
        return Attempt::Done(Err(crate::reactor::cancelled_failure(
            "I/O handle closed while waiting",
        )));
    }
    match action {
        Action::Read => {
            let Some(channel) = slot.bytes() else {
                return closed(&format!("read {}", slot.describe()));
            };
            READ_BUFFER.with_borrow_mut(|buffer| loop {
                match channel.read(buffer) {
                    Ok(count) => {
                        if channel.is_tcp() {
                            crate::transport::count_read(count);
                        }
                        // An empty read is end of stream.
                        return Attempt::Done(Ok(IoValue::Bytes(buffer[..count].to_vec())));
                    }
                    Err(error) if error.kind() == io::ErrorKind::WouldBlock => {
                        return Attempt::Blocked { progressed: false };
                    }
                    Err(error) if error.kind() == io::ErrorKind::Interrupted => {}
                    Err(error) => {
                        let operation = format!("read {}", slot.describe());
                        return failed(channel.is_tcp(), &operation, &error);
                    }
                }
            })
        }
        Action::Readable => {
            // Data, end of stream, an error or a pending connection: the next
            // read or accept will not wait.
            if slot.readable() {
                Attempt::Done(Ok(IoValue::Count(0)))
            } else {
                Attempt::Blocked { progressed: false }
            }
        }
        Action::Accept => {
            if crate::reactor::listener_admission_closed() {
                return Attempt::Done(Err(crate::reactor::cancelled_failure(
                    "accept TCP connection",
                )));
            }
            let Some(listener) = slot.listener() else {
                return closed("accept TCP connection");
            };
            match accept_ready(listener) {
                Ok(Some(handle)) => {
                    Attempt::Done(Ok(IoValue::Connection(AcceptedConnection(handle))))
                }
                Ok(None) => Attempt::Blocked { progressed: false },
                Err(error) => failed(true, "accept TCP connection", &error),
            }
        }
        Action::StdinLine => unreachable!("standard input advances under its buffer lock"),
        Action::Write { data, written } => {
            let Some(channel) = slot.bytes() else {
                return closed(&format!("write {}", slot.describe()));
            };
            let bytes = data.bytes();
            let mut progressed = false;
            while *written < bytes.len() {
                let end = bytes.len().min(*written + WRITE_CHUNK);
                match channel.write(&bytes[*written..end]) {
                    Ok(0) => {
                        return Attempt::Done(Err(IoFailure::from_io(
                            &format!("write {}", slot.describe()),
                            &io::Error::new(io::ErrorKind::WriteZero, "socket accepted no bytes"),
                        )))
                    }
                    Ok(count) => {
                        if channel.is_tcp() {
                            crate::transport::count_written(count);
                        }
                        *written += count;
                        progressed = true;
                    }
                    Err(error) if error.kind() == io::ErrorKind::WouldBlock => {
                        return Attempt::Blocked { progressed };
                    }
                    Err(error) if error.kind() == io::ErrorKind::Interrupted => {}
                    Err(error) => {
                        let operation = format!("write {}", slot.describe());
                        return failed(channel.is_tcp(), &operation, &error);
                    }
                }
            }
            Attempt::Done(Ok(IoValue::Count(
                i64::try_from(*written).expect("native write count fits i64"),
            )))
        }
    }
}

/// Accept a connection already pending on a non-blocking listener and give
/// it a slot; `None` when none is pending.
fn accept_ready(listener: &std::net::TcpListener) -> io::Result<Option<c_int>> {
    loop {
        match listener.accept() {
            Ok((stream, _)) => {
                let _ = stream.set_nodelay(true);
                crate::transport::count_accept();
                return Ok(Some(crate::reactor::register(
                    crate::reactor::IoObject::TcpStream(stream),
                )));
            }
            Err(error) if error.kind() == io::ErrorKind::WouldBlock => return Ok(None),
            Err(error) if error.kind() == io::ErrorKind::Interrupted => {}
            Err(error) => return Err(error),
        }
    }
}

/// Accept a connection already pending on `slot`'s listener without waiting:
/// a listener stream's `try_recv`. `None` when none is pending or shutdown
/// has closed listener admission.
pub(crate) fn accept_now(slot: &Slot) -> io::Result<Option<c_int>> {
    if crate::reactor::listener_admission_closed() {
        return Ok(None);
    }
    let Some(listener) = slot.listener() else {
        return Err(io::Error::from_raw_os_error(libc::EBADF));
    };
    slot.ensure_nonblocking()?;
    accept_ready(listener).inspect_err(|error| {
        crate::transport::record_tcp_error_kind(error.kind());
    })
}

impl NetOp {
    /// Bytes of a write's item that reached the OS so far.
    pub(super) fn written(&self) -> Option<usize> {
        match &*self.action.lock_or_recover() {
            Action::Write { written, .. } => Some(*written),
            _ => None,
        }
    }
}

fn io_timed_out() -> IoFailure {
    super::deadline::timed_out("TCP I/O timeout")
}

/// Try the operation's syscall on this worker; on `WouldBlock`, register it
/// as the slot's waiter and arm readiness.
pub(super) fn advance(operation: &Arc<HewAsyncIo>) {
    let net = operation
        .net
        .as_ref()
        .expect("advance runs only on readiness operations");
    if matches!(*net.action.lock_or_recover(), Action::StdinLine) {
        super::stdin::advance(operation, &net.slot);
        return;
    }
    let attempt = attempt(&net.slot, &mut net.action.lock_or_recover());
    match attempt {
        Attempt::Done(result) => operation.complete(result),
        Attempt::Blocked { progressed } => {
            if let Err(error) = net.slot.wait(net.direction, operation) {
                operation.complete(Err(error));
                return;
            }
            // A read deadline counts from the operation's start; a write
            // deadline measures inactivity, so progress restarts it.
            if let Some(timeout) = net.slot.timeout(net.direction) {
                if progressed || !operation.has_deadline() {
                    operation.set_deadline(timeout, io_timed_out);
                }
            }
        }
    }
}

/// Admit a wait on a handle the program already holds. Shutdown does not
/// refuse it: a termination request refuses new root work (connections,
/// listeners, processes), never a wait on an existing handle.
fn admit(slot: &Slot, action: &Action, direction: Direction) -> Result<(), IoFailure> {
    if crate::runtime::rt_current_opt().is_none() {
        return Err(IoFailure::invalid(
            "asynchronous I/O requires an installed runtime",
        ));
    }
    let kind_matches = match action {
        Action::Accept => slot.listener().is_some(),
        Action::StdinLine => slot.is_stdin(),
        Action::Readable => !slot.is_stdin(),
        _ => slot.bytes().is_some(),
    };
    if !kind_matches {
        return Err(IoFailure::from_io(
            &format!("wait on {}", slot.describe()),
            &io::Error::from_raw_os_error(libc::EBADF),
        ));
    }
    // Standard input queues concurrent readers; a socket takes one per direction.
    if !slot.is_stdin() && slot.has_waiter(direction) {
        return Err(IoFailure::from_io(
            "I/O handle already has pending I/O",
            &io::Error::from_raw_os_error(libc::EBUSY),
        ));
    }
    slot.ensure_nonblocking().map_err(|error| {
        IoFailure::from_io(&format!("set {} nonblocking", slot.describe()), &error)
    })
}

unsafe fn start(handle: i32, action: Action, waker: *const HewWaker) -> *const HewAsyncIo {
    let direction = if matches!(action, Action::Write { .. }) {
        Direction::Write
    } else {
        Direction::Read
    };
    let Some(slot) = crate::reactor::lookup(handle) else {
        // SAFETY: the caller borrows a valid descriptor through this call.
        let operation = unsafe { HewAsyncIo::new(waker) };
        operation.complete(Err(IoFailure::from_io(
            "wait on I/O handle",
            &io::Error::from_raw_os_error(libc::EBADF),
        )));
        return Arc::into_raw(operation);
    };
    // SAFETY: the caller's waker contract is forwarded unchanged.
    unsafe { start_on(slot, action, direction, waker) }
}

/// Start a standard-input line read on its slot.
///
/// # Safety
/// `waker` is null or a borrowed valid `HewWaker`; its context is retained.
pub(super) unsafe fn start_stdin_line(
    slot: Arc<Slot>,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller's waker contract is forwarded unchanged.
    unsafe { start_on(slot, Action::StdinLine, Direction::Read, waker) }
}

unsafe fn start_on(
    slot: Arc<Slot>,
    action: Action,
    direction: Direction,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    let admitted = admit(&slot, &action, direction);
    let net = NetOp {
        slot,
        direction,
        action: Mutex::new(action),
        ready: AtomicBool::new(false),
        holds_drain: !crate::coro_root::current_context_is_process_root(),
    };
    // SAFETY: the caller borrows a valid descriptor through this call.
    let operation = unsafe { HewAsyncIo::with_net(waker, Some(net)) };
    match admitted {
        Ok(()) => advance(&operation),
        Err(error) => operation.complete(Err(error)),
    }
    Arc::into_raw(operation)
}

/// Start a one-shot TCP read. EOF is successful empty bytes. The operation
/// holds its connection's slot, so a close while it waits completes it with
/// `ECANCELED` and the socket stays open until the operation is freed.
///
/// # Safety
/// `waker` is null or a borrowed valid `HewWaker`; its context is retained.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tcp_read(
    connection: i32,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller supplies the borrowed readiness descriptor.
    unsafe { start(connection, Action::Read, waker) }
}

/// Watch a connection until a read would make progress, reading nothing. A
/// selection observes a socket stream this way, so a losing arm leaves every
/// byte for the stream's next receive.
///
/// # Safety
/// `waker` is null or a borrowed valid `HewWaker`; its context is retained.
pub(crate) unsafe fn start_tcp_readable(
    connection: i32,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller supplies the borrowed readiness descriptor.
    unsafe { start(connection, Action::Readable, waker) }
}

/// Start a one-shot accept. The operation owns the newly accepted connection
/// until `hew_async_io_take_handle` transfers it to the resume edge. An
/// untaken or late connection is closed when its owning result is discarded.
///
/// # Safety
/// `waker` is null or a borrowed valid `HewWaker`; its context is retained.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tcp_accept(
    listener: i32,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller supplies the borrowed readiness descriptor.
    unsafe { start(listener, Action::Accept, waker) }
}

/// Submit a complete byte write, keeping partial progress across readiness.
/// The bytes stay borrowed until the operation is freed; cancellation can
/// leave a prefix committed to the peer.
///
/// # Safety
/// `data` is null or a valid bytes carrier whose region stays live and
/// unchanged until the operation is freed. `waker` is null or a valid
/// descriptor.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tcp_write(
    connection: i32,
    data: *const crate::bytes::BytesTriple,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller lends the carrier and its region.
    let region = unsafe { data.as_ref() }
        .ok_or_else(|| IoFailure::invalid("TCP write carrier is null"))
        .and_then(|data| {
            if data.len > i32::MAX as u32 || (data.len != 0 && data.ptr.is_null()) {
                return Err(IoFailure::invalid(
                    "TCP write region is invalid or its count exceeds i32",
                ));
            }
            if data.len == 0 {
                return Ok(None);
            }
            Ok(Some(WriteData::Borrowed {
                // SAFETY: the source contract supplies a readable active region.
                ptr: unsafe { data.ptr.add(data.offset as usize) },
                len: data.len as usize,
            }))
        });
    match region {
        Ok(Some(data)) => {
            // SAFETY: the region and waker satisfy this function's contract.
            unsafe { start(connection, Action::Write { data, written: 0 }, waker) }
        }
        result => {
            // SAFETY: the borrowed waker obeys the ordinary submission contract.
            let operation = unsafe { HewAsyncIo::new(waker) };
            operation.complete(result.map(|_| IoValue::Count(0)));
            Arc::into_raw(operation)
        }
    }
}

/// Submit an owned stream envelope through the same TCP readiness authority.
///
/// # Safety
/// `waker` is null or a valid descriptor.
pub(crate) unsafe fn start_tcp_stream_write(
    connection: i32,
    bytes: Vec<u8>,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    if bytes.is_empty() {
        // SAFETY: caller lends the descriptor during operation construction.
        let operation = unsafe { HewAsyncIo::new(waker) };
        operation.complete(Ok(IoValue::Count(0)));
        Arc::into_raw(operation)
    } else {
        let action = Action::Write {
            data: WriteData::Owned(bytes),
            written: 0,
        };
        // SAFETY: the caller supplies the borrowed readiness descriptor.
        unsafe { start(connection, action, waker) }
    }
}
