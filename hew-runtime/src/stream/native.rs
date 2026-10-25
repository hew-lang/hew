//! Owned native stream operations. Queue observations are withdrawn on drop;
//! content I/O keeps its source loan until the blocking producer is quiescent.

use std::cell::UnsafeCell;
use std::ffi::c_void;
use std::mem::ManuallyDrop;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

#[cfg(not(target_arch = "wasm32"))]
use hew_cabi::vec::HewTypeOwnershipKind;
use hew_cabi::vec::HewValueLayout;

#[cfg(not(target_arch = "wasm32"))]
use super::NativeRead;
use super::{HewSink, HewStream};
#[cfg(not(target_arch = "wasm32"))]
use crate::async_io::{self, HewAsyncIo, IoFailure};
use crate::channel_common::{
    decode_elem_envelope, drop_elem_envelope, move_elem_envelope, move_elem_layout_witness,
};
use crate::channel_core::ChannelCore;
use crate::wake::{HewWaker, OwnedWaker, WakeTarget};

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
    Shared(Box<super::sender::SharedWrite>),
    Immediate {
        status: i32,
        fault: *mut crate::fault::HewFault,
        release: *mut crate::release_walker::HewReleaseCursor,
    },
    Generator {
        stream: *mut HewStream,
        parent: *mut crate::coro_state::HewCoroState,
        closing: bool,
    },
    Finished,
}

pub struct HewNativeStream {
    waker: OwnedWaker,
    armed: AtomicBool,
    state: UnsafeCell<ManuallyDrop<StreamState>>,
}

// SAFETY: one creator owns all state access under the native operation's
// exclusive poll/cleanup contract. Notification shares only the immutable
// retained waker and atomic arm; it never reads or destroys operation state.
unsafe impl Send for HewNativeStream {}
// SAFETY: concurrent observers use only the immutable notification header.
// Mutable state is accessible solely through the creator's exclusive loan.
unsafe impl Sync for HewNativeStream {}

impl WakeTarget for HewNativeStream {
    fn owned_waker(&self) -> &OwnedWaker {
        &self.waker
    }

    fn wake(&self) {
        if self.armed.load(Ordering::Acquire) {
            self.waker.wake();
        }
    }
}

impl HewNativeStream {
    unsafe fn new(waker: *const HewWaker, state: StreamState) -> *mut Self {
        Arc::into_raw(Arc::new(Self {
            // SAFETY: the caller lends a live descriptor during construction.
            waker: unsafe { OwnedWaker::retain(&*waker) },
            armed: AtomicBool::new(true),
            state: UnsafeCell::new(ManuallyDrop::new(state)),
        }))
        .cast_mut()
    }

    /// Borrow the creator's existing strong reference without incrementing it.
    /// # Safety
    /// The unique creator reference remains live throughout this borrow.
    unsafe fn borrow(operation: *const Self) -> ManuallyDrop<Arc<Self>> {
        // SAFETY: the raw creator owner came from Arc::into_raw. ManuallyDrop
        // prevents this temporary borrow from consuming its strong reference.
        ManuallyDrop::new(unsafe { Arc::from_raw(operation) })
    }
}

struct StreamState {
    layout: HewValueLayout,
    backing: Backing,
    /// What a completed reactor read yields; writes and pipes ignore it.
    #[cfg(not(target_arch = "wasm32"))]
    yields: ReadYield,
    envelope: Option<Vec<u8>>,
    read_stream: Option<*mut HewStream>,
    #[cfg(not(target_arch = "wasm32"))]
    write_prefix: usize,
}

/// The item a reactor read produces, fixed when the read starts.
#[cfg(not(target_arch = "wasm32"))]
#[derive(Clone, Copy)]
enum ReadYield {
    /// String or bytes content.
    Content,
    /// An accepted connection handle.
    Connection,
    /// `()`: the handle turned readable.
    Readiness,
}

impl Drop for StreamState {
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

/// A connection item is its handle's slot image.
#[cfg(not(target_arch = "wasm32"))]
fn connection_layout(layout: &HewValueLayout) {
    if !matches!(layout.size, 4 | 8) {
        crate::channel_common::abort_elem_witness(
            "native incoming stream",
            "an accepted connection needs a handle-sized witness",
        );
    }
}

/// A readiness item is `()`.
#[cfg(not(target_arch = "wasm32"))]
fn unit_layout(layout: &HewValueLayout) {
    if layout.size != 0 || layout.ownership_kind != HewTypeOwnershipKind::Plain {
        crate::channel_common::abort_elem_witness(
            "native readiness stream",
            "a readiness item is `()`",
        );
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
    match IoFailure::take_recorded() {
        Some(failure) => Err(failure),
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
    match IoFailure::take_recorded() {
        Some(failure) => Err(failure),
        None => Ok(()),
    }
}

/// Complete an immediately ready pipe receive without creating a parked
/// operation. Other backings and empty open pipes return pending (0).
/// Returns 1 with an owned element in `out`, 2 at EOF, or 3 with an owned fault.
///
/// # Safety
/// `stream` is exclusively borrowed, `layout` describes its element, and
/// `out`/`fault_out` are disjoint writable output slots. Pending initializes
/// neither element ownership nor a wake registration.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_read_ready_native(
    stream: *mut HewStream,
    layout: *const HewValueLayout,
    out: *mut c_void,
    fault_out: *mut *mut crate::fault::HewFault,
) -> i32 {
    // SAFETY: the caller supplies the same exact witness as a parked receive.
    let layout = unsafe { move_elem_layout_witness(layout, "ready native stream read") };
    // SAFETY: the exclusive loan and output slots survive this call.
    unsafe {
        *fault_out = std::ptr::null_mut();
        if (*stream).exhausted() {
            return 2;
        }
        let Some(core) = (*stream).channel.as_ref() else {
            return 0;
        };
        let (status, item) = core.try_recv_native();
        if status == 1 {
            (*stream).received();
            decode_elem_envelope(item, out, layout, "ready native stream read");
        } else if status == 3 {
            *fault_out = Box::into_raw(Box::new(crate::fault::HewFault::with_message(
                crate::internal::types::HEW_TRAP_USER_PANIC,
                core.fault_message(),
            )));
        }
        status
    }
}

/// # Safety
/// The stream is exclusively borrowed until cleanup reports quiescence;
/// layout is static and waker is live during this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_read_start_native(
    stream: *mut HewStream,
    waker: *const HewWaker,
    layout: *const HewValueLayout,
    parent: *mut crate::coro_state::HewCoroState,
) -> *mut HewNativeStream {
    // SAFETY: compiler-provided live operands satisfy the start contract.
    let layout = unsafe { *move_elem_layout_witness(layout, "native stream read") };
    #[cfg(not(target_arch = "wasm32"))]
    let mut yields = ReadYield::Content;
    // SAFETY: stream is a live exclusive loan.
    let backing = if unsafe { (*stream).exhausted() } {
        Backing::Finished
    } else if unsafe { (*stream).inner.generator().is_some() } {
        Backing::Generator {
            stream,
            parent,
            closing: false,
        }
    } else {
        // SAFETY: the exclusive source loan is live until this request drains.
        match unsafe { (*stream).channel.clone() } {
            Some(core) => Backing::Pipe(core),
            #[cfg(not(target_arch = "wasm32"))]
            None => Backing::Io(unsafe {
                // SAFETY: the exclusive stream loan remains live during inspection;
                // generated cleanup retains it through producer quiescence.
                match (*stream).native_read() {
                    Some(NativeRead::Content(handle)) => {
                        content_layout(&layout);
                        async_io::hew_async_tcp_read(handle, waker)
                    }
                    Some(NativeRead::Accept(listener)) => {
                        connection_layout(&layout);
                        yields = ReadYield::Connection;
                        async_io::hew_async_tcp_accept(listener, waker)
                    }
                    Some(NativeRead::Readiness(handle)) => {
                        unit_layout(&layout);
                        yields = ReadYield::Readiness;
                        async_io::start_tcp_readable(handle, waker)
                    }
                    None => {
                        content_layout(&layout);
                        async_io::start_stream_read(stream, waker)
                    }
                }
            }),
            #[cfg(target_arch = "wasm32")]
            None => reactor_backing_unreachable("read"),
        }
    };
    // SAFETY: the caller lends its live wake descriptor through construction.
    unsafe {
        HewNativeStream::new(
            waker,
            StreamState {
                layout,
                backing,
                #[cfg(not(target_arch = "wasm32"))]
                yields,
                envelope: None,
                read_stream: Some(stream),
                #[cfg(not(target_arch = "wasm32"))]
                write_prefix: 0,
            },
        )
    }
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
    parent: *const crate::coro_state::HewCoroState,
    park: bool,
    sink_layout: *const HewValueLayout,
) -> *mut HewNativeStream {
    // SAFETY: the source read loan pins the endpoint and its exact descriptor.
    unsafe { super::sender::retain(sink, sink_layout, false) };
    // SAFETY: the installed owner serializes every underlying write through terminal cleanup.
    unsafe { start_underlying(sink, waker, data, layout, parent, park) }
}

/// # Safety
/// The caller holds the underlying sender gate, or borrows an installed shared endpoint.
unsafe fn try_underlying(
    sink: *mut HewSink,
    waker: *const HewWaker,
    data: *mut c_void,
    layout: HewValueLayout,
) -> *mut HewNativeStream {
    let mut release = std::ptr::null_mut();
    let mut fault = std::ptr::null_mut();
    // SAFETY: the shared sender gate exclusively lends this raw backing for one capacity attempt.
    let status = unsafe {
        super::try_send_underlying(
            sink,
            data,
            &raw const layout,
            &raw mut release,
            &raw mut fault,
        )
    };
    let status = match status {
        0 => 1,
        1 => 2,
        2 => 4,
        3 => 3,
        _ => std::process::abort(),
    };
    // SAFETY: the caller lends its live wake descriptor through construction.
    unsafe {
        HewNativeStream::new(
            waker,
            StreamState {
                layout,
                backing: Backing::Immediate {
                    status,
                    fault,
                    release,
                },
                #[cfg(not(target_arch = "wasm32"))]
                yields: ReadYield::Content,
                envelope: None,
                read_stream: None,
                #[cfg(not(target_arch = "wasm32"))]
                write_prefix: 0,
            },
        )
    }
}

/// # Safety
/// The caller holds the underlying sender gate, or borrows an installed shared endpoint.
pub(super) unsafe fn start_underlying(
    sink: *mut HewSink,
    waker: *const HewWaker,
    data: *mut c_void,
    layout: *const HewValueLayout,
    parent: *const crate::coro_state::HewCoroState,
    park: bool,
) -> *mut HewNativeStream {
    // SAFETY: the compiler provides a static descriptor and a live owned value.
    let layout = unsafe { *move_elem_layout_witness(layout, "native stream write") };
    // SAFETY: this operation pins the endpoint while inspecting its shared backing.
    let owner = unsafe { super::sender::shared(sink) };
    if !park && owner.is_none() {
        // SAFETY: the shared gate lends this raw backing for one nonparking capacity attempt.
        return unsafe { try_underlying(sink, waker, data, layout) };
    }
    // SAFETY: the source value transfers exactly once at operation construction.
    let envelope = unsafe { move_elem_envelope(data, &layout, "native stream write") };
    // A finished sink has released its backing, so there is nothing left to
    // inspect for a queue or a transport. Report the closed status the write
    // poll already carries; `Drop` releases the element this operation took.
    // SAFETY: sink is a live exclusive loan.
    if unsafe { (*sink).is_closed() } {
        // SAFETY: the caller lends its live wake descriptor through construction.
        return unsafe {
            HewNativeStream::new(
                waker,
                StreamState {
                    layout,
                    backing: Backing::Finished,
                    #[cfg(not(target_arch = "wasm32"))]
                    yields: ReadYield::Content,
                    envelope: Some(envelope),
                    read_stream: None,
                    #[cfg(not(target_arch = "wasm32"))]
                    write_prefix: 0,
                },
            )
        };
    }
    // An unencoded pipe already serializes producers in ChannelCore. Its owned
    // core survives endpoint finish; queue state rejects writes after the last
    // producer closes. It needs neither the transport gate nor a second operation.
    let core = match &owner {
        Some(owner) if park && owner.encoder.is_none() => owner.channel.clone(),
        None => {
            // SAFETY: the operation holds the original endpoint's source loan.
            unsafe { super::clone_sink_channel_core(sink) }
        }
        _ => None,
    };
    // Encoders and transports retain their shared admission gate, including
    // nonparking attempts whose rejection/accepted-remainder policy it owns.
    if let Some(owner) = owner.filter(|_| core.is_none()) {
        // SAFETY: the caller lends its live wake descriptor through construction.
        return unsafe {
            HewNativeStream::new(
                waker,
                StreamState {
                    layout,
                    backing: Backing::Shared(Box::new(super::sender::SharedWrite::new(
                        owner, sink, parent, park,
                    ))),
                    #[cfg(not(target_arch = "wasm32"))]
                    yields: ReadYield::Content,
                    envelope: Some(envelope),
                    read_stream: None,
                    #[cfg(not(target_arch = "wasm32"))]
                    write_prefix: 0,
                },
            )
        };
    }
    #[cfg(not(target_arch = "wasm32"))]
    let mut write_prefix = 0;
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
                let mut bytes = unsafe { (*sink).take_pending() };
                write_prefix = bytes.len();
                bytes.extend_from_slice(&envelope);
                // SAFETY: the caller retains the sink loan and borrowed waker.
                unsafe { async_io::start_tcp_stream_write(connection, bytes, waker) }
            } else {
                // SAFETY: generated cleanup retains sink through producer quiescence.
                unsafe { async_io::start_sink_write(sink, envelope, waker) }
            };
            (Backing::Io(io), None)
        }
        #[cfg(target_arch = "wasm32")]
        None => reactor_backing_unreachable("write"),
    };
    // SAFETY: the caller lends its live wake descriptor through construction.
    unsafe {
        HewNativeStream::new(
            waker,
            StreamState {
                layout,
                backing,
                #[cfg(not(target_arch = "wasm32"))]
                yields: ReadYield::Content,
                envelope,
                read_stream: None,
                #[cfg(not(target_arch = "wasm32"))]
                write_prefix,
            },
        )
    }
}

/// Poll a receive: 0 pending, 1 value, 2 EOF, 3 failure.
///
/// # Safety
/// Operation is a live exclusive read operation; out is one writable element.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_read_poll_native(operation: *mut HewNativeStream) -> i32 {
    // SAFETY: the caller lends the live exclusive operation.
    let owner = unsafe { HewNativeStream::borrow(operation) };
    // SAFETY: only the creator accesses state; notifications share the header.
    let operation = unsafe { &mut **owner.state.get() };
    let (status, item) = match &operation.backing {
        Backing::Pipe(core) => core.poll_recv_observed(&owner),
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => unsafe {
            // SAFETY: the operation owns this live async request.
            match operation.yields {
                ReadYield::Content => async_io::take_stream_item(*io),
                ReadYield::Connection => async_io::take_accepted_item(*io, operation.layout.size),
                ReadYield::Readiness => async_io::take_readiness_item(*io),
            }
        },
        Backing::Shared(_) | Backing::Immediate { .. } => std::process::abort(),
        Backing::Generator { stream, parent, .. } => {
            // SAFETY: the operation retains the exclusive stream and parent loans.
            unsafe {
                (**stream)
                    .inner
                    .generator()
                    .expect("generator backing")
                    .poll(*parent, &operation.layout)
            }
        }
        // A read never begins on a finished backing; report end of data.
        Backing::Finished => (2, None),
    };
    if status == 1 {
        if let Some(stream) = operation.read_stream.take() {
            // SAFETY: the operation holds its source loan through cleanup.
            unsafe { (*stream).received() };
        }
        operation.envelope = item;
    }
    status
}

/// The fault a receive or send reports after its poll answered failure (3):
/// the faulted pipe or the I/O error, never a bare code.
///
/// # Safety
/// Operation is a live operation whose last poll reported failure.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_operation_fault_native(
    operation: *mut HewNativeStream,
) -> *mut crate::fault::HewFault {
    // SAFETY: the creator lends exclusive state; notifications use only the header.
    let operation = unsafe { &mut **(*operation).state.get() };
    // SAFETY: the caller holds this operation's exclusive poll loan.
    if let Backing::Shared(write) = &mut operation.backing {
        // SAFETY: this owned write reported a terminal failure.
        return unsafe { write.take_fault() };
    }
    // SAFETY: the operation exclusively owns this immediate terminal fault.
    if let Backing::Immediate { fault, .. } = &mut operation.backing {
        return std::mem::replace(fault, std::ptr::null_mut());
    }
    // SAFETY: the operation owns the unique terminal generator fault transfer.
    if let Backing::Generator { stream, .. } = &operation.backing {
        let mut fault = std::ptr::null_mut();
        // SAFETY: the exclusive operation loan pins this terminal generator and its fault slot.
        unsafe {
            let generator = (**stream)
                .inner
                .generator()
                .expect("generator backing")
                .generator;
            crate::generator_checked::hew_checked_generator_take_fault(generator, &raw mut fault);
        }
        return fault;
    }
    // SAFETY: the caller lends the live operation.
    let message = match &operation.backing {
        Backing::Pipe(core) => core.fault_message(),
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => {
            // SAFETY: the operation owns this live async request.
            unsafe { async_io::failure_message(*io) }
                .unwrap_or_else(|| "stream I/O failed".to_string())
        }
        Backing::Shared(_) | Backing::Immediate { .. } | Backing::Generator { .. } => {
            unreachable!()
        }
        Backing::Finished => "stream operation on a finished stream".to_string(),
    };
    Box::into_raw(Box::new(crate::fault::HewFault::with_message(
        crate::internal::types::HEW_TRAP_USER_PANIC,
        message,
    )))
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
    let owner = unsafe { HewNativeStream::borrow(operation) };
    // SAFETY: only the creator accesses state; notifications share the header.
    let operation = unsafe { &mut **owner.state.get() };
    let item = operation.envelope.take();
    // SAFETY: out receives the descriptor's sole owned value.
    unsafe { decode_elem_envelope(item, out, &operation.layout, "native stream read") }
}

/// Poll a write: 0 pending, 1 transferred, 2 closed, 3 failure, 4 timed out
/// (a socket write deadline passed; [`hew_stream_write_committed_native`]
/// reports how much of the item reached the OS).
///
/// # Safety
/// Operation is a live exclusive write operation.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_write_poll_native(operation: *mut HewNativeStream) -> i32 {
    // SAFETY: the caller lends the live exclusive operation.
    let owner = unsafe { HewNativeStream::borrow(operation) };
    // SAFETY: only the creator accesses state; notifications share the header.
    let operation = unsafe { &mut **owner.state.get() };
    match &mut operation.backing {
        Backing::Immediate { status, .. } => *status,
        Backing::Shared(write) => {
            // SAFETY: this operation drives the unique pending frame under shared sender admission.
            unsafe { write.poll(&mut operation.envelope, &operation.layout, &owner) }
        }
        Backing::Pipe(core) => {
            let Some(envelope) = operation.envelope.take() else {
                return 1;
            };
            let (status, envelope) = core.poll_send_observed(&owner, envelope);
            operation.envelope = envelope;
            status
        }
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => {
            // SAFETY: this operation owns the live async request.
            match unsafe { async_io::hew_async_io_status(*io) } {
                0 => 0,
                1 => 1,
                // SAFETY: the status answered Error for this live request.
                _ => match unsafe { async_io::write_failure(*io) } {
                    async_io::WriteFailure::Closed => 2,
                    async_io::WriteFailure::TimedOut(_) => 4,
                    async_io::WriteFailure::Other => 3,
                },
            }
        }
        Backing::Generator { .. } => std::process::abort(),
        Backing::Finished => 2,
    }
}

/// Bytes of the item a timed-out socket write committed to the OS before its
/// deadline; `0` for any other operation. The peer's view of them is unknown.
///
/// # Safety
/// Operation is a live write operation whose last poll answered 4.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_write_committed_native(operation: *mut HewNativeStream) -> i64 {
    // SAFETY: the creator lends exclusive state; notifications use only the header.
    let operation = unsafe { &mut **(*operation).state.get() };
    // SAFETY: the caller lends the live operation.
    match &operation.backing {
        #[cfg(not(target_arch = "wasm32"))]
        // SAFETY: the operation owns this live async request.
        Backing::Io(io) => match unsafe { async_io::write_failure(*io) } {
            async_io::WriteFailure::TimedOut(committed) => committed
                .saturating_sub(i64::try_from(operation.write_prefix).unwrap_or(i64::MAX))
                .max(0),
            _ => 0,
        },
        // SAFETY: this retained write reported the timeout being observed.
        Backing::Shared(write) => unsafe { write.committed() },
        _ => 0,
    }
}

/// # Safety
/// Operation is live; cancellation precedes draining a pending operation.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_cancel_native(operation: *mut HewNativeStream) {
    // SAFETY: the creator lends exclusive state; notifications use only the header.
    let operation = unsafe { &mut **(*operation).state.get() };
    // SAFETY: the caller lends the live exclusive operation.
    match &mut operation.backing {
        // SAFETY: this operation owns and exclusively cancels its child write.
        Backing::Shared(write) => unsafe { write.cancel() },
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => {
            // SAFETY: this operation owns the async request.
            unsafe { async_io::hew_async_io_cancel(*io) };
        }
        // A pipe observation expires when its operation owner is released.
        Backing::Generator {
            stream,
            parent,
            closing,
        } => {
            *closing = true;
            // SAFETY: the live operation owns both exclusive loans through drain.
            unsafe {
                crate::generator_checked::hew_checked_generator_poll(
                    (**stream)
                        .inner
                        .generator()
                        .expect("generator backing")
                        .generator,
                    *parent,
                    true,
                );
            }
        }
        Backing::Pipe(_) | Backing::Immediate { .. } | Backing::Finished => {}
    }
}

/// # Safety
/// Operation is live and waker obeys the borrowed descriptor contract.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_cleanup_status_native(
    operation: *mut HewNativeStream,
    waker: *const HewWaker,
) -> i32 {
    // SAFETY: the creator lends exclusive state; notifications use only the header.
    let operation = unsafe { &mut **(*operation).state.get() };
    // SAFETY: the caller lends the live exclusive operation.
    match &mut operation.backing {
        // SAFETY: the owner retains the source loan and live waker through quiescence.
        Backing::Shared(write) => unsafe { write.cleanup(waker) },
        #[cfg(not(target_arch = "wasm32"))]
        Backing::Io(io) => {
            // SAFETY: this operation owns the async request.
            unsafe { async_io::hew_async_io_cleanup_status(*io, waker) }
        }
        // Nothing borrowed a handle beyond this call, so the operation is
        // already quiescent.
        Backing::Generator {
            stream,
            parent,
            closing,
        } => {
            if !*closing {
                return 1;
            }
            // SAFETY: producer cancellation uses the existing checked close driver.
            unsafe {
                let generator = (**stream)
                    .inner
                    .generator()
                    .expect("generator backing")
                    .generator;
                let status =
                    crate::generator_checked::hew_checked_generator_poll(generator, *parent, true);
                i32::from(status != crate::coro_state::CoroStatus::Pending as i32)
            }
        }
        Backing::Pipe(_) | Backing::Immediate { .. } | Backing::Finished => {
            let _ = waker;
            1
        }
    }
}

/// # Safety
/// Operation is uniquely owned and its producer cleanup status is ready.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_operation_free_native(operation: *mut HewNativeStream) {
    // SAFETY: caller transfers the drained operation's unique creator reference.
    // SAFETY: consume the one creator reference, leaving notification upgrades
    // with access to the immutable header only.
    let owner = unsafe { Arc::from_raw(operation) };
    owner.armed.store(false, Ordering::Release);
    // SAFETY: producer quiescence and the exclusive creator contract end all
    // state access. Destroy semantic owners here, never on a notifier thread.
    unsafe { ManuallyDrop::drop(&mut *owner.state.get()) };
}

/// Consume a quiescent operation, closing an untransferred element before its
/// operation and producer references are released.
/// # Safety
/// `operation` is uniquely owned and its producer cleanup status is ready.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_operation_release_begin(
    operation: *mut HewNativeStream,
) -> *mut crate::release_walker::HewReleaseCursor {
    use crate::release_walker::{HewReleaseCursor, ReleaseItem};
    unsafe fn free_operation(owner: *mut c_void) {
        // SAFETY: the cursor has consumed the detached envelope and owns the creator reference.
        unsafe { hew_stream_operation_free_native(owner.cast()) };
    }
    // SAFETY: the quiescent creator exclusively owns mutable state.
    let state = unsafe { &mut **(*operation).state.get() };
    // SAFETY: quiescence excludes producers borrowing the operation's storage.
    let value = state.envelope.take();
    // SAFETY: the operation retains its exact descriptor through this transfer.
    let layout = state.layout;
    // SAFETY: the operation is exclusively owned after producer quiescence.
    let shared = match &mut state.backing {
        // SAFETY: the quiescent child and input transfer once to the returned cursor.
        Backing::Shared(write) => unsafe { write.release_cursor(layout) },
        Backing::Immediate { release, fault, .. } => {
            // SAFETY: an unobserved terminal fault remains owned by this operation.
            unsafe { crate::fault::hew_fault_drop(std::mem::replace(fault, std::ptr::null_mut())) };
            std::mem::replace(release, std::ptr::null_mut())
        }
        _ => std::ptr::null_mut(),
    };
    let storage = ReleaseItem::Storage {
        owner: operation.cast(),
        free: free_operation,
    };
    let Some(value) = value else {
        if shared.is_null() {
            // SAFETY: no envelope or child cleanup retains the quiescent operation.
            unsafe { free_operation(operation.cast()) };
        } else {
            // SAFETY: child cleanup owns the operation's final storage release.
            unsafe { HewReleaseCursor::after(shared, storage) };
        }
        return shared;
    };
    let envelope = HewReleaseCursor::envelopes(vec![value], Some(layout), storage);
    // SAFETY: the owner storage remains pinned until its child/input cursors finish.
    unsafe { HewReleaseCursor::join([shared, envelope]) }
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
    use std::sync::atomic::AtomicUsize;
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

            release_start: None,
        }
    }

    #[test]
    fn owner_release_destroys_state_before_a_racing_notification_finishes() {
        use std::sync::{Barrier, Mutex};

        struct Notification {
            entered: Barrier,
            finish: Barrier,
            calls: AtomicUsize,
        }
        unsafe extern "C" fn retain_notification(context: *mut c_void) {
            // SAFETY: the descriptor borrows one live Arc allocation.
            unsafe { Arc::increment_strong_count(context.cast::<Notification>()) };
        }
        unsafe extern "C" fn release_notification(context: *mut c_void) {
            // SAFETY: each retained descriptor owns one Arc reference.
            unsafe { Arc::decrement_strong_count(context.cast::<Notification>()) };
        }
        unsafe extern "C" fn notify_blocked(context: *mut c_void) {
            // SAFETY: the retained descriptor pins its immutable context.
            let target = unsafe { &*context.cast::<Notification>() };
            if target.calls.fetch_add(1, Ordering::SeqCst) == 0 {
                target.entered.wait();
                target.finish.wait();
            }
        }
        unsafe extern "C-unwind" fn drop_on_owner(slot: *mut c_void) {
            // SAFETY: the envelope uniquely owns this boxed thread recorder.
            let record = unsafe {
                Box::from_raw(*slot.cast::<*mut Arc<Mutex<Vec<std::thread::ThreadId>>>>())
            };
            record.lock().unwrap().push(std::thread::current().id());
        }
        let notification = Arc::new(Notification {
            entered: Barrier::new(2),
            finish: Barrier::new(2),
            calls: AtomicUsize::new(0),
        });
        let wake = HewWaker {
            context: Arc::as_ptr(&notification).cast_mut().cast(),
            wake: notify_blocked,
            retain: retain_notification,
            release: release_notification,
        };
        let drops = Arc::new(Mutex::new(Vec::<std::thread::ThreadId>::new()));
        let pointer = Box::into_raw(Box::new(Arc::clone(&drops)));
        let layout = HewValueLayout {
            drop_fn: Some(drop_on_owner),
            ..owned_layout()
        };
        // SAFETY: the operation takes the boxed owner once; the notifier only
        // borrows its immutable header while the creator destroys state.
        unsafe {
            let operation = HewNativeStream::new(
                &raw const wake,
                StreamState {
                    layout,
                    backing: Backing::Finished,
                    #[cfg(not(target_arch = "wasm32"))]
                    yields: ReadYield::Content,
                    envelope: Some((pointer as usize).to_ne_bytes().to_vec()),
                    read_stream: None,
                    #[cfg(not(target_arch = "wasm32"))]
                    write_prefix: 0,
                },
            );
            let weak = {
                let owner = HewNativeStream::borrow(operation);
                Arc::downgrade(&owner)
            };
            let racing = weak.clone();
            let notifier = std::thread::spawn(move || racing.upgrade().unwrap().wake());
            notification.entered.wait();
            hew_stream_operation_free_native(operation);
            assert_eq!(*drops.lock().unwrap(), [std::thread::current().id()]);
            // A notification already holds the header, but the creator's
            // release disarms every subsequent notification through it.
            weak.upgrade().unwrap().wake();
            assert_eq!(notification.calls.load(Ordering::SeqCst), 1);
            notification.finish.wait();
            notifier.join().unwrap();
            assert!(weak.upgrade().is_none());
            assert_eq!(drops.lock().unwrap().len(), 1);
            assert_eq!(Arc::strong_count(&notification), 1);
        }
    }

    #[test]
    fn cancelled_pipe_read_does_not_disarm_its_successor() {
        let (signal, notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let layout = owned_layout();
        let drops = Arc::new(AtomicUsize::new(0));
        let mut sink = std::ptr::null_mut();
        // SAFETY: each operation independently retains the descriptor, and
        // both stream loans end before endpoint destruction.
        unsafe {
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            let cancelled = hew_stream_read_start_native(
                stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            );
            assert_eq!(hew_stream_read_poll_native(cancelled), 0);
            hew_stream_cancel_native(cancelled);
            assert_eq!(
                hew_stream_cleanup_status_native(cancelled, &raw const wake),
                1
            );
            hew_stream_operation_free_native(cancelled);
            assert_eq!(Arc::strong_count(&signal), 1);
            let successor = hew_stream_read_start_native(
                stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            );
            assert_eq!(hew_stream_read_poll_native(successor), 0);
            let mut value = Box::into_raw(Box::new(Arc::clone(&drops)));
            let write = start_underlying(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
            );
            assert_eq!(hew_stream_write_poll_native(write), 1);
            hew_stream_operation_free_native(write);
            notifications.recv_timeout(Duration::from_secs(5)).unwrap();
            assert!(notifications.try_recv().is_err());
            assert_eq!(hew_stream_read_poll_native(successor), 1);
            let mut received: *mut Arc<AtomicUsize> = std::ptr::null_mut();
            assert_eq!(
                hew_stream_read_take_native(successor, (&raw mut received).cast()),
                1
            );
            hew_stream_operation_free_native(successor);
            assert_eq!(received, value);
            drop_owned((&raw mut received).cast());
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            assert_eq!(Arc::strong_count(&signal), 1);
            drop(Box::from_raw(stream));
            super::super::hew_sink_close(sink);
        }
    }

    #[test]
    fn ready_pipe_receive_transfers_owner_and_pending_receive_still_wakes() {
        let (signal, notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let drops = Arc::new(AtomicUsize::new(0));
        let layout = owned_layout();
        let mut sink = std::ptr::null_mut();
        // SAFETY: each slot and endpoint stays live through its unique transfer.
        unsafe {
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            let mut received: *mut Arc<AtomicUsize> = std::ptr::null_mut();
            let mut fault = std::ptr::null_mut();
            assert_eq!(
                hew_stream_read_ready_native(
                    stream,
                    &raw const layout,
                    (&raw mut received).cast(),
                    &raw mut fault,
                ),
                0
            );
            assert!(received.is_null());
            assert!(fault.is_null());
            assert_eq!(Arc::strong_count(&signal), 1);

            let pending = hew_stream_read_start_native(
                stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            );
            assert_eq!(hew_stream_read_poll_native(pending), 0);
            let mut value = Box::into_raw(Box::new(Arc::clone(&drops)));
            let write = start_underlying(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
            );
            assert_eq!(hew_stream_write_poll_native(write), 1);
            assert!(hew_stream_operation_release_begin(write).is_null());
            notifications
                .recv_timeout(Duration::from_secs(1))
                .expect("parked read wakes");
            assert_eq!(hew_stream_read_poll_native(pending), 1);
            assert_eq!(
                hew_stream_read_take_native(pending, (&raw mut received).cast()),
                1
            );
            assert!(hew_stream_operation_release_begin(pending).is_null());
            drop_owned((&raw mut received).cast());
            assert_eq!(drops.load(Ordering::SeqCst), 1);

            let mut value = Box::into_raw(Box::new(Arc::clone(&drops)));
            let write = start_underlying(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
            );
            assert_eq!(hew_stream_write_poll_native(write), 1);
            assert!(hew_stream_operation_release_begin(write).is_null());
            super::super::hew_sink_finish(sink);
            assert_eq!(
                hew_stream_read_ready_native(
                    stream,
                    &raw const layout,
                    (&raw mut received).cast(),
                    &raw mut fault,
                ),
                1,
                "queued value precedes EOF"
            );
            assert!(fault.is_null());
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            drop_owned((&raw mut received).cast());
            assert_eq!(drops.load(Ordering::SeqCst), 2);
            assert_eq!(
                hew_stream_read_ready_native(
                    stream,
                    &raw const layout,
                    (&raw mut received).cast(),
                    &raw mut fault,
                ),
                2
            );
            assert_eq!(Arc::strong_count(&signal), 1);
            drop(Box::from_raw(stream));
            super::super::hew_sink_close(sink);
        }
    }

    #[test]
    fn ready_pipe_receive_preserves_terminal_fault_ownership() {
        let layout = owned_layout();
        let mut sink = std::ptr::null_mut();
        // SAFETY: the exclusive endpoints and output slots survive the probe.
        unsafe {
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            (*stream).channel.as_ref().unwrap().fault_close(73);
            let mut received: *mut Arc<AtomicUsize> = std::ptr::null_mut();
            let mut fault = std::ptr::null_mut();
            assert_eq!(
                hew_stream_read_ready_native(
                    stream,
                    &raw const layout,
                    (&raw mut received).cast(),
                    &raw mut fault,
                ),
                3
            );
            assert!(received.is_null());
            assert!(!fault.is_null());
            crate::fault::hew_fault_drop(fault);
            drop(Box::from_raw(stream));
            super::super::hew_sink_close(sink);
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
            let first_op = start_underlying(
                sink,
                &raw const wake,
                (&raw mut first).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
            );
            assert_eq!(hew_stream_write_poll_native(first_op), 1);
            hew_stream_operation_free_native(first_op);
            let mut second = Box::into_raw(Box::new(Arc::clone(&drops)));
            let second_op = start_underlying(
                sink,
                &raw const wake,
                (&raw mut second).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
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

    unsafe extern "C-unwind" fn drop_sink(slot: *mut c_void) {
        // SAFETY: this descriptor owns exactly one live sink handle.
        unsafe { super::super::hew_sink_close(*slot.cast::<*mut HewSink>()) };
    }

    fn sink_layout() -> HewValueLayout {
        HewValueLayout {
            size: size_of::<*mut HewSink>(),
            align: align_of::<*mut HewSink>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: Some(drop_sink),
            release_start: None,
        }
    }

    #[test]
    fn cloned_pipe_write_cancellation_preserves_owners_and_last_producer_eof() {
        let (signal, notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let drops = Arc::new(AtomicUsize::new(0));
        let layout = owned_layout();
        let sink_layout = sink_layout();
        let mut sink = std::ptr::null_mut();
        // SAFETY: handles, descriptors and slots stay live through operation
        // cleanup. Each transferred value and handle is released exactly once.
        unsafe {
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            let sibling = super::super::sender::hew_sink_clone(sink, &raw const sink_layout);
            let mut first = Box::into_raw(Box::new(Arc::clone(&drops)));
            let sent = hew_stream_write_start_native(
                sink,
                &raw const wake,
                (&raw mut first).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
                &raw const sink_layout,
            );
            assert_eq!(hew_stream_write_poll_native(sent), 1);
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(sent));

            let mut second = Box::into_raw(Box::new(Arc::clone(&drops)));
            let pending = hew_stream_write_start_native(
                sibling,
                &raw const wake,
                (&raw mut second).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
                &raw const sink_layout,
            );
            assert_eq!(hew_stream_write_poll_native(pending), 0);
            // Closing another producer must not close this producer's pipe.
            super::super::hew_sink_close(sink);
            hew_stream_cancel_native(pending);
            assert_eq!(
                hew_stream_cleanup_status_native(pending, &raw const wake),
                1
            );
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(pending));
            assert_eq!(drops.load(Ordering::SeqCst), 1);

            let read = hew_stream_read_start_native(
                stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            );
            assert_eq!(hew_stream_read_poll_native(read), 1);
            let mut received: *mut Arc<AtomicUsize> = std::ptr::null_mut();
            assert_eq!(
                hew_stream_read_take_native(read, (&raw mut received).cast()),
                1
            );
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(read));
            drop_owned((&raw mut received).cast());
            assert_eq!(drops.load(Ordering::SeqCst), 2);
            assert!(
                notifications.try_recv().is_err(),
                "cancelled send must not wake"
            );

            let eof = hew_stream_read_start_native(
                stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            );
            assert_eq!(
                hew_stream_read_poll_native(eof),
                0,
                "last producer is still open"
            );
            super::super::hew_sink_close(sibling);
            notifications
                .recv_timeout(Duration::from_secs(1))
                .expect("last producer wakes reader");
            assert_eq!(hew_stream_read_poll_native(eof), 2);
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(eof));
            drop(Box::from_raw(stream));
        }
        assert_eq!(drops.load(Ordering::SeqCst), 2);
        assert_eq!(Arc::strong_count(&signal), 1);
    }

    #[test]
    fn pipe_finish_before_first_write_poll_rejects_owned_value_after_eof() {
        let (signal, _notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let drops = Arc::new(AtomicUsize::new(0));
        let layout = owned_layout();
        let sink_layout = sink_layout();
        let mut sink = std::ptr::null_mut();
        // SAFETY: finish keeps the handle alive; both operation references
        // drain before their handles and exact descriptors are released.
        unsafe {
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            let mut value = Box::into_raw(Box::new(Arc::clone(&drops)));
            let write = hew_stream_write_start_native(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
                &raw const sink_layout,
            );
            super::super::hew_sink_finish(sink);
            assert_eq!(hew_stream_write_poll_native(write), 2);
            assert_eq!(hew_stream_cleanup_status_native(write, &raw const wake), 1);
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(write));
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            let read = hew_stream_read_start_native(
                stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            );
            assert_eq!(hew_stream_read_poll_native(read), 2, "no post-EOF value");
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(read));
            super::super::hew_sink_close(sink);
            drop(Box::from_raw(stream));
        }
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        assert_eq!(Arc::strong_count(&signal), 1);
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
            let operation = hew_stream_read_start_native(
                stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            );
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
        struct OperationOwner(*mut HewNativeStream);
        impl Drop for OperationOwner {
            fn drop(&mut self) {
                // SAFETY: this fixture owns the sole creator reference; its
                // reactor guard keeps the source loan live through teardown.
                unsafe { hew_stream_operation_free_native(self.0) };
            }
        }
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
            let abandoned = OperationOwner(hew_stream_read_start_native(
                &raw mut *stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            ));
            assert_eq!(hew_stream_read_poll_native(abandoned.0), 0);
            hew_stream_cancel_native(abandoned.0);
            while hew_stream_cleanup_status_native(abandoned.0, &raw const wake) == 0 {
                notifications.recv_timeout(Duration::from_secs(5)).unwrap();
            }
            drop(abandoned);

            let next = OperationOwner(hew_stream_read_start_native(
                &raw mut *stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            ));
            peer.write_all(&[0]).unwrap();
            loop {
                match hew_stream_read_poll_native(next.0) {
                    0 => {
                        notifications.recv_timeout(Duration::from_secs(5)).unwrap();
                    }
                    1 => break,
                    status => panic!("live socket was invalidated by cancellation: {status}"),
                }
            }
            while hew_stream_cleanup_status_native(next.0, &raw const wake) == 0 {
                notifications.recv_timeout(Duration::from_secs(5)).unwrap();
            }
            let mut output: *mut hew_cabi::string::HewString = std::ptr::null_mut();
            assert_eq!(
                hew_stream_read_take_native(next.0, (&raw mut output).cast()),
                1
            );
            let bytes = hew_cabi::string::string_as_bytes(output).to_vec();
            hew_cabi::string::string_release(output);
            assert_eq!(bytes, &[0]);
        }
    }
}
