//! Owned native stream operations. Queue observations are withdrawn on drop;
//! content I/O keeps its source loan until the blocking producer is quiescent.

use std::cell::UnsafeCell;
use std::ffi::c_void;
use std::mem::ManuallyDrop;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use hew_cabi::vec::HewValueLayout;

use super::{HewSink, HewStream};
use crate::channel_common::{
    decode_elem_envelope, drop_elem_envelope, move_elem_envelope, move_elem_layout_witness,
};
use crate::channel_core::ChannelCore;
use crate::platform;
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
    TypedWrite(Box<crate::channel_core::typed::TypedWrite>),
    TypedRead(Box<crate::channel_core::typed::TypedRead>),
    Io(platform::host_io::StreamAsyncIo),
    Shared(Box<super::sender::SharedWrite>),
    Immediate(platform::host_io::StreamImmediate),
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
    envelope: Option<Vec<u8>>,
    read_stream: Option<*mut HewStream>,
    io: platform::host_io::StreamIoFields,
    fault: *mut crate::fault::HewFault,
    _code: crate::code_owner::CodeRoot,
}

/// Visit the owning payload of a frozen native stream operation.
/// # Safety
/// The slot contains a retained operation; its containing invocation excludes poll and teardown.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_stream_operation(
    slot: *const c_void,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    use hew_cabi::managed::UNDESCRIBED;
    cabi_guard!(slot.is_null(), 0);
    cabi_guard!(visitor.is_null(), UNDESCRIBED);
    // SAFETY: the compiler supplies the operation's live handle slot.
    unsafe {
        let pointer = slot.cast::<*mut HewNativeStream>().read();
        let Some(header) = pointer.as_ref() else {
            return 0;
        };
        // SAFETY: the invocation excludes all mutable state access; notifications
        // share only the header and cannot access the state.
        let operation = &mut **header.state.get();
        // Queue/read envelopes require their exact typed ownership receipt.
        if operation.envelope.is_some() {
            return UNDESCRIBED;
        }
        let status = operation.io.visit(visitor);
        if status != 0 {
            return status;
        }
        match &mut operation.backing {
            Backing::TypedWrite(write) => write.visit_protocol(visitor),
            Backing::TypedRead(read) => read.visit(visitor),
            Backing::Shared(write) => write.visit_managed(&operation.layout, visitor),
            Backing::Immediate(immediate) => immediate.visit_release(visitor),
            Backing::Io(io) => {
                if operation.io.has_write_content() {
                    0
                } else {
                    io.visit_stream_content(visitor)
                }
            }
            Backing::Pipe(_) => UNDESCRIBED,
            Backing::Generator { .. } | Backing::Finished => 0,
        }
    }
}

impl Drop for StreamState {
    fn drop(&mut self) {
        // SAFETY: an unobserved materialization fault remains uniquely owned here.
        unsafe { crate::fault::hew_fault_drop(self.fault) };
        if let Some(envelope) = self.envelope.take() {
            drop_elem_envelope(Some(&self.layout), envelope, "native stream drop");
        }
        if let Backing::Io(io) = self.backing {
            // SAFETY: this operation owns the creator reference; generated
            // cleanup waits for producer quiescence before releasing borrowed
            // handles.
            unsafe { io.free() };
        }
    }
}

/// Begin one receive without blocking a scheduler worker.
///
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
    if !fault_out.is_null() {
        // SAFETY: caller supplies writable fault_out when non-null.
        unsafe { *fault_out = std::ptr::null_mut() };
    }
    cabi_guard!(
        stream.is_null() || layout.is_null() || out.is_null() || fault_out.is_null(),
        2
    );
    // SAFETY: the caller supplies the same exact witness as a parked receive.
    let layout = unsafe { move_elem_layout_witness(layout, "ready native stream read") };
    // SAFETY: the exclusive loan and output slots survive this call.
    unsafe {
        if (*stream).exhausted() {
            return 2;
        }
        let Some(core) = (*stream).channel.as_ref() else {
            return 0;
        };
        // Typed reads must retain their budget reservation and refund receipt.
        if core.is_typed() {
            return 0;
        }
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
    cabi_guard!(
        stream.is_null() || waker.is_null() || layout.is_null(),
        std::ptr::null_mut()
    );
    // SAFETY: compiler-provided live operands satisfy the start contract.
    let layout = unsafe { *move_elem_layout_witness(layout, "native stream read") };
    // SAFETY: stream is a live exclusive loan.
    let exhausted = unsafe { (*stream).exhausted() };
    let (backing, io) = if exhausted {
        (Backing::Finished, platform::host_io::StreamIoFields::EMPTY)
    } else {
        // SAFETY: stream is a live exclusive loan.
        let is_generator = unsafe { (*stream).inner.generator().is_some() };
        if is_generator {
            (
                Backing::Generator {
                    stream,
                    parent,
                    closing: false,
                },
                platform::host_io::StreamIoFields::EMPTY,
            )
        } else {
            // SAFETY: the exclusive source loan is live until this request drains.
            match unsafe { (*stream).channel.clone() } {
                Some(core) if core.is_typed() => (
                    Backing::TypedRead(crate::channel_core::typed::TypedRead::new(
                        core,
                        // SAFETY: the caller lends a live descriptor during construction.
                        Arc::new(unsafe { OwnedWaker::retain(&*waker) }),
                    )),
                    platform::host_io::StreamIoFields::EMPTY,
                ),
                Some(core) => (
                    Backing::Pipe(core),
                    platform::host_io::StreamIoFields::EMPTY,
                ),
                None => {
                    // SAFETY: caller's exclusive loan and live waker satisfy the reactor read start contract.
                    match unsafe {
                        platform::host_io::try_start_stream_reactor_read(
                            stream, layout, parent, waker,
                        )
                    } {
                        Some((io, fields)) => (Backing::Io(io), fields),
                        None => platform::host_io::missing_stream_pipe_backing("read"),
                    }
                }
            }
        }
    };
    {
        let state = StreamState {
            // SAFETY: the caller's descriptor owner remains live during construction.
            _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
            layout,

            backing,
            envelope: None,
            read_stream: Some(stream),
            io,
            fault: std::ptr::null_mut(),
        };
        // SAFETY: the caller lends a live descriptor during header construction.
        unsafe { HewNativeStream::new(waker, state) }
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
    out_status: *mut i32,
) -> *mut HewNativeStream {
    cabi_guard!(out_status.is_null(), std::ptr::null_mut());
    // SAFETY: out_status is writable; clear it before validating other pointers.
    unsafe { out_status.write(0) };
    if sink.is_null() || waker.is_null() || layout.is_null() || sink_layout.is_null() {
        // SAFETY: out_status verified non-null above.
        unsafe { out_status.write(hew_cabi::managed::UNDESCRIBED) };
        return std::ptr::null_mut();
    }
    // SAFETY: `layout` was verified non-null above.
    let needs_data = unsafe { crate::vec::layout_needs_slot(&*layout) };
    if data.is_null() && needs_data {
        // SAFETY: out_status verified non-null above.
        unsafe { out_status.write(hew_cabi::managed::UNDESCRIBED) };
        return std::ptr::null_mut();
    }
    // SAFETY: the source read loan pins the endpoint and its exact descriptor.
    unsafe { super::sender::retain(sink, sink_layout, false) };
    // SAFETY: the installed owner serializes every underlying write through terminal cleanup.
    unsafe {
        match start_underlying(sink, waker, data, layout, parent, park) {
            Ok(operation) => operation,
            Err(status) => {
                out_status.write(status);
                std::ptr::null_mut()
            }
        }
    }
}

/// # Safety
/// The caller holds the underlying sender gate, or borrows an installed shared endpoint.
#[allow(
    clippy::too_many_lines,
    reason = "one constructor selects the existing typed, content and legacy operation backings"
)]
pub(super) unsafe fn start_underlying(
    sink: *mut HewSink,
    waker: *const HewWaker,
    data: *mut c_void,
    layout: *const HewValueLayout,
    parent: *const crate::coro_state::HewCoroState,
    park: bool,
) -> Result<*mut HewNativeStream, i32> {
    // SAFETY: the compiler provides a static descriptor and a live owned value.
    let layout = unsafe { *move_elem_layout_witness(layout, "native stream write") };
    // SAFETY: this operation pins the endpoint while inspecting its shared backing.
    let owner = unsafe { super::sender::shared(sink) };
    // SAFETY: an adapter endpoint pins its shared sender through this exclusive loan.
    if let Some(owner) = owner {
        return Ok({
            let state = StreamState {
                // SAFETY: the caller retains this descriptor until the operation owns it.
                _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
                layout,
                // SAFETY: the caller retains this descriptor during operation construction.

                // SAFETY: the shared endpoint, parent and typed input remain live for construction.
                backing: Backing::Shared(Box::new(unsafe {
                    super::sender::SharedWrite::new(owner, sink, parent, park, data, &layout)?
                })),
                envelope: None,
                read_stream: None,
                io: platform::host_io::StreamIoFields::EMPTY,
                fault: std::ptr::null_mut(),
            };
            // SAFETY: the caller lends a live descriptor during header construction.
            unsafe { HewNativeStream::new(waker, state) }
        });
    }
    // SAFETY: the shared admission gate pins the underlying queue reference.
    let raw = unsafe { (*sink).channel_core_ptr().cast::<ChannelCore>() };
    let core = if raw.is_null() {
        None
    } else {
        // SAFETY: the sink retains this channel Arc while the operation acquires its own reference.
        unsafe { Arc::increment_strong_count(raw) };
        // SAFETY: the preceding increment transferred exactly one channel reference to this operation.
        Some(unsafe { Arc::from_raw(raw) })
    };
    if let Some(core) = core.as_ref().filter(|core| core.is_typed()) {
        // SAFETY: the caller retains the borrowed waker descriptor during construction.
        let receipt_waker = Arc::new(unsafe { OwnedWaker::retain(&*waker) });
        return Ok({
            let state = StreamState {
                // SAFETY: the checked descriptor pins its defining code during root acquisition.
                _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
                layout,
                // SAFETY: the enclosing operation retains the typed input and parent until admission or cancellation.
                backing: Backing::TypedWrite(unsafe {
                    crate::channel_core::typed::TypedWrite::new(
                        Arc::clone(core),
                        data,
                        layout,
                        parent,
                        Arc::clone(&receipt_waker),
                    )
                }),
                envelope: None,
                read_stream: None,
                io: platform::host_io::StreamIoFields::EMPTY,
                fault: std::ptr::null_mut(),
            };
            // SAFETY: the caller lends a live descriptor during header construction.
            unsafe { HewNativeStream::new(waker, state) }
        });
    }
    if !park {
        // SAFETY: legacy content backings perform one nonparking capacity attempt.
        let (immediate, io) =
            unsafe { platform::host_io::try_nonblocking_stream_write(sink, data, layout, parent) }?;
        return Ok({
            let state = StreamState {
                // SAFETY: the caller's descriptor owner remains live during construction.
                _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
                layout,
                backing: Backing::Immediate(immediate),
                envelope: None,
                read_stream: None,
                io,
                fault: std::ptr::null_mut(),
            };
            // SAFETY: the caller lends a live descriptor during header construction.
            unsafe { HewNativeStream::new(waker, state) }
        });
    }
    if let Some(started) =
        // SAFETY: caller holds exclusive sink loan and live operands through reactor start.
        unsafe {
            platform::host_io::try_start_sink_reactor_write(sink, waker, data, layout, parent)
        }
    {
        let (reactor_io, io) = started?;
        return Ok({
            let state = StreamState {
                // SAFETY: the checked descriptor pins its defining code during root acquisition.
                _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
                layout,
                backing: Backing::Io(reactor_io),
                envelope: None,
                read_stream: None,
                io,
                fault: std::ptr::null_mut(),
            };
            // SAFETY: the caller lends a live descriptor during header construction.
            unsafe { HewNativeStream::new(waker, state) }
        });
    }
    // SAFETY: the source value transfers exactly once at operation construction.
    let envelope = unsafe { move_elem_envelope(data, &layout, "native stream write") };
    // A finished sink has released its backing, so there is nothing left to
    // inspect for a queue or a transport. Report the closed status the write
    // poll already carries; `Drop` releases the element this operation took.
    // SAFETY: sink is a live exclusive loan.
    if unsafe { (*sink).is_closed() } {
        return Ok({
            let state = StreamState {
                // SAFETY: the caller retains this descriptor until the operation owns it.
                _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
                layout,
                backing: Backing::Finished,
                envelope: Some(envelope),
                read_stream: None,
                io: platform::host_io::StreamIoFields::EMPTY,
                fault: std::ptr::null_mut(),
            };
            // SAFETY: the caller lends a live descriptor during header construction.
            unsafe { HewNativeStream::new(waker, state) }
        });
    }
    let (backing, envelope) = match core {
        Some(core) => {
            core.stamp_elem_layout(&layout);
            (Backing::Pipe(core), Some(envelope))
        }
        None => platform::host_io::missing_stream_pipe_backing("write"),
    };
    Ok({
        let state = StreamState {
            // SAFETY: the caller's descriptor owner remains live during construction.
            _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
            layout,
            backing,
            envelope,
            read_stream: None,
            io: platform::host_io::StreamIoFields::EMPTY,
            fault: std::ptr::null_mut(),
        };
        // SAFETY: the caller lends a live descriptor during header construction.
        unsafe { HewNativeStream::new(waker, state) }
    })
}

/// Poll a receive: 0 pending, 1 value, 2 EOF, 3 failure.
///
/// # Safety
/// Operation is a live exclusive read operation; out is one writable element.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_read_poll_native(operation: *mut HewNativeStream) -> i32 {
    cabi_guard!(operation.is_null(), 2);
    // SAFETY: the creator reference remains live throughout this exclusive poll.
    let owner = unsafe { HewNativeStream::borrow(operation) };
    // SAFETY: the caller lends the live exclusive operation.
    let operation = unsafe { &mut **(*operation).state.get() };
    let (status, item) = match &mut operation.backing {
        Backing::TypedRead(read) => (read.poll(), None),
        Backing::Pipe(core) => core.poll_recv_observed(&owner),
        Backing::Io(io) => {
            // SAFETY: the operation owns this live async request.
            unsafe { operation.io.poll_io(*io, operation.layout.size) }
        }
        Backing::Shared(_) | Backing::Immediate(_) | Backing::TypedWrite(_) => {
            std::process::abort()
        }
        Backing::Generator { stream, parent, .. } => {
            // SAFETY: the operation retains the exclusive stream and parent loans.
            let status = unsafe {
                (**stream)
                    .inner
                    .generator()
                    .expect("generator backing")
                    .poll_native(*parent)
            };
            (status, None)
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
    cabi_guard!(operation.is_null(), std::ptr::null_mut());
    // SAFETY: the creator exclusively lends state; notifications access only the header.
    let state = unsafe { &mut **(*operation).state.get() };
    // SAFETY: the operation owns its unpublished-result diagnostic.
    if !state.fault.is_null() {
        // SAFETY: the caller retains this live operation throughout the exclusive access.
        return std::mem::replace(&mut state.fault, std::ptr::null_mut());
    }
    // SAFETY: the enclosing stream operation retains this async request.
    if let Backing::Io(io) = &state.backing {
        // SAFETY: the enclosing stream operation retains this async request.
        if let Some(code) = unsafe { io.failure_trap() } {
            return crate::fault::hew_fault_new(code);
        }
    }
    // SAFETY: the caller holds this operation's exclusive poll loan.
    if let Backing::Shared(write) = &mut state.backing {
        // SAFETY: this owned write reported a terminal failure.
        return unsafe { write.take_fault() };
    }
    // SAFETY: the operation exclusively owns this immediate terminal fault.
    if let Backing::Immediate(immediate) = &mut state.backing {
        return immediate.take_fault();
    }
    // SAFETY: the operation owns the unique terminal generator fault transfer.
    if let Backing::Generator { stream, .. } = &state.backing {
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
    let message = match &state.backing {
        Backing::TypedRead(read) => read
            .fault_message()
            .unwrap_or_else(|| "typed stream failed".to_owned()),
        Backing::Pipe(core) => core.fault_message(),
        Backing::Io(io) => {
            // SAFETY: the operation owns this live async request.
            unsafe { io.failure_message() }.unwrap_or_else(|| "stream I/O failed".to_string())
        }
        Backing::Shared(_)
        | Backing::Immediate(_)
        | Backing::Generator { .. }
        | Backing::TypedWrite(_) => {
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
    cabi_guard!(operation.is_null() || out.is_null(), 2);
    // SAFETY: the caller lends the live exclusive operation.
    let operation = unsafe { &mut **(*operation).state.get() };
    if let Backing::TypedRead(read) = &mut operation.backing {
        // SAFETY: out is writable storage of the operation's exact element descriptor.
        return i32::from(unsafe { read.take(out) });
    }
    if let Backing::Generator { stream, .. } = &mut operation.backing {
        // SAFETY: the operation retains the unique Stream loan and out has its exact yield layout.
        return i32::from(unsafe {
            (**stream)
                .inner
                .generator()
                .expect("generator backing")
                .take_native(out)
        });
    }
    // SAFETY: the temporary content image is decoded into the caller's unpublished slot.
    if let Some(status) = unsafe {
        operation
            .io
            .take_read_content(&operation.layout, out, &mut operation.fault)
    } {
        return status;
    }
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
    cabi_guard!(operation.is_null(), 2);
    // SAFETY: the creator reference remains live throughout this exclusive poll.
    let owner = unsafe { HewNativeStream::borrow(operation) };
    // SAFETY: the caller lends the live exclusive operation.
    let operation = unsafe { &mut **(*operation).state.get() };
    match &mut operation.backing {
        Backing::TypedWrite(write) => write.poll(),
        Backing::Immediate(immediate) => immediate.status(),
        Backing::Shared(write) => {
            // SAFETY: this operation drives the unique pending frame under shared sender admission.
            unsafe { write.poll(&operation.layout, &owner) }
        }
        Backing::Pipe(core) => {
            let Some(envelope) = operation.envelope.take() else {
                return 1;
            };
            let (status, envelope) = core.poll_send_observed(&owner, envelope);
            operation.envelope = envelope;
            status
        }
        Backing::Io(io) => {
            // SAFETY: this operation owns the live async request.
            unsafe { io.poll_write() }
        }
        Backing::Generator { .. } | Backing::TypedRead(_) => std::process::abort(),
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
    cabi_guard!(operation.is_null(), 0);
    // SAFETY: the creator exclusively lends state; notifications access only the header.
    let state = unsafe { &**(*operation).state.get() };
    // SAFETY: the caller lends the live operation.
    match &state.backing {
        // SAFETY: the operation owns this live async request.
        Backing::Io(io) => unsafe { io.timed_out_committed(state.io.write_prefix()) },
        // SAFETY: this retained write reported the timeout being observed.
        Backing::Shared(write) => unsafe { write.committed() },
        _ => 0,
    }
}

/// # Safety
/// Operation is live; cancellation precedes draining a pending operation.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_cancel_native(operation: *mut HewNativeStream) {
    cabi_guard!(operation.is_null());
    // SAFETY: the creator exclusively lends state; notifications access only the header.
    let state = unsafe { &mut **(*operation).state.get() };
    // SAFETY: the caller lends the live exclusive operation.
    match &mut state.backing {
        Backing::TypedWrite(write) => write.cancel(),
        Backing::TypedRead(read) => read.cancel(),
        // SAFETY: this operation owns and exclusively cancels its child write.
        Backing::Shared(write) => unsafe { write.cancel() },
        Backing::Io(io) => {
            // SAFETY: this operation owns the async request.
            unsafe { io.cancel() };
        }
        // A pipe registration is withdrawn by the read slot the await entry
        // parked on, not here.
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
        Backing::Pipe(_) | Backing::Immediate(_) | Backing::Finished => {}
    }
}

/// # Safety
/// Operation is live and waker obeys the borrowed descriptor contract.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_cleanup_status_native(
    operation: *mut HewNativeStream,
    waker: *const HewWaker,
) -> i32 {
    cabi_guard!(operation.is_null(), 1);
    // SAFETY: the creator exclusively lends state; notifications access only the header.
    let state = unsafe { &mut **(*operation).state.get() };
    // SAFETY: the caller lends the live exclusive operation.
    match &mut state.backing {
        // SAFETY: the owner retains the source loan and live waker through quiescence.
        Backing::Shared(write) => unsafe { write.cleanup(waker) },
        Backing::Io(io) => {
            // SAFETY: this operation owns the async request.
            unsafe { io.cleanup_status(waker) }
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
        Backing::Pipe(_)
        | Backing::TypedRead(_)
        | Backing::TypedWrite(_)
        | Backing::Immediate(_)
        | Backing::Finished => {
            let _ = waker;
            1
        }
    }
}

/// # Safety
/// Operation is uniquely owned and its producer cleanup status is ready.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_operation_free_native(operation: *mut HewNativeStream) {
    cabi_guard!(operation.is_null());
    // SAFETY: caller transfers the drained operation's unique creator reference.
    // SAFETY: consume the one creator reference, leaving notification upgrades
    // with access to the immutable header only.
    let owner = unsafe { Arc::from_raw(operation) };
    owner.armed.store(false, Ordering::Release);
    // SAFETY: producer quiescence and the exclusive creator contract end all
    // state access. Destroy semantic owners here, never on a notifier thread.
    unsafe { ManuallyDrop::drop(&mut *owner.state.get()) };
}

/// A typed Pipe receipt borrows the outer sender's slot until queue admission succeeds.
pub(super) unsafe fn borrows_write_input(operation: *const HewNativeStream) -> bool {
    // SAFETY: the creator exclusively lends state; notifications access only the header.
    let state = unsafe { &**(*operation).state.get() };
    // SAFETY: the caller retains this live operation throughout the exclusive access.
    matches!(state.backing, Backing::TypedWrite(_))
}

unsafe fn free_operation(owner: *mut c_void) {
    // SAFETY: the cursor has consumed the detached envelope and owns the creator reference.
    unsafe { hew_stream_operation_free_native(owner.cast()) };
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
    cabi_guard!(operation.is_null(), std::ptr::null_mut());
    // SAFETY: the quiescent creator exclusively owns mutable state.
    let state = unsafe { &mut **(*operation).state.get() };
    // SAFETY: quiescence excludes producers borrowing the operation's storage.
    let value = state.envelope.take();
    // SAFETY: the operation retains its exact descriptor through this transfer.
    let layout = state.layout;
    // SAFETY: the operation is exclusively owned after producer quiescence.
    let typed_read = if matches!(state.backing, Backing::TypedRead(_)) {
        let Backing::TypedRead(read) = std::mem::replace(&mut state.backing, Backing::Finished)
        else {
            unreachable!()
        };
        read.release_begin()
    } else {
        std::ptr::null_mut()
    };
    let shared = match &mut state.backing {
        // SAFETY: the quiescent child and input transfer once to the returned cursor.
        Backing::Shared(write) => unsafe { write.release_cursor(layout) },
        // SAFETY: quiescence excludes all concurrent access to immediate fault and release state.
        Backing::Immediate(immediate) => unsafe { immediate.release_begin() },
        _ => std::ptr::null_mut(),
    };
    // SAFETY: each fresh cursor transfers its own retained payload account.
    let shared = unsafe { HewReleaseCursor::join([typed_read, shared]) };
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

#[cfg(not(target_arch = "wasm32"))]
mod native_blocking {
    use std::ffi::c_void;
    use std::sync::Arc;

    use super::{HewSink, HewStream};
    use crate::async_io::{self, HewAsyncIo, IoFailure};
    use crate::wake::HewWaker;

    /// # Safety
    /// The exclusive source loan survives this operation and its producer drain.
    pub(crate) unsafe fn read_content(
        stream: *mut HewStream,
    ) -> Result<Option<Vec<u8>>, IoFailure> {
        let _ = crate::stream::take_last_error();
        // SAFETY: the producer owns the exclusive heap-handle loan.
        let item = unsafe { (*stream).inner.next() };
        match IoFailure::take_recorded() {
            Some(failure) => Err(failure),
            None => Ok(item),
        }
    }

    /// # Safety
    /// The exclusive sink loan survives this operation and its producer drain.
    pub(crate) unsafe fn write_content(sink: *mut HewSink, data: &[u8]) -> Result<(), IoFailure> {
        let _ = crate::stream::take_last_error();
        // SAFETY: the producer owns the exclusive heap-handle loan.
        unsafe { (*sink).write_item(data) };
        match IoFailure::take_recorded() {
            Some(failure) => Err(failure),
            None => Ok(()),
        }
    }

    unsafe extern "C" fn retain_thread(context: *mut c_void) {
        // SAFETY: the synchronous adapter lends a live Arc<Thread> descriptor.
        unsafe { Arc::increment_strong_count(context.cast::<std::thread::Thread>()) };
    }

    unsafe extern "C" fn release_thread(context: *mut c_void) {
        // SAFETY: this consumes one reference retained by the operation.
        unsafe { Arc::decrement_strong_count(context.cast::<std::thread::Thread>()) };
    }

    unsafe extern "C" fn notify_thread(context: *mut c_void) {
        // SAFETY: the operation retains the thread through notification.
        unsafe { &*context.cast::<std::thread::Thread>() }.unpark();
    }

    /// Synchronous C ABI stream entries wait on the same readiness operation.
    /// Native coroutine entries above retain their ordinary suspend/resume path.
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

    pub(crate) fn blocking_tcp_read(connection: i32) -> Option<Vec<u8>> {
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

    pub(crate) fn blocking_tcp_write(connection: i32, content: &[u8]) {
        let operation = wait_tcp(|waker| {
            // SAFETY: the synchronous backing retains its borrowed connection.
            unsafe {
                async_io::start_tcp_stream_write(
                    connection,
                    async_io::content::Content::unbounded(content.to_vec()),
                    waker,
                )
            }
        });
        // SAFETY: wait_tcp transfers a ready, drained operation reference.
        unsafe { async_io::hew_async_io_free(operation) };
    }

    #[cfg(test)]
    pub(in super::super) struct TestReactor;

    #[cfg(test)]
    impl TestReactor {
        pub(in super::super) fn new() -> Self {
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
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(super) use native_blocking::TestReactor;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) use native_blocking::{
    blocking_tcp_read, blocking_tcp_write, read_content, write_content,
};

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::vec::HewTypeOwnershipKind;
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

    unsafe extern "C-unwind" fn drop_sink(slot: *mut c_void) {
        // SAFETY: the descriptor describes one owning sink handle.
        unsafe { super::super::hew_sink_close(slot.cast::<*mut HewSink>().read()) };
    }

    fn sink_layout() -> HewValueLayout {
        static MANAGED: hew_cabi::managed::HewManagedValueDescriptor =
            hew_cabi::managed::HewManagedValueDescriptor {
                slot_bytes: size_of::<*mut HewSink>() as u64,
                visit: Some(super::super::sender::hew_managed_visit_sink),
            };
        HewValueLayout {
            size: size_of::<*mut HewSink>(),
            align: align_of::<*mut HewSink>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: Some(drop_sink),
            release_start: None,
            code_owner: std::ptr::null(),
            managed: &raw const MANAGED,
        }
    }

    fn owned_layout() -> HewValueLayout {
        static MANAGED: hew_cabi::managed::HewManagedValueDescriptor =
            hew_cabi::managed::HewManagedValueDescriptor {
                slot_bytes: 8,
                visit: None,
            };
        HewValueLayout {
            code_owner: core::ptr::null(),
            managed: &raw const MANAGED,
            size: size_of::<usize>(),
            align: align_of::<usize>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: Some(drop_owned),

            release_start: None,
        }
    }

    #[test]
    fn ready_receive_defers_typed_values_to_their_owned_receipt() {
        let (signal, _notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let metadata = hew_cabi::managed::HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: None,
        };
        let layout = HewValueLayout {
            size: 8,
            align: 8,
            ownership_kind: hew_cabi::value::HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: std::ptr::null(),
            managed: &raw const metadata,
        };
        let mut sink = std::ptr::null_mut();
        // SAFETY: all endpoints, descriptors and borrowed source slots outlive
        // their operation and consuming cleanup. The output receives one owner.
        unsafe {
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            let mut value = 71_i64;
            let write = start_underlying(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
            )
            .unwrap();
            assert_eq!(hew_stream_write_poll_native(write), 1);
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(write));
            let mut output = -1_i64;
            let mut fault = std::ptr::null_mut();
            assert_eq!(
                hew_stream_read_ready_native(
                    stream,
                    &raw const layout,
                    (&raw mut output).cast(),
                    &raw mut fault
                ),
                0
            );
            assert_eq!(output, -1);
            assert!(fault.is_null());
            let read = hew_stream_read_start_native(
                stream,
                &raw const wake,
                &raw const layout,
                std::ptr::null_mut(),
            );
            assert_eq!(hew_stream_read_poll_native(read), 1);
            assert_eq!(
                hew_stream_read_take_native(read, (&raw mut output).cast()),
                1
            );
            assert_eq!(output, 71);
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(read));
            super::super::hew_sink_close(sink);
            super::super::hew_stream_close(stream);
        }
        assert_eq!(Arc::strong_count(&signal), 1);
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
                    envelope: Some((pointer as usize).to_ne_bytes().to_vec()),
                    read_stream: None,
                    io: platform::host_io::StreamIoFields::EMPTY,
                    fault: std::ptr::null_mut(),
                    _code: crate::code_owner::CodeRoot::acquire(layout.code_owner),
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
            let first_op = first_op.unwrap();
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
            let second_op = second_op.unwrap();
            assert_eq!(hew_stream_write_poll_native(second_op), 0);
            // Earlier admission may already have signalled the shared test waker.
            while notifications.try_recv().is_ok() {}
            hew_stream_cancel_native(second_op);
            assert_eq!(
                hew_stream_cleanup_status_native(second_op, &raw const wake),
                1
            );
            hew_stream_operation_free_native(second_op);
            // A refused typed admission leaves the source slot owned by its caller.
            drop_owned((&raw mut second).cast());
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            assert_eq!(Arc::strong_count(&signal), 1);
            super::super::hew_stream_close(stream);
            super::super::hew_sink_close(sink);
        }
        assert_eq!(drops.load(Ordering::SeqCst), 2);
        assert!(
            notifications.try_recv().is_err(),
            "abandoned producer must not wake"
        );
    }

    #[test]
    fn shared_write_cancel_before_admission_releases_its_typed_input() {
        let (signal, _) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let drops = Arc::new(AtomicUsize::new(0));
        let layout = owned_layout();
        let sink_layout = sink_layout();
        let mut sink = std::ptr::null_mut();
        // SAFETY: cancellation drains the operation before either endpoint closes.
        unsafe {
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            let mut value = Box::into_raw(Box::new(Arc::clone(&drops)));
            let operation = hew_stream_write_start_native(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                &raw const layout,
                std::ptr::null(),
                true,
                &raw const sink_layout,
                &mut 0,
            );
            hew_stream_cancel_native(operation);
            assert_eq!(
                hew_stream_cleanup_status_native(operation, &raw const wake),
                1
            );
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(operation));
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            super::super::hew_sink_close(sink);
            super::super::hew_stream_close(stream);
        }
    }

    #[test]
    fn shared_write_reserves_its_input_before_consuming_and_refunds_on_cancel() {
        let (signal, _) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let drops = Arc::new(AtomicUsize::new(0));
        let managed = hew_cabi::managed::HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: None,
        };
        let layout = HewValueLayout {
            managed: &raw const managed,
            ..owned_layout()
        };
        let sink_layout = sink_layout();
        // SAFETY: all descriptors, slots and endpoints remain live through cancellation.
        unsafe {
            let state =
                crate::coro_state::hew_coro_state_new(&raw const wake, std::ptr::null_mut());
            let owner = crate::owned_value_budget::ValueOwner::new(Some(7));
            (*state).execution_mut().value_owner = Some(owner.clone());
            let mut sink = std::ptr::null_mut();
            let stream = super::super::hew_stream_pipe_native(1, &raw const layout, &raw mut sink);
            let mut value = Box::into_raw(Box::new(Arc::clone(&drops)));
            let original = value;
            let mut status = 0;
            let refused = hew_stream_write_start_native(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                &raw const layout,
                state,
                true,
                &raw const sink_layout,
                &raw mut status,
            );
            assert!(refused.is_null());
            assert_eq!(status, hew_cabi::managed::HEAP_EXCEEDED);
            assert_eq!(value, original);
            assert_eq!(drops.load(Ordering::SeqCst), 0);
            assert_eq!(owner.live_bytes(), 0);
            let owner = crate::owned_value_budget::ValueOwner::new(Some(8));
            (*state).execution_mut().value_owner = Some(owner.clone());
            let operation = hew_stream_write_start_native(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                &raw const layout,
                state,
                true,
                &raw const sink_layout,
                &raw mut status,
            );
            assert!(!operation.is_null());
            assert_eq!(status, 0);
            assert_eq!(owner.live_bytes(), 8);
            hew_stream_cancel_native(operation);
            assert_eq!(
                hew_stream_cleanup_status_native(operation, &raw const wake),
                1
            );
            crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(operation));
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            assert_eq!(owner.live_bytes(), 0);
            super::super::hew_sink_close(sink);
            super::super::hew_stream_close(stream);
            crate::coro_state::hew_coro_state_free(state);
        }
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

    #[test]
    fn pending_content_write_transfers_its_producer_payload_and_refunds_on_completion() {
        let _runtime = crate::runtime_test_guard();
        let (entered, started) = mpsc::channel();
        let (release, released) = mpsc::channel();
        let release = ReleaseContent(release);
        let sink = super::super::into_sink_ptr(
            (entered, released),
            |(entered, released), _data| {
                entered.send(()).unwrap();
                released.recv().unwrap();
            },
            |_| {},
            |_| {},
        );
        let (signal, notifications) = mpsc::channel();
        let signal = Arc::new(signal);
        let wake = waker(&signal);
        let layout = &crate::layout_intrinsics::hew_layout_val_string;
        let managed = hew_cabi::managed::HewManagedValueDescriptor {
            slot_bytes: 0,
            visit: Some(hew_managed_visit_stream_operation),
        };
        let operation_layout = HewValueLayout {
            managed: &raw const managed,
            drop_fn: None,
            ..owned_layout()
        };
        // SAFETY: this test pins the operation and excludes its polls while transfer runs.
        unsafe {
            let state =
                crate::coro_state::hew_coro_state_new(&raw const wake, std::ptr::null_mut());
            let source = crate::owned_value_budget::ValueOwner::new(Some(1024));
            (*state).execution_mut().value_owner = Some(source.clone());
            let mut value = hew_cabi::string::string_from_str("content");
            source
                .acquire_value(layout, (&raw const value).cast())
                .unwrap()
                .commit();
            let operation = start_underlying(
                sink,
                &raw const wake,
                (&raw mut value).cast(),
                layout,
                state,
                true,
            )
            .unwrap();
            started.recv_timeout(Duration::from_secs(5)).unwrap();
            let capacity = source.live_bytes();
            assert!(capacity >= 8);
            let destination = crate::owned_value_budget::ValueOwner::new(Some(capacity));
            crate::owned_value_budget::transfer::ValueTransfer::prepare(
                Some(source.clone()),
                Some(destination.clone()),
                &raw const operation_layout,
                (&raw const operation).cast(),
            )
            .unwrap()
            .commit()
            .unwrap();
            assert_eq!(source.live_bytes(), 0);
            assert_eq!(destination.live_bytes(), capacity);
            drop(release);
            while hew_stream_cleanup_status_native(operation, &raw const wake) == 0 {
                notifications.recv_timeout(Duration::from_secs(5)).unwrap();
            }
            assert_eq!(destination.live_bytes(), 0);
            hew_stream_operation_free_native(operation);
            super::super::hew_sink_close(sink);
            crate::coro_state::hew_coro_state_free(state);
        }
    }

    #[test]
    fn content_read_reserves_output_before_consuming_its_retained_bytes() {
        let _runtime = crate::runtime_test_guard();
        for limit in [6, 12] {
            let stream = super::super::into_stream_ptr(super::super::VecStream {
                items: [b"result".to_vec()].into(),
            });
            let (signal, notifications) = mpsc::channel();
            let signal = Arc::new(signal);
            let wake = waker(&signal);
            let layout = &crate::layout_intrinsics::hew_layout_val_string;
            // SAFETY: this test retains every source, result and invocation until producer drain.
            unsafe {
                let state =
                    crate::coro_state::hew_coro_state_new(&raw const wake, std::ptr::null_mut());
                let owner = crate::owned_value_budget::ValueOwner::new(Some(limit));
                (*state).execution_mut().value_owner = Some(owner.clone());
                let operation =
                    hew_stream_read_start_native(stream, &raw const wake, layout, state);
                loop {
                    let status = hew_stream_read_poll_native(operation);
                    if status == 1 {
                        break;
                    }
                    assert_eq!(status, 0);
                    notifications.recv_timeout(Duration::from_secs(5)).unwrap();
                }
                while hew_stream_cleanup_status_native(operation, &raw const wake) == 0 {
                    notifications.recv_timeout(Duration::from_secs(5)).unwrap();
                }
                assert_eq!(owner.live_bytes(), 6);
                let sentinel = std::ptr::dangling_mut::<hew_cabi::string::HewString>();
                let mut output = sentinel;
                let status = hew_stream_read_take_native(operation, (&raw mut output).cast());
                if limit == 6 {
                    assert_eq!(status, 3);
                    assert_eq!(output, sentinel);
                    assert_eq!(owner.live_bytes(), 6);
                    let fault = hew_stream_operation_fault_native(operation);
                    assert_eq!(
                        (*fault).code(),
                        crate::internal::types::HEW_TRAP_HEAP_EXCEEDED
                    );
                    crate::fault::hew_fault_drop(fault);
                } else {
                    assert_eq!(status, 1);
                    assert_eq!(hew_cabi::string::string_as_bytes(output), b"result");
                    assert_eq!(owner.live_bytes(), 6);
                    crate::owned_value_budget::with_value_owner(&Some(owner.clone()), || {
                        crate::string::hew_string_drop_managed(output);
                    });
                }
                crate::release_walker::hew_release_sync(hew_stream_operation_release_begin(
                    operation,
                ));
                assert_eq!(owner.live_bytes(), 0);
                super::super::hew_stream_close(stream);
                crate::coro_state::hew_coro_state_free(state);
            }
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
        struct OperationOwner(*mut HewNativeStream);
        impl Drop for OperationOwner {
            fn drop(&mut self) {
                // SAFETY: the fixture owns its quiescent creator reference.
                unsafe { hew_stream_operation_free_native(self.0) };
            }
        }

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
