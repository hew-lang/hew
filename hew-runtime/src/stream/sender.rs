//! Cloned endpoints serialize one sender, its encoder and accepted remainder.

use super::HewSink;
use crate::callable::HewCallableValue;
use crate::vec::HewValueLayout;
use crate::wake::{OwnedWaker, ReadinessRegistrations};
use hew_cabi::sink::SinkOwners;
use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

unsafe extern "C-unwind" fn drop_bytes(slot: *mut c_void) {
    // SAFETY: this callback consumes the one bytes slot described below.
    unsafe { crate::bytes::hew_bytes_drop((*slot.cast::<crate::bytes::BytesTriple>()).ptr) };
}

pub(super) const BYTES: HewValueLayout = HewValueLayout {
    code_owner: core::ptr::null(),
    size: size_of::<crate::bytes::BytesTriple>(),
    align: align_of::<crate::bytes::BytesTriple>(),
    ownership_kind: crate::vec::HewTypeOwnershipKind::Bytes,
    clone_fn: None,
    drop_fn: Some(drop_bytes),
    release_start: None,
};

pub(crate) struct SharedSender {
    pub(super) sink: *mut HewSink,
    channel: *const crate::channel_core::ChannelCore,
    sink_layout: HewValueLayout,
    pub(super) encoder: Option<HewCallableValue>,
    encoder_layout: Option<HewValueLayout>,
    busy: AtomicBool,
    waiters: ReadinessRegistrations,
    endpoints: AtomicUsize,
    _code: [crate::code_owner::CodeRoot; 2],
}

// SAFETY: admission proves Send for the encoder; the gate serializes every access.
unsafe impl Send for SharedSender {}
// SAFETY: endpoint counts/readiness are atomic; sink and encoder require the exclusive gate.
unsafe impl Sync for SharedSender {}

impl SharedSender {
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn underlying(&self) -> *mut HewSink {
        self.sink
    }
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn idle(&self) -> bool {
        !self.busy.load(Ordering::Acquire)
    }
    pub(crate) fn observe(&self, waker: &Arc<OwnedWaker>) {
        self.waiters.register(waker);
    }
    pub(super) fn claim(&self) -> bool {
        self.busy
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
    }
    pub(super) fn release(&self) {
        self.busy.store(false, Ordering::Release);
        self.waiters.notify();
    }

    fn wait_claim(&self) {
        let (ready, waker) = crate::wake::blocking::Readiness::new();
        let waker = Arc::new(waker);
        self.observe(&waker);
        while !self.claim() {
            ready.wait();
        }
    }
}

#[expect(
    clippy::struct_excessive_bools,
    reason = "admission policy, gate, callback output and cancellation track independent ownership facts"
)]
pub(super) struct SharedWrite {
    owner: Arc<SharedSender>,
    child: *mut super::native::HewNativeStream,
    claimed: bool,
    observed: bool,
    input: Option<(*mut c_void, std::alloc::Layout, bool)>,
    fault: *mut crate::fault::HewFault,
    source: *mut HewSink,
    parent: *const crate::coro_state::HewCoroState,
    park: bool,
    invocation: crate::callable::BorrowedInvocation,
    argument: *mut c_void,
    encoded: std::mem::MaybeUninit<crate::bytes::BytesTriple>,
    encoded_live: bool,
    cancelled: bool,
}

impl SharedWrite {
    pub(super) fn new(
        owner: Arc<SharedSender>,
        source: *mut HewSink,
        parent: *const crate::coro_state::HewCoroState,
        park: bool,
    ) -> Self {
        Self {
            owner,
            child: std::ptr::null_mut(),
            claimed: false,
            observed: false,
            input: None,
            fault: std::ptr::null_mut(),
            source,
            parent,
            park,
            invocation: crate::callable::BorrowedInvocation::new(),
            argument: std::ptr::null_mut(),
            encoded: std::mem::MaybeUninit::uninit(),
            encoded_live: false,
            cancelled: false,
        }
    }

    pub(super) unsafe fn poll(
        &mut self,
        envelope: &mut Option<Vec<u8>>,
        layout: &HewValueLayout,
        waker: &Arc<OwnedWaker>,
    ) -> i32 {
        if !self.fault.is_null() {
            return 3;
        }
        if !self.claimed {
            if self.park && !self.observed {
                self.owner.observe(waker);
                self.observed = true;
            }
            // SAFETY: the source loan pins the installed owner through admission.
            let admission = unsafe {
                (*self.source).with_shared(
                    |_| std::process::abort(),
                    |_, closed| {
                        if closed {
                            2
                        } else {
                            i32::from(self.owner.claim())
                        }
                    },
                )
            };
            if admission != 1 {
                return if admission == 2 {
                    2
                } else if self.park {
                    0
                } else {
                    4
                };
            }
            self.claimed = true;
            let allocation = std::alloc::Layout::from_size_align(layout.size.max(1), layout.align)
                .unwrap_or_else(|_| std::process::abort());
            // SAFETY: the runtime transfers its owned envelope into an exact aligned slot.
            let slot = unsafe { std::alloc::alloc(allocation) }.cast::<c_void>();
            if slot.is_null() {
                std::alloc::handle_alloc_error(allocation);
            }
            // SAFETY: the owned envelope initializes this exact aligned input slot once.
            unsafe {
                crate::channel_common::decode_elem_envelope(
                    envelope.take(),
                    slot,
                    layout,
                    "shared sink input",
                )
            };
            self.input = Some((slot, allocation, self.owner.encoder.is_some()));
            self.argument = slot;
        }
        if self.child.is_null() {
            let (data, item_layout) = if let Some(encoder) = &self.owner.encoder {
                // SAFETY: boxed state pins the argument array, input, output and gated environment.
                match unsafe {
                    self.invocation.poll(
                        self.parent,
                        encoder,
                        &raw const self.argument,
                        self.encoded.as_mut_ptr().cast(),
                    )
                } {
                    crate::coro_state::CoroStatus::Pending => return 0,
                    crate::coro_state::CoroStatus::Yielded => std::process::abort(),
                    crate::coro_state::CoroStatus::Complete => self.encoded_live = true,
                    crate::coro_state::CoroStatus::Fault
                    | crate::coro_state::CoroStatus::Cancelled => {
                        self.fault = self.invocation.take_fault();
                        return 3;
                    }
                }
                (self.encoded.as_mut_ptr().cast(), &BYTES)
            } else {
                (self.argument, layout)
            };
            // SAFETY: the admission gate exclusively lends the backing; the child takes its input.
            self.child = unsafe {
                super::native::start_underlying(
                    self.owner.sink,
                    waker.descriptor(),
                    data,
                    item_layout,
                    self.parent,
                    self.park,
                )
            };
            self.encoded_live = false;
        }
        // SAFETY: this owner drives its unique underlying operation.
        unsafe { super::native::hew_stream_write_poll_native(self.child) }
    }

    pub(super) unsafe fn take_fault(&mut self) -> *mut crate::fault::HewFault {
        if !self.fault.is_null() {
            return std::mem::replace(&mut self.fault, std::ptr::null_mut());
        }
        // SAFETY: the child reported failure and remains retained through its fault transfer.
        unsafe { super::native::hew_stream_operation_fault_native(self.child) }
    }

    pub(super) unsafe fn committed(&self) -> i64 {
        if self.child.is_null() {
            return 0;
        }
        // SAFETY: the retained child reported the timeout whose byte count is requested.
        unsafe { super::native::hew_stream_write_committed_native(self.child) }
    }

    pub(super) unsafe fn cancel(&mut self) {
        self.cancelled = true;
        self.invocation.cancel();
        if !self.child.is_null() {
            // SAFETY: this write owns its pending child operation.
            unsafe { super::native::hew_stream_cancel_native(self.child) };
        }
    }

    pub(super) unsafe fn cleanup(&mut self, waker: *const crate::wake::HewWaker) -> i32 {
        if self.cancelled && self.claimed && self.child.is_null() {
            if let Some(encoder) = &self.owner.encoder {
                // SAFETY: all invocation slots and the gated environment remain pinned until terminal cleanup.
                match unsafe {
                    self.invocation.poll(
                        self.parent,
                        encoder,
                        &raw const self.argument,
                        self.encoded.as_mut_ptr().cast(),
                    )
                } {
                    crate::coro_state::CoroStatus::Pending => return 0,
                    crate::coro_state::CoroStatus::Yielded => std::process::abort(),
                    crate::coro_state::CoroStatus::Complete => self.encoded_live = true,
                    crate::coro_state::CoroStatus::Fault
                    | crate::coro_state::CoroStatus::Cancelled => {
                        let fault = self.invocation.take_fault();
                        // SAFETY: cancellation abandons this owned callback fault after its frame drained.
                        unsafe { crate::fault::hew_fault_drop(fault) };
                    }
                }
            }
        }
        // SAFETY: the child's source loan remains held by this sender gate.
        if !self.child.is_null()
            // SAFETY: the owned child remains live while its producer drains.
            && unsafe { super::native::hew_stream_cleanup_status_native(self.child, waker) } == 0
        {
            return 0;
        }
        if self.claimed {
            self.owner.release();
            self.claimed = false;
        }
        1
    }

    pub(super) unsafe fn release_cursor(
        &mut self,
        layout: HewValueLayout,
    ) -> *mut crate::release_walker::HewReleaseCursor {
        use crate::release_walker::{HewReleaseCursor, ReleaseItem};
        let child = if self.child.is_null() {
            std::ptr::null_mut()
        } else {
            // SAFETY: cleanup established child quiescence before ownership transfers to its cursor.
            unsafe {
                super::native::hew_stream_operation_release_begin(std::mem::replace(
                    &mut self.child,
                    std::ptr::null_mut(),
                ))
            }
        };
        let own = self
            .input
            .take()
            .map_or(std::ptr::null_mut(), |(slot, allocation, owned)| {
                let mut pending = vec![ReleaseItem::Allocation {
                    pointer: slot,
                    size: allocation.size(),
                    align: allocation.align(),
                }];
                if owned {
                    pending.push(ReleaseItem::Value { slot, layout });
                }
                HewReleaseCursor::new(pending)
            });
        let encoded = if std::mem::take(&mut self.encoded_live) {
            // SAFETY: a completed but abandoned callback result transfers once before boxed storage frees.
            unsafe { HewReleaseCursor::detached(self.encoded.as_ptr().cast(), BYTES) }
        } else {
            std::ptr::null_mut()
        };
        // SAFETY: these fresh cursors transfer child, input and abandoned result obligations.
        unsafe { HewReleaseCursor::join(vec![child, own, encoded]) }
    }
}

impl Drop for SharedWrite {
    fn drop(&mut self) {
        if self.claimed || !self.child.is_null() || self.input.is_some() || self.encoded_live {
            std::process::abort();
        }
        // SAFETY: an unobserved fault remains uniquely owned by this completed write.
        unsafe { crate::fault::hew_fault_drop(self.fault) };
    }
}

type Endpoint = Option<Arc<SharedSender>>;

/// # Safety
/// A retained endpoint or pending finish pins this exact adapter allocation.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn retain_adapter(context: *const c_void) -> Arc<SharedSender> {
    let context = context.cast::<SharedSender>();
    // SAFETY: the backing retains one Arc while this reference is acquired.
    unsafe {
        Arc::increment_strong_count(context);
        Arc::from_raw(context)
    }
}

fn write(_endpoint: &mut Endpoint, _data: &[u8]) {
    crate::channel_common::abort_elem_witness(
        "shared sink",
        "writes require typed native admission",
    );
}

fn flush(endpoint: &mut Endpoint) {
    let Some(owner) = endpoint else {
        return;
    };
    owner.wait_claim();
    // SAFETY: the endpoint pins this underlying handle and owns its admission gate.
    unsafe { (*owner.sink).flush() };
    owner.release();
}

fn retire(endpoint: &mut Endpoint) -> bool {
    let Some(owner) = endpoint else {
        return false;
    };
    owner.endpoints.fetch_sub(1, Ordering::AcqRel) == 1
}

fn finish(endpoint: &mut Endpoint) {
    let Some(owner) = endpoint else {
        return;
    };
    owner.wait_claim();
    // SAFETY: the last open endpoint acquired the gate after all competing writes drained.
    unsafe { (*owner.sink).close() };
    owner.release();
}

fn owners(endpoint: &mut Endpoint) -> Option<SinkOwners> {
    unsafe fn free(owner: *mut c_void) {
        // SAFETY: the cursor consumed both exact value slots before their storage.
        unsafe { drop(Box::from_raw(owner.cast::<SharedSender>())) };
    }
    let mut owner = Box::new(Arc::into_inner(endpoint.take()?)?);
    let mut values = Vec::new();
    if let (Some(encoder), Some(layout)) = (&mut owner.encoder, owner.encoder_layout) {
        values.push(((encoder as *mut HewCallableValue).cast(), layout));
    }
    values.push(((&raw mut owner.sink).cast(), owner.sink_layout));
    Some(SinkOwners {
        values,
        storage: Box::into_raw(owner).cast(),
        free,
    })
}

fn endpoint(owner: Arc<SharedSender>, finished: bool) -> *mut HewSink {
    let context = Arc::as_ptr(&owner).cast();
    // SAFETY: the endpoint Arc pins this exact adapter identity until owners transfers it.
    unsafe {
        hew_cabi::sink::into_owned_sink_ptr(
            Some(owner),
            context,
            finished,
            write,
            flush,
            retire,
            finish,
            owners,
        )
    }
}

/// # Safety
/// The caller retains an open endpoint, which pins its underlying channel.
pub(super) unsafe fn fault_channel(
    owner: &SharedSender,
) -> Option<&crate::channel_core::ChannelCore> {
    // SAFETY: an open endpoint prevents the underlying backing from finishing.
    unsafe { owner.channel.as_ref() }
}

unsafe fn channel(sink: *mut HewSink) -> *const crate::channel_core::ChannelCore {
    // SAFETY: the constructor exclusively borrows this live source endpoint.
    if unsafe { (*sink).is_closed() } {
        return std::ptr::null();
    }
    // SAFETY: the source endpoint pins its shared backing through construction.
    if let Some(owner) = unsafe { shared(sink) } {
        owner.channel
    } else {
        // SAFETY: the original channel backing is retained until its last open endpoint finishes.
        unsafe { (*sink).channel_core_ptr().cast() }
    }
}

/// # Safety
/// Cleanup holds this endpoint exclusively after its own write has drained.
pub(super) unsafe fn abandon_pending(sink: *mut HewSink) {
    if sink.is_null() {
        return;
    }
    // SAFETY: this exclusive cleanup loan retains the live endpoint.
    if unsafe { (*sink).is_closed() } {
        return;
    }
    // SAFETY: the retained endpoint pins any shared sender.
    if let Some(owner) = unsafe { shared(sink) } {
        if owner.endpoints.load(Ordering::Acquire) != 1 {
            return;
        }
        if !owner.claim() {
            std::process::abort();
        }
        // SAFETY: the last open endpoint owns the underlying sink's sole active write loan.
        unsafe { abandon_pending(owner.sink) };
        owner.release();
    } else {
        // SAFETY: the ordinary sink is exclusively borrowed after producer quiescence.
        unsafe { drop((*sink).take_pending()) };
    }
}

/// Borrow the shared sender while a live endpoint pins its backing.
/// # Safety
/// The endpoint is exclusively borrowed until the operation is quiescent.
pub(super) unsafe fn shared(sink: *mut HewSink) -> Option<Arc<SharedSender>> {
    if sink.is_null() {
        return None;
    }
    // SAFETY: this live endpoint owns the Arc allocation named by its adapter identity.
    let context = unsafe { (*sink).runtime_adapter() }.cast::<SharedSender>();
    if context.is_null() {
        return None;
    }
    // SAFETY: the endpoint's retained Arc pins this exact shared owner allocation.
    unsafe {
        Arc::increment_strong_count(context);
        Some(Arc::from_raw(context))
    }
}

/// Transfer one byte sink and a checked synchronous encoder into a sender.
/// # Safety
/// Both value descriptors are exact, immutable and outlive every cloned endpoint.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_from_encoder(
    sink: *mut HewSink,
    encoder: *mut HewCallableValue,
    sink_layout: *const HewValueLayout,
    encoder_layout: *const HewValueLayout,
) -> *mut HewSink {
    // SAFETY: the compiler transfers one complete callable carrier.
    let encoder = unsafe { encoder.read() };
    // SAFETY: construction exclusively consumes the underlying endpoint.
    let finished = unsafe { (*sink).is_closed() };
    // SAFETY: this consumed endpoint retains its channel until its last producer finishes.
    let channel = unsafe { channel(sink) };
    endpoint(
        Arc::new(SharedSender {
            // SAFETY: both transferred owners retain their checked descriptors.
            _code: unsafe {
                [
                    crate::code_owner::CodeRoot::acquire((*sink_layout).code_owner),
                    crate::code_owner::CodeRoot::acquire((*encoder_layout).code_owner),
                ]
            },
            sink,
            channel,
            encoder: Some(encoder),
            // SAFETY: the compiler provides both checked static value descriptors.
            sink_layout: unsafe { *sink_layout },
            // SAFETY: this checked encoder descriptor is immutable and static.
            encoder_layout: Some(unsafe { *encoder_layout }),
            busy: AtomicBool::new(false),
            waiters: ReadinessRegistrations::default(),
            endpoints: AtomicUsize::new(usize::from(!finished)),
        }),
        finished,
    )
}

/// Clone an endpoint without duplicating its underlying resource or codec.
/// # Safety
/// The caller borrows a live sink and provides its exact static value descriptor.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_clone(
    sink: *mut HewSink,
    layout: *const HewValueLayout,
) -> *mut HewSink {
    // SAFETY: the source read loan pins this endpoint through atomic owner retention.
    let (owner, closed) = unsafe { retain(sink, layout, true) };
    endpoint(owner, closed)
}

/// # Safety
/// The source read loan pins the endpoint and its exact static descriptor.
pub(super) unsafe fn retain(
    sink: *mut HewSink,
    layout: *const HewValueLayout,
    clone_endpoint: bool,
) -> (Arc<SharedSender>, bool) {
    // SAFETY: installation only allocates the shared owner and endpoint; no authored
    // callback or continuation executes while the endpoint installation lock is held.
    unsafe {
        (*sink).with_shared(
            |original| {
                let closed = original.is_closed();
                let channel = original.channel_core_ptr().cast();
                let owner = Arc::new(SharedSender {
                    _code: [
                        crate::code_owner::CodeRoot::acquire((*layout).code_owner),
                        crate::code_owner::CodeRoot::acquire(std::ptr::null()),
                    ],
                    sink: Box::into_raw(Box::new(original)),
                    channel,
                    encoder: None,
                    sink_layout: *layout,
                    encoder_layout: None,
                    busy: AtomicBool::new(false),
                    waiters: ReadinessRegistrations::default(),
                    endpoints: AtomicUsize::new(usize::from(!closed)),
                });
                *Box::from_raw(endpoint(owner, closed))
            },
            |context, closed| {
                let context = context.cast::<SharedSender>();
                Arc::increment_strong_count(context);
                let owner = Arc::from_raw(context);
                if clone_endpoint && !closed {
                    owner.endpoints.fetch_add(1, Ordering::AcqRel);
                }
                (owner, closed)
            },
        )
    }
}
