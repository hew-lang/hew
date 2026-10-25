//! Cloned endpoints serialize one sender, its encoder and accepted remainder.

use super::HewSink;
use crate::callable::HewCallableValue;
use crate::vec::HewValueLayout;
use crate::wake::{OwnedWaker, ReadinessRegistrations};
use hew_cabi::sink::SinkOwners;
use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

// Content encoding uses the Bytes envelope mode while retaining the canonical
// Bytes ownership and accounting descriptor used by runtime collections.
pub(super) static BYTES: HewValueLayout = HewValueLayout {
    ownership_kind: crate::vec::HewTypeOwnershipKind::Bytes,
    ..crate::layout_intrinsics::hew_layout_val_bytes
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
    fn slot_bytes(&self) -> Result<u64, i32> {
        let Some(encoder) = &self.encoder_layout else {
            return Ok(0);
        };
        // SAFETY: the shared owner retains both exact descriptors and their defining code.
        unsafe {
            crate::owned_value_budget::visit::slot_bytes(&self.sink_layout)?
                .checked_add(crate::owned_value_budget::visit::slot_bytes(encoder)?)
                .ok_or(hew_cabi::managed::HEAP_EXCEEDED)
        }
    }

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

/// A sink retains its encoder's owned values; the receiving stream alone owns queued items.
/// # Safety
/// The slot contains a retained endpoint, and authored mutation/teardown is excluded.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_sink(
    slot: *const c_void,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the endpoint pins the shared adapter until its semantic release expands it.
    unsafe {
        let sink = slot.cast::<*const HewSink>().read();
        let Some(sink) = sink.as_ref() else {
            return 0;
        };
        let Some(owner) = sink.runtime_adapter().cast::<SharedSender>().as_ref() else {
            return 0;
        };
        visit_shared(owner, visitor)
    }
}

unsafe fn visit_shared(
    owner: &SharedSender,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    use hew_cabi::managed::{visit_value, DESCEND, SKIP_DESCENDANTS};
    // A lazily installed plain sender contains only protocol handles, with no typed payload.
    let Some(encoder_layout) = &owner.encoder_layout else {
        return 0;
    };
    let bytes = match owner.slot_bytes() {
        Ok(bytes) => bytes,
        Err(status) => return status,
    };
    // SAFETY: immutable owned fields stay at stable Arc addresses through the entire walk.
    unsafe {
        match crate::owned_value_budget::visit::storage(
            visitor,
            std::ptr::from_ref(owner).cast(),
            bytes,
        ) {
            DESCEND => {}
            SKIP_DESCENDANTS => return 0,
            failure => return failure,
        }
        let status = visit_value(
            &owner.sink_layout,
            std::ptr::from_ref(&owner.sink).cast(),
            visitor,
        );
        if status != 0 {
            return status;
        }
        visit_value(
            encoder_layout,
            std::ptr::from_ref(owner.encoder.as_ref().expect("encoder descriptor")).cast(),
            visitor,
        )
    }
}

unsafe extern "C" fn visit_shared_slot(
    slot: *const c_void,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: this private descriptor receives one retained shared-owner pointer.
    unsafe { visit_shared(&*slot.cast::<*const SharedSender>().read(), visitor) }
}

static SHARED_OWNER: HewValueLayout = HewValueLayout {
    size: size_of::<*const SharedSender>(),
    align: align_of::<*const SharedSender>(),
    ownership_kind: crate::vec::HewTypeOwnershipKind::Plain,
    clone_fn: None,
    drop_fn: None,
    release_start: None,
    code_owner: std::ptr::null(),
    managed: &hew_cabi::managed::HewManagedValueDescriptor {
        slot_bytes: 0,
        visit: Some(visit_shared_slot),
    },
};

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
    value_owner: Option<crate::owned_value_budget::ValueOwner>,
    input_slot_bytes: u64,
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
    /// The enclosing invocation freezes this operation before visiting its owning inputs.
    pub(super) unsafe fn visit_managed(
        &mut self,
        layout: &HewValueLayout,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        use hew_cabi::managed::{visit_value, HewManagedWalkKind, DESCEND, SKIP_DESCENDANTS};
        // SAFETY: the operation and every initialized slot remain pinned and frozen.
        unsafe {
            // Stop any unpublished child admission before its borrowed input is frozen elsewhere.
            let status = super::native::hew_managed_visit_stream_operation(
                std::ptr::from_ref(&self.child).cast(),
                visitor,
            );
            if status != 0 {
                return status;
            }
            if let Some((slot, _, owned)) = self.input {
                match crate::owned_value_budget::visit::storage(
                    visitor,
                    slot,
                    self.input_slot_bytes,
                ) {
                    DESCEND if owned => {
                        let status = visit_value(layout, slot, visitor);
                        if status != 0 {
                            return status;
                        }
                    }
                    DESCEND | SKIP_DESCENDANTS => {}
                    failure => return failure,
                }
            }
            if self.encoded_live {
                let status = visit_value(&BYTES, self.encoded.as_ptr().cast(), visitor);
                if status != 0 {
                    return status;
                }
            }
            let status = self.invocation.visit_managed(visitor);
            if status != 0 {
                return status;
            }
            if (*visitor).kind == HewManagedWalkKind::CommitTransfer {
                self.value_owner = crate::owned_value_budget::transfer::destination_owner(visitor);
            }
            0
        }
    }

    pub(super) unsafe fn new(
        owner: Arc<SharedSender>,
        source: *mut HewSink,
        parent: *const crate::coro_state::HewCoroState,
        park: bool,
        data: *const c_void,
        layout: &HewValueLayout,
    ) -> Result<Self, i32> {
        // SAFETY: the caller retains the optional parent invocation.
        let value_owner =
            unsafe { crate::owned_value_budget::materialize::invocation_owner(parent) };
        // Unbounded foreign descriptors may lack metadata; a bounded caller must describe its input.
        let input_slot_bytes = unsafe { crate::owned_value_budget::visit::slot_bytes(layout) }
            .or_else(|failure| {
                if value_owner.is_none() {
                    Ok(0)
                } else {
                    Err(failure)
                }
            })?;
        let reservation = value_owner
            .as_ref()
            .map(|owner| owner.reserve_new(input_slot_bytes))
            .transpose()
            .map_err(|_| hew_cabi::managed::HEAP_EXCEEDED)?;
        let allocation = std::alloc::Layout::from_size_align(layout.size.max(1), layout.align)
            .unwrap_or_else(|_| std::process::abort());
        // SAFETY: this operation takes the initialized source into aligned storage.
        let slot = unsafe { std::alloc::alloc(allocation) }.cast::<c_void>();
        if slot.is_null() {
            std::alloc::handle_alloc_error(allocation);
        }
        // SAFETY: the caller transfers one complete value without cloning its children.
        unsafe {
            std::ptr::copy_nonoverlapping(data.cast::<u8>(), slot.cast(), layout.size);
        }
        if let Some(reservation) = reservation {
            reservation.commit(&[crate::owned_value_budget::ManagedStorage {
                identity: slot as usize,
                bytes: input_slot_bytes,
            }]);
        }
        Ok(Self {
            value_owner,
            input_slot_bytes,
            owner,
            child: std::ptr::null_mut(),
            claimed: false,
            observed: false,
            input: Some((slot, allocation, true)),
            fault: std::ptr::null_mut(),
            source,
            parent,
            park,
            invocation: crate::callable::BorrowedInvocation::new(),
            argument: slot,
            encoded: std::mem::MaybeUninit::uninit(),
            encoded_live: false,
            cancelled: false,
        })
    }

    pub(super) unsafe fn poll(&mut self, layout: &HewValueLayout, waker: &Arc<OwnedWaker>) -> i32 {
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
            let child = unsafe {
                super::native::start_underlying(
                    self.owner.sink,
                    waker.descriptor(),
                    data,
                    item_layout,
                    self.parent,
                    self.park,
                )
            };
            self.child = match child {
                Ok(child) => child,
                Err(status) => {
                    self.fault = crate::fault::hew_fault_new(
                        if status == hew_cabi::managed::HEAP_EXCEEDED {
                            crate::internal::types::HEW_TRAP_HEAP_EXCEEDED
                        } else {
                            crate::internal::types::HEW_TRAP_USER_PANIC
                        },
                    );
                    return 3;
                }
            };
            if !unsafe { super::native::borrows_write_input(self.child) } {
                self.child_consumed_input();
            }
        }
        // SAFETY: this owner drives its unique underlying operation.
        let status = unsafe { super::native::hew_stream_write_poll_native(self.child) };
        if status == 1 && unsafe { super::native::borrows_write_input(self.child) } {
            self.child_consumed_input();
        }
        if status == 0 && !self.park {
            4
        } else {
            status
        }
    }

    fn child_consumed_input(&mut self) {
        if self.owner.encoder.is_none() {
            if let Some((slot, allocation, _)) = self.input.take() {
                // SAFETY: successful admission consumed the source representation without retaining a borrow.
                unsafe { std::alloc::dealloc(slot.cast(), allocation) };
                if let Some(owner) = &self.value_owner {
                    owner.release_backing(slot, self.input_slot_bytes);
                }
                self.argument = std::ptr::null_mut();
            }
        }
        self.encoded_live = false;
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
                if self.value_owner.is_some() {
                    unsafe fn keep_allocation(_pointer: *mut c_void) {}
                    pending.push(ReleaseItem::ManagedBacking {
                        pointer: slot,
                        bytes: self.input_slot_bytes,
                        free: keep_allocation,
                    });
                }
                if owned {
                    pending.push(ReleaseItem::Value { slot, layout });
                }
                let cursor = HewReleaseCursor::new(pending);
                // SAFETY: this fresh cursor owns the retained input and its allocation.
                if let Some(cursor) = unsafe { cursor.as_mut() } {
                    cursor.set_value_owner(self.value_owner.clone());
                }
                cursor
            });
        let encoded = if std::mem::take(&mut self.encoded_live) {
            // SAFETY: a completed but abandoned callback result transfers once before boxed storage frees.
            unsafe { HewReleaseCursor::detached(self.encoded.as_ptr().cast(), BYTES) }
        } else {
            std::ptr::null_mut()
        };
        // SAFETY: these fresh cursors transfer child, input and abandoned result obligations.
        unsafe {
            let cursor = HewReleaseCursor::join(vec![child, own, encoded]);
            if let Some(cursor) = cursor.as_mut() {
                cursor.set_value_owner(self.value_owner.clone());
            }
            cursor
        }
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
        unsafe { drop(Box::from_raw(owner.cast::<Arc<SharedSender>>())) };
    }
    let mut owner = Box::new(endpoint.take()?);
    let root = Arc::as_ptr(&owner);
    let Some(value) = Arc::get_mut(&mut owner) else {
        if let Some(account) = crate::owned_value_budget::current_owner() {
            // SAFETY: another endpoint retains the immutable owned fields after this alias ends.
            unsafe { account.release_value(&SHARED_OWNER, (&raw const root).cast()) }
                .unwrap_or_else(|_| std::process::abort());
        }
        return None;
    };
    let managed_backing = value.encoder_layout.as_ref().map(|_| {
        (
            root.cast(),
            value.slot_bytes().unwrap_or_else(|_| std::process::abort()),
        )
    });
    let mut values = Vec::new();
    if let (Some(encoder), Some(layout)) = (&mut value.encoder, value.encoder_layout) {
        values.push(((encoder as *mut HewCallableValue).cast(), layout));
    }
    values.push(((&raw mut value.sink).cast(), value.sink_layout));
    Some(SinkOwners {
        values,
        storage: Box::into_raw(owner).cast(),
        free,
        managed_backing,
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
    out_status: *mut i32,
) -> *mut HewSink {
    // SAFETY: the compiler provides writable status and live concrete descriptors.
    unsafe { out_status.write(0) };
    let width = unsafe {
        crate::owned_value_budget::visit::slot_bytes(&*sink_layout).and_then(|sink| {
            crate::owned_value_budget::visit::slot_bytes(&*encoder_layout).and_then(|encoder| {
                sink.checked_add(encoder)
                    .ok_or(hew_cabi::managed::HEAP_EXCEEDED)
            })
        })
    };
    let width = match width {
        Ok(width) => width,
        Err(status) => {
            unsafe { out_status.write(status) };
            return std::ptr::null_mut();
        }
    };
    let reservation = match crate::owned_value_budget::current_owner()
        .map(|owner| owner.reserve_new(width))
        .transpose()
    {
        Ok(reservation) => reservation,
        Err(_) => {
            unsafe { out_status.write(hew_cabi::managed::HEAP_EXCEEDED) };
            return std::ptr::null_mut();
        }
    };
    // SAFETY: the compiler transfers one complete callable carrier.
    let encoder = unsafe { encoder.read() };
    // SAFETY: construction exclusively consumes the underlying endpoint.
    let finished = unsafe { (*sink).is_closed() };
    // SAFETY: this consumed endpoint retains its channel until its last producer finishes.
    let channel = unsafe { channel(sink) };
    let owner = Arc::new(SharedSender {
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
    });
    if let Some(reservation) = reservation {
        // The two owned inputs move beneath this new shared root; their existing
        // descendant interests remain unchanged in the same incarnation.
        reservation.commit(&[crate::owned_value_budget::ManagedStorage {
            identity: Arc::as_ptr(&owner) as usize,
            bytes: width,
        }]);
    }
    endpoint(owner, finished)
}

/// Clone an endpoint after acquiring this incarnation's shared payload interest.
/// # Safety
/// The source loan and its descriptor remain live; status is writable.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_clone_checked(
    sink: *mut HewSink,
    layout: *const HewValueLayout,
    out_status: *mut i32,
) -> *mut HewSink {
    unsafe { out_status.write(0) };
    let acquired = match crate::owned_value_budget::current_owner()
        .map(|owner| {
            // SAFETY: the immutable source loan pins the descriptor-selected graph.
            unsafe { owner.acquire_value(&*layout, (&raw const sink).cast()) }
        })
        .transpose()
    {
        Ok(acquired) => acquired,
        Err(status) => {
            unsafe { out_status.write(status) };
            return std::ptr::null_mut();
        }
    };
    // SAFETY: the existing physical clone does not duplicate account acquisition.
    let result = unsafe { hew_sink_clone(sink, layout) };
    if let Some(acquired) = acquired {
        acquired.commit();
    }
    result
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::owned_value_budget::{with_value_owner, ValueOwner};
    use hew_cabi::managed::HewManagedValueDescriptor;

    unsafe extern "C-unwind" fn drop_sink(slot: *mut c_void) {
        unsafe { super::super::hew_sink_close(slot.cast::<*mut HewSink>().read()) };
    }

    unsafe extern "C-unwind" fn drop_callable(slot: *mut c_void) {
        unsafe { crate::callable::hew_callable_drop(slot.cast()) };
    }

    #[test]
    fn shared_encoder_reserves_before_move_and_releases_each_actor_interest() {
        let sink_layout = HewValueLayout {
            size: size_of::<*mut HewSink>(),
            align: align_of::<*mut HewSink>(),
            ownership_kind: crate::vec::HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: Some(drop_sink),
            release_start: None,
            code_owner: std::ptr::null(),
            managed: &HewManagedValueDescriptor {
                slot_bytes: 8,
                visit: Some(hew_managed_visit_sink),
            },
        };
        let callable_layout = HewValueLayout {
            size: size_of::<HewCallableValue>(),
            align: align_of::<HewCallableValue>(),
            ownership_kind: crate::vec::HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: Some(drop_callable),
            release_start: None,
            code_owner: std::ptr::null(),
            managed: &HewManagedValueDescriptor {
                slot_bytes: 8,
                visit: Some(crate::owned_value_budget::visit::hew_managed_visit_callable),
            },
        };
        let empty_layout = HewValueLayout {
            size: 0,
            align: 1,
            ownership_kind: crate::vec::HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: std::ptr::null(),
            managed: &HewManagedValueDescriptor {
                slot_bytes: 0,
                visit: None,
            },
        };
        let descriptor = crate::callable::HewCallableDescriptor {
            environment: &empty_layout,
            invoke_borrow: None,
            invoke_once: None,
            invoke_borrow_sync: None,
            invoke_once_sync: None,
            specializations: std::ptr::null(),
        };
        let mut encoder = HewCallableValue {
            environment: std::ptr::null_mut(),
            descriptor: &descriptor,
        };
        let closes = Arc::new(AtomicUsize::new(0));
        let sink = super::super::into_sink_ptr(
            Arc::clone(&closes),
            |_, _| {},
            |_| {},
            |count| {
                count.fetch_add(1, Ordering::SeqCst);
            },
        );
        // SAFETY: all descriptors and retained endpoints outlive every alias and its cleanup.
        unsafe {
            let too_small = ValueOwner::new(Some(15));
            let mut status = 0;
            let refused = with_value_owner(&Some(too_small.clone()), || {
                hew_sink_from_encoder(
                    sink,
                    &raw mut encoder,
                    &sink_layout,
                    &callable_layout,
                    &mut status,
                )
            });
            assert!(refused.is_null());
            assert_eq!(status, hew_cabi::managed::HEAP_EXCEEDED);
            assert_eq!(encoder.descriptor, &raw const descriptor);
            assert!(!(*sink).is_closed());
            assert_eq!(too_small.live_bytes(), 0);
            let source = ValueOwner::new(Some(16));
            let endpoint = with_value_owner(&Some(source.clone()), || {
                hew_sink_from_encoder(
                    sink,
                    &raw mut encoder,
                    &sink_layout,
                    &callable_layout,
                    &mut status,
                )
            });
            assert!(!endpoint.is_null());
            assert_eq!(status, 0);
            assert_eq!(source.live_bytes(), 16);
            let copy = with_value_owner(&Some(source.clone()), || {
                hew_sink_clone_checked(endpoint, &sink_layout, &mut status)
            });
            assert_eq!(status, 0);
            assert_eq!(source.live_bytes(), 16);
            let destination = ValueOwner::new(Some(16));
            crate::owned_value_budget::transfer::ValueTransfer::prepare(
                Some(source.clone()),
                Some(destination.clone()),
                &sink_layout,
                (&raw const copy).cast(),
            )
            .unwrap()
            .commit()
            .unwrap();
            assert_eq!(source.live_bytes(), 16);
            assert_eq!(destination.live_bytes(), 16);
            with_value_owner(&Some(source.clone()), || {
                super::super::hew_sink_close(endpoint)
            });
            assert_eq!(source.live_bytes(), 0);
            assert_eq!(destination.live_bytes(), 16);
            assert_eq!(closes.load(Ordering::SeqCst), 0);
            with_value_owner(&Some(destination.clone()), || {
                super::super::hew_sink_close(copy)
            });
            assert_eq!(destination.live_bytes(), 0);
            assert_eq!(closes.load(Ordering::SeqCst), 1);
        }
    }
}
