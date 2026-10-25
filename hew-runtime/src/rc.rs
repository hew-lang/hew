//! Reference-counted smart pointer (`Rc<T>`).
//!
//! Non-atomic reference counting for single-actor use. NOT `Send` — cannot
//! cross actor boundaries. Ordinary immutable values retain their checked
//! cross-actor value semantics.
//!
//! Layout: `[HewRcInner header | data bytes...]`
//! The returned pointer from `hew_rc_new` points to the **data region**
//! (immediately after the header). Strong-reference functions accept data
//! pointers; weak-reference functions accept header pointers.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::util::MutexExt;
use hew_cabi::value::{HewValueDropThunk, HewValueLayout};
use std::alloc::{alloc, dealloc, Layout};
use std::cell::Cell;
use std::ffi::{c_char, c_void};
use std::ptr;
use std::sync::{Arc, Mutex};

/// Header preceding the user data in an Rc allocation.
#[repr(C)]
#[derive(Debug)]
struct HewRcInner {
    strong: Cell<usize>,
    weak: Cell<usize>,
    layout: HewValueLayout,
    type_name: *const c_char,
    owner: Option<Arc<RcAllocations>>,
    owner_index: Cell<usize>,
    dying: bool,
    data_size: usize,
    data_align: usize,
    data_offset: usize,
}

/// Actor-owned membership only; the release walker owns payload destruction.
#[derive(Debug, Default)]
pub(crate) struct RcAllocations {
    inner: Mutex<Allocations>,
}

#[derive(Debug, Default)]
struct Allocations {
    blocks: Vec<*mut HewRcInner>,
    closed: bool,
}
// SAFETY: membership and slot indices are protected by `inner`; each payload
// and its non-atomic counts remain with their checked single-actor owner.
unsafe impl Send for Allocations {}

impl RcAllocations {
    pub(crate) fn is_empty(&self) -> bool {
        self.inner.lock_or_recover().blocks.is_empty()
    }

    pub(crate) fn close_empty(&self) -> bool {
        let mut allocations = self.inner.lock_or_recover();
        if !allocations.blocks.is_empty() {
            return false;
        }
        allocations.closed = true;
        true
    }

    unsafe fn register(self: &Arc<Self>, header: *mut HewRcInner) {
        let mut allocations = self.inner.lock_or_recover();
        assert!(
            !allocations.closed,
            "Rc allocation after actor cleanup quiesced"
        );
        // SAFETY: construction owns this fresh header; membership protects index.
        unsafe {
            (*header).owner_index.set(allocations.blocks.len());
            (*header).owner = Some(self.clone());
        }
        allocations.blocks.push(header);
    }

    /// Detach the first finite wave. Its cursor drains successive waves in bounded
    /// polls, marking every member before any authored payload cleanup.
    /// # Safety
    /// The actor terminal owner has drained ordinary turns and scoped tasks.
    pub(crate) unsafe fn sweep(
        self: &Arc<Self>,
    ) -> Option<*mut crate::release_walker::HewReleaseCursor> {
        let mut allocations = self.inner.lock_or_recover();
        if allocations.blocks.is_empty() {
            allocations.closed = true;
            return None;
        }
        let blocks = std::mem::take(&mut allocations.blocks);
        drop(allocations);
        Some(crate::release_walker::HewReleaseCursor::new(vec![
            crate::release_walker::ReleaseItem::RcSurvivors(Box::new(RcSurvivors {
                count: blocks.len() as u64,
                blocks,
                owner: self.clone(),
                waves_complete: false,
                marked: 0,
                released: 0,
                freed: 0,
                by_type: std::collections::BTreeMap::new(),
                summary: Vec::new(),
            })),
        ]))
    }
}

/// Retained terminal sweep. Preparation, release and header reclamation are
/// cursor steps; every wave is fully pinned and marked before payload cleanup.
#[derive(Debug)]
pub(crate) struct RcSurvivors {
    owner: Arc<RcAllocations>,
    waves_complete: bool,
    count: u64,
    blocks: Vec<*mut HewRcInner>,
    marked: usize,
    released: usize,
    freed: usize,
    by_type: std::collections::BTreeMap<String, u64>,
    summary: Vec<hew_observe_protocol::RcSurvivorType>,
}

impl RcSurvivors {
    /// # Safety
    /// The cursor owns this detached wave after actor/construction quiescence.
    pub(crate) unsafe fn step(
        mut self: Box<Self>,
        pending: &mut Vec<crate::release_walker::ReleaseItem>,
    ) -> Option<hew_observe_protocol::RcSweep> {
        use crate::release_walker::ReleaseItem;
        if let Some(&header) = self.blocks.get(self.marked) {
            // SAFETY: no ordinary turn or scoped child can access this wave.
            let inner = unsafe { &mut *header };
            assert!(inner.strong.get() != 0 && !inner.dying);
            // SAFETY: construction retains the immutable descriptor label through this wave.
            let type_name = unsafe { std::ffi::CStr::from_ptr(inner.type_name) }
                .to_string_lossy()
                .into_owned();
            *self.by_type.entry(type_name).or_default() += 1;
            inner.dying = true;
            inner.strong.set(0);
            inner.weak.set(
                inner
                    .weak
                    .get()
                    .checked_add(1)
                    .expect("Rc weak count overflow"),
            );
            inner.owner = None;
            self.marked += 1;
            pending.push(ReleaseItem::RcSurvivors(self));
        } else if let Some(&header) = self.blocks.get(self.released) {
            self.released += 1;
            // SAFETY: all headers remain pinned until every payload closes.
            let value = unsafe {
                ReleaseItem::Value {
                    slot: header.cast::<u8>().add((*header).data_offset).cast(),
                    layout: (*header).layout,
                }
            };
            pending.push(ReleaseItem::RcSurvivors(self));
            pending.push(value);
        } else if let Some(&header) = self.blocks.get(self.freed) {
            self.freed += 1;
            // SAFETY: all payloads have completed; this consumes one reserved pin.
            unsafe { release_pin(header.cast()) };
            pending.push(ReleaseItem::RcSurvivors(self));
        } else if !self.waves_complete {
            let mut allocations = self.owner.inner.lock_or_recover();
            self.blocks = std::mem::take(&mut allocations.blocks);
            if self.blocks.is_empty() {
                allocations.closed = true;
                self.waves_complete = true;
            }
            drop(allocations);
            self.count += self.blocks.len() as u64;
            self.marked = 0;
            self.released = 0;
            self.freed = 0;
            pending.push(ReleaseItem::RcSurvivors(self));
        } else if let Some((type_name, count)) = self.by_type.pop_first() {
            self.summary
                .push(hew_observe_protocol::RcSurvivorType { type_name, count });
            pending.push(ReleaseItem::RcSurvivors(self));
        } else {
            return Some(hew_observe_protocol::RcSweep {
                count: self.count,
                by_type: self.summary,
            });
        }
        None
    }
}

pub(crate) fn current_allocations() -> Option<Arc<RcAllocations>> {
    let actor = crate::actor::hew_actor_self();
    if !actor.is_null() {
        // SAFETY: the canonical dispatch context pins its actor incarnation.
        return unsafe { &*actor }
            .native_completion
            .as_ref()
            .map(|completion| completion.cleanup.rc.clone());
    }
    // Independently scheduled scoped tasks retain their creator attribution.
    let owner = crate::fault::owning_actor_at_spawn();
    let pin = crate::lifetime::live_actors::pin_actor_by_id(owner)?;
    pin.actor()
        .native_completion
        .as_ref()
        .map(|completion| completion.cleanup.rc.clone())
}

unsafe fn unlink(header: *mut HewRcInner) {
    // SAFETY: one final strong release owns this header. The membership lock
    // serializes swap-removal against unrelated scoped-task allocations.
    let Some(owner) = (unsafe { (*header).owner.take() }) else {
        return;
    };
    let mut allocations = owner.inner.lock_or_recover();
    // SAFETY: membership retains the header, and its index is locked here.
    let index = unsafe { (*header).owner_index.get() };
    assert_eq!(allocations.blocks.swap_remove(index), header);
    if let Some(&moved) = allocations.blocks.get(index) {
        // SAFETY: this index is read and written only under its owner's lock.
        unsafe { (*moved).owner_index.set(index) };
    }
}

unsafe fn release_pin(header: *mut c_void) {
    // SAFETY: this consumes one temporary weak owner retained before cleanup.
    unsafe { hew_weak_drop_rc(header.cast()) };
}

/// Take the final payload once and pin its header across opaque callbacks.
unsafe fn release_strong(value: *mut u8) -> Option<(*mut HewRcInner, HewValueLayout)> {
    if value.is_null() {
        return None;
    }
    // SAFETY: the caller transfers a valid live strong handle.
    let header = unsafe { header_from_data(value) };
    // SAFETY: that strong owner pins the header during this count transition.
    let inner = unsafe { &*header };
    if inner.dying {
        return None;
    }
    assert!(inner.strong.get() != 0, "Rc strong owner already released");
    inner.strong.set(inner.strong.get() - 1);
    if inner.strong.get() != 0 {
        return None;
    }
    inner.weak.set(
        inner
            .weak
            .get()
            .checked_add(1)
            .expect("Rc weak count overflow"),
    );
    let layout = inner.layout;
    // SAFETY: final release owns removal and already reserved the weak pin.
    unsafe { unlink(header) };
    Some((header, layout))
}

/// Recover header pointer from data pointer.
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_rc_new`].
unsafe fn header_from_data(data_ptr: *mut u8) -> *mut HewRcInner {
    // SAFETY: hew_rc_new stores the offset in the usize immediately preceding
    // the returned data pointer.
    let offset = unsafe {
        data_ptr
            .sub(size_of::<usize>())
            .cast::<usize>()
            .read_unaligned()
    };
    // SAFETY: the stored offset points back to the start of the allocation.
    unsafe { data_ptr.sub(offset) }.cast()
}

/// Normalise the caller-supplied payload alignment into a valid `Layout`
/// alignment (a non-zero power of two).
///
/// Codegen knows the payload type's ABI alignment and threads it in via
/// `hew_rc_new`'s payload descriptor. A value of `0` means "no explicit
/// alignment" (e.g. a zero-sized payload); we fall back to `1`, which is
/// the natural alignment of an empty allocation. A non-power-of-two value
/// would be a codegen bug, so we round up to the next power of two rather
/// than under-aligning the allocation.
fn normalise_align(align: usize) -> usize {
    if align <= 1 {
        1
    } else {
        align.next_power_of_two()
    }
}

/// Compute allocation layout for header + offset slot + aligned data.
/// Returns `None` on overflow.
fn alloc_layout(data_size: usize, data_align: usize) -> Option<(Layout, usize)> {
    let (prefix, _) = Layout::new::<HewRcInner>()
        .extend(Layout::new::<usize>())
        .ok()?;
    let data = Layout::from_size_align(data_size, data_align).ok()?;
    let (layout, data_offset) = prefix.extend(data).ok()?;
    Some((layout.pad_to_align(), data_offset))
}

// ── Public C ABI ───────────────────────────────────────────────────────

/// Create a new `Rc<T>`. Copies `layout.size` bytes from `data` into a
/// heap-allocated block with a reference count header. Returns a pointer
/// to the data region (caller uses this as the Rc handle).
///
/// The exact payload descriptor supplies alignment and synchronous/resumable
/// release. The allocation is attributed to the checked invocation's creator.
/// Returns null if the layout computation overflows.
///
/// # Safety
///
/// - `data` is valid for `layout.size` bytes (or null for an empty payload).
/// - `layout` is the exact immutable payload descriptor and its callbacks remain
///   live until all payloads and weak headers have been released.
/// - `type_name` is an immutable NUL-terminated payload name with that lifetime.
/// - `invocation` is null or a live checked invocation through this call.
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "ptr was allocated with align_of::<HewRcInner>() via alloc_layout"
)]
pub unsafe extern "C" fn hew_rc_new(
    data: *const u8,
    layout: *const HewValueLayout,
    type_name: *const c_char,
    invocation: *const crate::coro_state::HewCoroState,
) -> *mut u8 {
    // SAFETY: codegen supplies an immutable exact payload descriptor.
    let value_layout = unsafe { *layout };
    let size = value_layout.size;
    let align = value_layout.align;
    let data_align = normalise_align(align);
    let Some((layout, data_offset)) = alloc_layout(size, data_align) else {
        return ptr::null_mut();
    };
    // SAFETY: layout is valid (non-zero size due to header).
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        std::alloc::handle_alloc_error(layout);
    }

    // SAFETY: ptr was allocated with align_of::<HewRcInner>(), so the cast is aligned.
    let header = ptr.cast::<HewRcInner>();
    // SAFETY: ptr is freshly allocated with correct alignment.
    unsafe {
        header.write(HewRcInner {
            strong: Cell::new(1),
            weak: Cell::new(0),
            layout: value_layout,
            type_name,
            owner: None,
            owner_index: Cell::new(0),
            dying: false,
            data_size: size,
            data_align,
            data_offset,
        });
    }

    // SAFETY: the offset slot lies within the allocated prefix and records how
    // to recover the header from the returned data pointer.
    unsafe {
        ptr.add(data_offset - size_of::<usize>())
            .cast::<usize>()
            .write_unaligned(data_offset);
    }

    // SAFETY: data_offset was computed by Layout::extend for this allocation.
    let data_ptr = unsafe { ptr.add(data_offset) };
    if !data.is_null() && size > 0 {
        // SAFETY: data is valid for size bytes, data_ptr is valid for size.
        unsafe { ptr::copy_nonoverlapping(data, data_ptr, size) };
    }
    // SAFETY: checked code supplies its live invocation; runtime controls may
    // pass null and use the canonical activation/creator attribution.
    let owner = unsafe { invocation.as_ref() }.map_or_else(current_allocations, |state| {
        state.execution().rc_owner.clone()
    });
    if let Some(owner) = owner {
        // SAFETY: construction has not published this fresh allocation.
        unsafe { owner.register(header) };
    }
    #[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
    {
        // SAFETY: the caller retains the immutable label until final header free.
        let label = unsafe { std::ffi::CStr::from_ptr(type_name) }.to_string_lossy();
        crate::observe::record_rc_allocation(&label);
    }
    data_ptr
}

/// Return an owned ordinary fault for access to terminally consumed storage.
///
/// # Safety
/// `value` is null or a live Rc handle, including a pinned dying survivor.
#[no_mangle]
pub unsafe extern "C" fn hew_rc_check(value: *mut u8) -> *mut crate::fault::HewFault {
    // SAFETY: checked handles retain their header throughout each access.
    if value.is_null() || unsafe { (*header_from_data(value)).dying } {
        Box::into_raw(Box::new(crate::fault::HewFault::with_message(
            crate::internal::types::HEW_TRAP_USER_PANIC,
            "Rc payload is being released".into(),
        )))
    } else {
        ptr::null_mut()
    }
}

/// Increment the strong reference count. Returns the same data pointer.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_clone(ptr: *mut u8) -> *mut u8 {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees ptr is a valid Rc data pointer.
    let header = unsafe { header_from_data(ptr) };
    // A descriptor copy cannot resurrect a dying payload. Its empty handle
    // remains safely releasable, and every authored access checks it first.
    // SAFETY: the input handle pins its header through this descriptor copy.
    if unsafe { (*header).dying } {
        return ptr::null_mut();
    }
    // SAFETY: header is valid and remains live under its strong owner.
    unsafe { (*header).strong.set((*header).strong.get() + 1) };
    ptr
}

/// Decrement the strong reference count. If it reaches zero, calls the
/// drop function (if any) on the data region, then frees the allocation
/// if the weak count is also zero.
///
/// # Panics
///
/// Panics if the layout computation overflows (should never happen since
/// the layout was valid at construction time).
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
/// Must not be used after this call (use-after-free).
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_rc_drop(ptr: *mut u8) {
    struct Pin(*mut HewRcInner);
    impl Drop for Pin {
        fn drop(&mut self) {
            // SAFETY: final release reserved this exact weak pin.
            unsafe { release_pin(self.0.cast()) };
        }
    }
    // SAFETY: the caller transfers one strong owner.
    let Some((header, layout)) = (unsafe { release_strong(ptr) }) else {
        return;
    };
    let _pin = Pin(header);
    // A synchronous descriptor never substitutes for a resumable close.
    assert!(
        layout.release_start.is_none(),
        "Rc payload needs resumable release"
    );
    if let Some(drop) = layout.drop_fn {
        // SAFETY: the pin retains storage, with no header borrow across cleanup.
        unsafe { drop(ptr.cast()) };
    }
}

/// Release one strong reference, transferring the final payload to a cursor.
/// A temporary weak reference pins the header while the payload close suspends.
/// # Safety
/// `value` is null or one live strong owner. `layout` is its exact immutable
/// payload descriptor and remains valid through the consuming traversal.
/// # Panics
/// Panics if the supplied strong reference was already released.
#[no_mangle]
pub unsafe extern "C" fn hew_rc_release_begin(
    value: *mut u8,
    layout: *const hew_cabi::value::HewValueLayout,
) -> *mut crate::release_walker::HewReleaseCursor {
    use crate::release_walker::{HewReleaseCursor, ReleaseItem};
    let mut pending = Vec::new();
    // SAFETY: the caller transfers one strong owner and its payload descriptor.
    if let Some((header, stored)) = unsafe { release_strong(value) } {
        // SAFETY: the retained header stores the construction descriptor.
        assert_eq!((stored.size, stored.align), unsafe {
            ((*layout).size, (*layout).align)
        });
        pending.push(ReleaseItem::Storage {
            owner: header.cast(),
            free: release_pin,
        });
        if stored.drop_fn.is_some() || stored.release_start.is_some() {
            pending.push(ReleaseItem::Value {
                slot: value.cast(),
                layout: stored,
            });
        }
    }
    HewReleaseCursor::new(pending)
}

/// Get the current strong reference count.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_strong_count(ptr: *mut u8) -> usize {
    if ptr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).strong.get() }
}

/// Get the current weak reference count.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_weak_count(ptr: *mut u8) -> usize {
    if ptr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).weak.get() }
}

/// Get the data pointer from an `Rc`. In the current layout the handle
/// itself *is* the data pointer, so this is an identity function — but
/// callers should use it for API clarity.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_get(ptr: *mut u8) -> *mut u8 {
    ptr
}

/// Replace the payload shared by every strong alias.
///
/// The replacement is installed before the displaced payload is dropped. If
/// that drop recursively replaces this allocation, the recursive replacement
/// therefore wins.
///
/// # Panics
///
/// Panics when either pointer is null, the Rc has no live strong owner, the
/// replacement is misaligned, either pointer range overflows, or the payload
/// and replacement ranges overlap.
///
/// # Safety
///
/// - `ptr` must be a live data pointer returned by [`hew_rc_new`] or
///   [`hew_rc_clone`].
/// - `replacement` must point to an initialized value with the exact size and
///   alignment used to construct `ptr`.
/// - The payload and replacement ranges must not overlap.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_rc_set(ptr: *mut u8, replacement: *mut u8) {
    // SAFETY: the swap validates both slots before transferring their owners.
    if let Some(drop_fn) = unsafe { swap_payload(ptr, replacement) } {
        // SAFETY: replacement now owns the displaced payload, independently of Rc.
        unsafe { drop_fn(replacement.cast()) };
    }
}

/// Replace the shared payload and return its displaced owner for checked release.
/// # Safety
/// `ptr` and `replacement` satisfy `hew_rc_set`; `layout` exactly describes them.
/// The replacement slot remains live until the returned cursor is consumed.
/// # Panics
/// Panics when either slot or its supplied layout violates that contract.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_rc_set_release(
    ptr: *mut u8,
    replacement: *mut u8,
    layout: *const hew_cabi::value::HewValueLayout,
) -> *mut crate::release_walker::HewReleaseCursor {
    assert!(!ptr.is_null() && !layout.is_null());
    // SAFETY: the caller supplies a live Rc header and exact immutable descriptor.
    unsafe {
        let inner = &*header_from_data(ptr);
        assert_eq!(inner.data_size, (*layout).size);
        assert_eq!(inner.data_align, (*layout).align);
        swap_payload(ptr, replacement);
        crate::release_walker::HewReleaseCursor::values([(replacement.cast(), *layout)])
    }
}

unsafe fn swap_payload(ptr: *mut u8, replacement: *mut u8) -> Option<HewValueDropThunk> {
    assert!(!ptr.is_null(), "Rc.set requires a non-null Rc data pointer");
    assert!(
        !replacement.is_null(),
        "Rc.set requires a non-null replacement pointer"
    );

    // Snapshot every value needed after the swap. No reference into the
    // allocation may remain live while the opaque payload callback runs.
    let (data_size, data_align, drop_fn) = {
        // SAFETY: caller guarantees ptr is a live Rc data pointer.
        let header = unsafe { header_from_data(ptr) };
        // SAFETY: the shared header remains live while strong > 0. This
        // immutable borrow ends at the block boundary before the callback.
        let inner = unsafe { &*header };
        assert!(
            inner.strong.get() > 0,
            "Rc.set requires a live strong reference"
        );
        (inner.data_size, inner.data_align, inner.layout.drop_fn)
    };

    assert_eq!(
        replacement.addr() % data_align,
        0,
        "Rc.set replacement is not aligned for the payload"
    );
    let payload_start = ptr.addr();
    let replacement_start = replacement.addr();
    let payload_end = payload_start
        .checked_add(data_size)
        .expect("Rc.set payload range overflow");
    let replacement_end = replacement_start
        .checked_add(data_size)
        .expect("Rc.set replacement range overflow");
    assert!(
        data_size == 0 || payload_end <= replacement_start || replacement_end <= payload_start,
        "Rc.set payload and replacement ranges overlap"
    );

    // SAFETY: the caller supplies two initialized, aligned, non-overlapping
    // regions of data_size bytes. The allocation receives the replacement and
    // the caller slot receives the displaced payload.
    unsafe { ptr::swap_nonoverlapping(ptr, replacement, data_size) };

    drop_fn
}

/// Returns 1 if this `Rc` is the only strong reference (refcount == 1),
/// 0 otherwise.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_is_unique(ptr: *mut u8) -> i32 {
    // SAFETY: forwarded to hew_rc_strong_count with same preconditions.
    i32::from(unsafe { hew_rc_strong_count(ptr) } == 1)
}

/// Create a `Weak` reference from an `Rc` data pointer. Increments the
/// weak count. Returns a pointer to the *header* (not the data) — weak
/// refs need the header to check if the strong count is still > 0.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_downgrade(ptr: *mut u8) -> *mut u8 {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).weak.set((*header).weak.get() + 1) };
    header.cast()
}

/// Clone a `Weak` reference. Returns the same header pointer.
///
/// # Panics
///
/// Panics when a non-null pointer does not represent a live explicit weak
/// handle.
///
/// # Safety
///
/// `weak_ptr` must have been returned by [`hew_rc_downgrade`] or this function
/// and must still represent a live explicit weak handle.
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "weak_ptr is a header pointer allocated with align_of::<HewRcInner>()"
)]
pub unsafe extern "C" fn hew_weak_clone_rc(weak_ptr: *mut u8) -> *mut u8 {
    if weak_ptr.is_null() {
        return ptr::null_mut();
    }
    let header = weak_ptr.cast::<HewRcInner>();
    // SAFETY: caller guarantees weak_ptr is a live Rc header pointer.
    let inner = unsafe { &*header };
    assert!(
        inner.weak.get() > 0,
        "Weak<Rc> clone requires a live weak handle"
    );
    inner.weak.set(inner.weak.get() + 1);
    weak_ptr
}

/// Attempt to upgrade a `Weak` reference back to a strong `Rc`.
/// Returns the data pointer if the value is still alive (strong > 0),
/// or null if the value has been dropped.
///
/// # Safety
///
/// `weak_ptr` must have been returned by [`hew_rc_downgrade`].
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "weak_ptr is a header pointer allocated with align_of::<HewRcInner>()"
)]
pub unsafe extern "C" fn hew_weak_upgrade_rc(weak_ptr: *mut u8) -> *mut u8 {
    if weak_ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: weak_ptr was returned by hew_rc_downgrade which returns a header pointer
    // allocated with align_of::<HewRcInner>().
    let header = weak_ptr.cast::<HewRcInner>();
    // SAFETY: weak_ptr is a header pointer.
    let inner = unsafe { &*header };

    if inner.strong.get() == 0 {
        return ptr::null_mut();
    }

    inner.strong.set(inner.strong.get() + 1);
    // SAFETY: data pointer is immediately after header, within the allocation.
    unsafe { weak_ptr.add(inner.data_offset) }
}

/// Drop a `Weak` reference. Decrements the weak count. If both strong
/// and weak counts reach zero, frees the allocation.
///
/// # Panics
///
/// Panics if the layout computation overflows (should never happen since
/// the layout was valid at construction time).
///
/// # Safety
///
/// `weak_ptr` must have been returned by [`hew_rc_downgrade`].
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "weak_ptr is a header pointer allocated with align_of::<HewRcInner>()"
)]
pub unsafe extern "C" fn hew_weak_drop_rc(weak_ptr: *mut u8) {
    if weak_ptr.is_null() {
        return;
    }
    // SAFETY: weak_ptr was returned by hew_rc_downgrade which returns a header pointer
    // allocated with align_of::<HewRcInner>().
    let header = weak_ptr.cast::<HewRcInner>();
    // SAFETY: weak_ptr is a header pointer.
    let inner = unsafe { &*header };

    // Same rationale as the strong-count assert in hew_rc_drop: a zero weak
    // count before decrement is a double-free and must abort in release.
    assert!(
        inner.weak.get() > 0,
        "Weak<Rc> double-free: weak refcount is already 0"
    );
    inner.weak.set(inner.weak.get() - 1);

    if inner.weak.get() == 0 && inner.strong.get() == 0 {
        // SAFETY: data_size was validated at construction time.
        let (layout, _) = alloc_layout(inner.data_size, inner.data_align)
            .expect("layout was valid at construction");
        #[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
        {
            // SAFETY: the immutable label is retained through this final free.
            let label = unsafe { std::ffi::CStr::from_ptr(inner.type_name) }.to_string_lossy();
            crate::observe::record_rc_free(&label);
        }
        // SAFETY: header was allocated with this layout.
        unsafe { dealloc(header.cast(), layout) };
    }
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "test data is allocated with proper alignment by hew_rc_new"
)]
mod tests {
    unsafe fn new_for_test(
        data: *const u8,
        size: usize,
        align: usize,
        drop_fn: Option<unsafe extern "C" fn(*mut u8)>,
    ) -> *mut u8 {
        let layout = HewValueLayout {
            code_owner: core::ptr::null(),
            size,
            align,
            ownership_kind: hew_cabi::value::HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            // SAFETY: a non-unwinding C callback also satisfies C-unwind.
            drop_fn: unsafe {
                std::mem::transmute::<
                    Option<unsafe extern "C" fn(*mut u8)>,
                    Option<HewValueDropThunk>,
                >(drop_fn)
            },
            release_start: None,
        };
        // SAFETY: the test supplies matching payload storage and callback.
        unsafe { hew_rc_new(data, &raw const layout, c"test".as_ptr(), ptr::null()) }
    }

    use super::*;

    #[test]
    fn rc_basic_lifecycle() {
        // SAFETY: Test exercises the Rc FFI lifecycle with valid pointers.
        unsafe {
            let val: i32 = 42;
            let rc = new_for_test(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                None,
            );
            assert!(!rc.is_null());

            // Read value through Rc
            let read_val = rc.cast::<i32>().read();
            assert_eq!(read_val, 42);
            assert_eq!(hew_rc_strong_count(rc), 1);

            // Clone
            let rc2 = hew_rc_clone(rc);
            assert_eq!(rc2, rc); // same pointer
            assert_eq!(hew_rc_strong_count(rc), 2);

            // Drop one
            hew_rc_drop(rc2);
            assert_eq!(hew_rc_strong_count(rc), 1);

            // Drop last — frees
            hew_rc_drop(rc);
        }
    }

    #[test]
    fn rc_with_drop_fn() {
        use std::sync::atomic::{AtomicI32, Ordering};
        static DROP_COUNT: AtomicI32 = AtomicI32::new(0);

        unsafe extern "C" fn drop_counter(_data: *mut u8) {
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }

        // SAFETY: Test exercises Rc FFI with a custom drop function.
        unsafe {
            DROP_COUNT.store(0, Ordering::SeqCst);
            let val: i32 = 99;
            let rc = new_for_test(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                Some(drop_counter),
            );

            let rc2 = hew_rc_clone(rc);
            hew_rc_drop(rc2);
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 0); // not dropped yet

            hew_rc_drop(rc);
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 1); // dropped!
        }
    }

    #[test]
    fn rc_drop_fn_may_reach_allocation() {
        // FND-01 regression: the destructor is an opaque callback that may
        // read the very allocation being dropped. `hew_rc_drop` must NOT hold
        // a live `&mut *header` across `drop_fn(ptr)` — doing so is aliasing
        // UB that Miri (Stacked/Tree Borrows) flags. This drop_fn reads its
        // own data region AND reaches the header via the public FFI, so a live
        // mutable borrow across the call would be detected as a conflict.
        use std::sync::atomic::{AtomicI32, Ordering};
        static OBSERVED: AtomicI32 = AtomicI32::new(0);

        unsafe extern "C" fn drop_reads_self(data: *mut u8) {
            // SAFETY: `data` is this Rc's data region; it is still valid while
            // the destructor runs (dealloc happens strictly after drop_fn).
            let v = unsafe { data.cast::<i32>().read() };
            OBSERVED.store(v, Ordering::SeqCst);
            // Reach the header from the same allocation and WRITE to it. If
            // `hew_rc_drop` still held a live `&mut *header` across this call,
            // this aliasing write pops the Unique tag (Stacked Borrows) /
            // disables the mutable borrow (Tree Borrows), so the drop's own
            // post-call read of `weak`/`data_size` would be flagged as UB by
            // Miri. This is the exact window FND-01 closes.
            // SAFETY: `data` is a valid Rc data pointer for this allocation.
            let header = unsafe { header_from_data(data) };
            // SAFETY: this callback consumes the one explicit weak owner;
            // final strong release must retain its independent temporary pin.
            unsafe { hew_weak_drop_rc(header.cast()) };
            // SAFETY: header is valid; strong is 0 here (decremented before the
            // call), so the allocation is still live and writable. Bump weak up
            // and back so the value is preserved but the WRITE actually occurs.
            unsafe {
                (*header).weak.set((*header).weak.get() + 1);
                (*header).weak.set((*header).weak.get() - 1);
            }
        }

        // SAFETY: exercises Rc FFI with a destructor that reads the allocation.
        unsafe {
            OBSERVED.store(0, Ordering::SeqCst);
            let val: i32 = 1234;
            let rc = new_for_test(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                Some(drop_reads_self),
            );
            let weak = hew_rc_downgrade(rc);
            assert!(!weak.is_null());
            assert_eq!(hew_rc_weak_count(rc), 1);
            // The destructor consumes weak and records the payload (1234).
            hew_rc_drop(rc);
            assert_eq!(OBSERVED.load(Ordering::SeqCst), 1234);
        }
    }

    #[test]
    fn rc_weak_upgrade() {
        // SAFETY: Test exercises weak reference upgrade with valid Rc pointers.
        unsafe {
            let val: i32 = 77;
            let rc = new_for_test(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                None,
            );

            // Downgrade to weak
            let weak = hew_rc_downgrade(rc);
            assert!(!weak.is_null());
            assert_eq!(hew_rc_weak_count(rc), 1);
            assert_eq!(hew_rc_strong_count(rc), 1);

            // Upgrade while strong > 0
            let upgraded = hew_weak_upgrade_rc(weak);
            assert!(!upgraded.is_null());
            assert_eq!(upgraded.cast::<i32>().read(), 77);
            assert_eq!(hew_rc_strong_count(rc), 2);

            // Drop all strong refs
            hew_rc_drop(rc);
            hew_rc_drop(upgraded);

            // Upgrade fails — strong == 0
            let failed = hew_weak_upgrade_rc(weak);
            assert!(failed.is_null());

            // Drop weak — frees allocation
            hew_weak_drop_rc(weak);
        }
    }

    #[test]
    fn rc_weak_exact_release_ladder() {
        // SAFETY: every handle is released exactly once in the asserted order.
        unsafe {
            let value = 17_i32;
            let rc = new_for_test(
                (&raw const value).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                None,
            );
            assert_eq!((hew_rc_strong_count(rc), hew_rc_weak_count(rc)), (1, 0));
            let rc2 = hew_rc_clone(rc);
            assert_eq!((hew_rc_strong_count(rc), hew_rc_weak_count(rc)), (2, 0));
            let weak = hew_rc_downgrade(rc);
            assert_eq!((hew_rc_strong_count(rc), hew_rc_weak_count(rc)), (2, 1));
            let weak2 = hew_weak_clone_rc(weak);
            assert_eq!(weak2, weak);
            assert_eq!((hew_rc_strong_count(rc), hew_rc_weak_count(rc)), (2, 2));
            let rc3 = hew_weak_upgrade_rc(weak);
            assert_eq!((hew_rc_strong_count(rc), hew_rc_weak_count(rc)), (3, 2));
            hew_rc_drop(rc3);
            assert_eq!((hew_rc_strong_count(rc), hew_rc_weak_count(rc)), (2, 2));
            hew_rc_drop(rc2);
            assert_eq!((hew_rc_strong_count(rc), hew_rc_weak_count(rc)), (1, 2));
            hew_rc_drop(rc);

            let header = weak.cast::<HewRcInner>();
            assert_eq!(((*header).strong.get(), (*header).weak.get()), (0, 2));
            assert!(hew_weak_upgrade_rc(weak).is_null());
            hew_weak_drop_rc(weak2);
            assert_eq!(((*header).strong.get(), (*header).weak.get()), (0, 1));
            hew_weak_drop_rc(weak);
        }
    }

    #[test]
    fn rc_set_updates_every_alias_and_drops_displaced_payload() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static DROPS: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn count_drop(_data: *mut u8) {
            DROPS.fetch_add(1, Ordering::SeqCst);
        }

        // SAFETY: replacement has the allocation's exact type and alignment.
        unsafe {
            DROPS.store(0, Ordering::SeqCst);
            let value = 5_i64;
            let rc = new_for_test(
                (&raw const value).cast(),
                size_of::<i64>(),
                align_of::<i64>(),
                Some(count_drop),
            );
            let alias = hew_rc_clone(rc);
            let mut replacement = 8_i64;
            hew_rc_set(rc, (&raw mut replacement).cast());
            assert_eq!(alias.cast::<i64>().read(), 8);
            assert_eq!(replacement, 5);
            assert_eq!(DROPS.load(Ordering::SeqCst), 1);
            hew_rc_drop(alias);
            assert_eq!(DROPS.load(Ordering::SeqCst), 1);
            hew_rc_drop(rc);
            assert_eq!(DROPS.load(Ordering::SeqCst), 2);
        }
    }

    #[test]
    fn rc_set_recursive_replacement_wins() {
        use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

        #[repr(C)]
        struct Payload {
            value: usize,
        }

        static RC: AtomicPtr<u8> = AtomicPtr::new(ptr::null_mut());
        static DROP_SEQUENCE: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn recursive_drop(data: *mut u8) {
            // SAFETY: the callback receives an initialized Payload slot.
            let value = unsafe { (*data.cast::<Payload>()).value };
            DROP_SEQUENCE
                .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |sequence| {
                    Some(sequence * 10 + value)
                })
                .unwrap();
            if value == 1 {
                let mut nested = Payload { value: 3 };
                // SAFETY: RC is the live allocation currently executing set;
                // nested has the allocation's exact layout.
                unsafe { hew_rc_set(RC.load(Ordering::SeqCst), (&raw mut nested).cast()) };
            }
        }

        // SAFETY: all pointers and replacement slots use Payload's layout.
        unsafe {
            DROP_SEQUENCE.store(0, Ordering::SeqCst);
            let initial = Payload { value: 1 };
            let rc = new_for_test(
                (&raw const initial).cast(),
                size_of::<Payload>(),
                align_of::<Payload>(),
                Some(recursive_drop),
            );
            RC.store(rc, Ordering::SeqCst);
            let mut replacement = Payload { value: 2 };
            hew_rc_set(rc, (&raw mut replacement).cast());
            assert_eq!((*rc.cast::<Payload>()).value, 3);
            assert_eq!(DROP_SEQUENCE.load(Ordering::SeqCst), 12);
            hew_rc_drop(rc);
            assert_eq!(DROP_SEQUENCE.load(Ordering::SeqCst), 123);
            RC.store(ptr::null_mut(), Ordering::SeqCst);
        }
    }

    #[test]
    fn rc_set_drops_weak_back_edge_after_installing_replacement() {
        use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

        #[repr(C)]
        struct Payload {
            weak: *mut u8,
            value: usize,
        }

        static RC: AtomicPtr<u8> = AtomicPtr::new(ptr::null_mut());
        static OBSERVED: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn drop_payload(data: *mut u8) {
            // SAFETY: callback receives an initialized Payload.
            let payload = unsafe { &*data.cast::<Payload>() };
            if !payload.weak.is_null() {
                let rc = RC.load(Ordering::SeqCst);
                // The replacement must already be visible through the Rc.
                // SAFETY: the strong owner keeps rc and its payload live.
                OBSERVED.store(unsafe { (*rc.cast::<Payload>()).value }, Ordering::SeqCst);
                // SAFETY: the embedded pointer owns one explicit weak count.
                unsafe { hew_weak_drop_rc(payload.weak) };
            }
        }

        // SAFETY: all pointers and replacement slots use Payload's layout.
        unsafe {
            OBSERVED.store(0, Ordering::SeqCst);
            let initial = Payload {
                weak: ptr::null_mut(),
                value: 1,
            };
            let rc = new_for_test(
                (&raw const initial).cast(),
                size_of::<Payload>(),
                align_of::<Payload>(),
                Some(drop_payload),
            );
            RC.store(rc, Ordering::SeqCst);
            let retained_weak = hew_rc_downgrade(rc);
            let payload_weak = hew_weak_clone_rc(retained_weak);
            assert_eq!(hew_rc_weak_count(rc), 2);

            let mut with_back_edge = Payload {
                weak: payload_weak,
                value: 2,
            };
            hew_rc_set(rc, (&raw mut with_back_edge).cast());
            assert_eq!(hew_rc_weak_count(rc), 2);

            let mut without_back_edge = Payload {
                weak: ptr::null_mut(),
                value: 3,
            };
            hew_rc_set(rc, (&raw mut without_back_edge).cast());
            assert_eq!(OBSERVED.load(Ordering::SeqCst), 3);
            assert_eq!(hew_rc_weak_count(rc), 1);

            hew_rc_drop(rc);
            assert!(hew_weak_upgrade_rc(retained_weak).is_null());
            hew_weak_drop_rc(retained_weak);
            RC.store(ptr::null_mut(), Ordering::SeqCst);
        }
    }

    #[test]
    fn rc_set_zero_sized_payload() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static DROPS: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn count_drop(_data: *mut u8) {
            DROPS.fetch_add(1, Ordering::SeqCst);
        }

        // SAFETY: the non-null replacement is a correctly aligned ZST slot.
        unsafe {
            DROPS.store(0, Ordering::SeqCst);
            let initial = ();
            let rc = new_for_test(
                (&raw const initial).cast(),
                0,
                align_of::<()>(),
                Some(count_drop),
            );
            let mut replacement = ();
            hew_rc_set(rc, (&raw mut replacement).cast());
            assert_eq!(DROPS.load(Ordering::SeqCst), 1);
            hew_rc_drop(rc);
            assert_eq!(DROPS.load(Ordering::SeqCst), 2);
        }
    }

    #[test]
    fn rc_honours_overaligned_data_align() {
        // A 64-byte cache-line-aligned payload. Its true alignment (64)
        // exceeds align_of::<u128>() (16) — the bound the old trailing-zeros
        // heuristic capped at — and is NOT recoverable from the byte size.
        // The authoritative alignment must come from the caller.
        #[repr(align(64))]
        struct Over {
            _x: [u8; 64],
        }
        assert!(align_of::<Over>() > size_of::<u128>());

        // SAFETY: hew_rc_new copies from a valid Over pointer and returns an
        // Rc allocation that is dropped before the test exits.
        unsafe {
            let value = Over { _x: [1; 64] };
            let rc = new_for_test(
                (&raw const value).cast(),
                size_of::<Over>(),
                align_of::<Over>(),
                None,
            );
            assert!(!rc.is_null());
            let header = header_from_data(rc);
            // The header records the caller's authoritative alignment, and the
            // returned data pointer actually satisfies it. Under the old
            // heuristic data_align would have been capped at 16 and this
            // allocation could land on a 16-aligned (not 64-aligned) address.
            assert_eq!((*header).data_align, align_of::<Over>());
            assert_eq!(rc as usize % align_of::<Over>(), 0);
            hew_rc_drop(rc);
        }
    }

    #[test]
    fn rc_normalises_zero_and_non_power_of_two_align() {
        assert_eq!(normalise_align(0), 1);
        assert_eq!(normalise_align(1), 1);
        assert_eq!(normalise_align(8), 8);
        assert_eq!(normalise_align(3), 4);
        assert_eq!(normalise_align(48), 64);
    }
}
