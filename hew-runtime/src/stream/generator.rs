//! A stream retains the ordinary checked generator and its polling invocation.

use super::{HewStream, Item, StreamBacking};
use crate::coro_state::{self, CoroStatus, HewCoroState};
use crate::generator_checked::{self, HewCheckedGenerator};
use crate::vec::HewValueLayout;
use std::ffi::c_void;
use std::ptr;

#[derive(Debug)]
pub(super) struct GeneratorStream {
    pub(super) generator: *mut HewCheckedGenerator,
    layout: HewValueLayout,
    polling: *mut HewCoroState,
    released: bool,
    done: bool,
    yield_ready: bool,
    owner: Option<crate::owned_value_budget::ValueOwner>,
    _code: crate::code_owner::CodeRoot,
}

// SAFETY: source admission proves Send for every captured value; polling is exclusive.
unsafe impl Send for GeneratorStream {}

impl GeneratorStream {
    pub(super) unsafe fn poll_native(&mut self, parent: *mut HewCoroState) -> i32 {
        if self.yield_ready {
            return 1;
        }
        let parent = if parent.is_null() {
            self.polling
        } else {
            parent
        };
        // SAFETY: the unique Stream loan retains the generator and polling parent.
        let status =
            unsafe { generator_checked::hew_checked_generator_poll(self.generator, parent, false) };
        if status == CoroStatus::Yielded as i32 {
            self.yield_ready = true;
            1
        } else if status == CoroStatus::Complete as i32 {
            self.done = true;
            2
        } else if status == CoroStatus::Pending as i32 {
            0
        } else {
            3
        }
    }

    pub(super) unsafe fn take_native(&mut self, out: *mut c_void) -> bool {
        if !self.yield_ready {
            return false;
        }
        // SAFETY: the published generator output moves directly into its caller's exact typed slot.
        unsafe { generator_checked::hew_checked_generator_take(self.generator, out) };
        self.yield_ready = false;
        true
    }

    pub(super) fn owner(&self) -> Option<crate::owned_value_budget::ValueOwner> {
        self.owner.clone()
    }

    pub(super) unsafe fn visit(
        &mut self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        use hew_cabi::managed::{visit_value, HewManagedWalkKind, DESCEND, SKIP_DESCENDANTS};
        // SAFETY: this provider retains its immutable compiler descriptor and stable Box.
        unsafe {
            let bytes = match crate::owned_value_budget::visit::slot_bytes(&self.layout) {
                Ok(bytes) => bytes,
                Err(status) => return status,
            };
            match crate::owned_value_budget::visit::storage(
                visitor,
                std::ptr::from_ref(self).cast(),
                bytes,
            ) {
                DESCEND => {}
                SKIP_DESCENDANTS => return 0,
                failure => return failure,
            }
            let status = visit_value(&self.layout, (&raw const self.generator).cast(), visitor);
            if status == 0 && (*visitor).kind == HewManagedWalkKind::CommitTransfer {
                self.owner = crate::owned_value_budget::transfer::destination_owner(visitor);
            }
            status
        }
    }

    pub(super) unsafe fn expand_release(
        &mut self,
        pending: &mut Vec<crate::release_walker::ReleaseItem>,
    ) {
        use crate::release_walker::ReleaseItem;
        unsafe fn keep_provider(_pointer: *mut c_void) {}
        // SAFETY: the cursor pins this provider and its descriptor until all children drain.
        let bytes = unsafe { crate::owned_value_budget::visit::slot_bytes(&self.layout) }
            .expect("checked generator stream descriptor");
        pending.push(ReleaseItem::ManagedBacking {
            pointer: std::ptr::from_mut(self).cast(),
            bytes,
            free: keep_provider,
        });
        pending.extend(
            self.owned_values()
                .into_iter()
                .map(|(slot, layout)| ReleaseItem::Value { slot, layout }),
        );
    }

    pub(super) unsafe fn poll(
        &mut self,
        parent: *mut HewCoroState,
        layout: &HewValueLayout,
    ) -> (i32, Option<Item>) {
        // SAFETY: this foreign content helper holds the unique Stream loan.
        let status = unsafe { self.poll_native(parent) };
        if status == 1 {
            let allocation = std::alloc::Layout::from_size_align(layout.size.max(1), layout.align)
                .unwrap_or_else(|_| std::process::abort());
            // SAFETY: the exact checked item layout determines this temporary transfer slot.
            let slot = unsafe { std::alloc::alloc(allocation) }.cast::<c_void>();
            if slot.is_null() {
                std::alloc::handle_alloc_error(allocation);
            }
            // SAFETY: one published yield transfers into this exact slot, then into its envelope.
            let item = unsafe {
                self.take_native(slot);
                let item =
                    crate::channel_common::move_elem_envelope(slot, layout, "generator stream");
                std::alloc::dealloc(slot.cast(), allocation);
                item
            };
            return (1, Some(item));
        }
        (status, None)
    }
}

impl StreamBacking for GeneratorStream {
    fn select_readiness(&self) -> super::SelectReadiness {
        super::SelectReadiness::Generator
    }

    fn generator(&mut self) -> Option<&mut GeneratorStream> {
        Some(self)
    }

    fn owned_values(&mut self) -> Vec<(*mut c_void, HewValueLayout)> {
        self.released = true;
        vec![((&raw mut self.generator).cast(), self.layout)]
    }

    fn next(&mut self) -> Option<Item> {
        crate::channel_common::abort_elem_witness(
            "generator stream",
            "receives require the checked poll path",
        )
    }

    fn try_next(&mut self, layout: &HewValueLayout) -> Option<Item> {
        // SAFETY: this exclusive stream retains its polling state across single polls.
        let (status, item) = unsafe { self.poll(ptr::null_mut(), layout) };
        if status == 3 {
            std::process::abort();
        }
        item
    }

    fn close(&mut self) {
        if !self.released {
            std::process::abort();
        }
    }

    fn is_closed(&self) -> bool {
        self.done
    }
}

impl Drop for GeneratorStream {
    fn drop(&mut self) {
        if !self.released {
            std::process::abort();
        }
        // SAFETY: the value cursor finished the generator before freeing this parent.
        unsafe { coro_state::hew_coro_state_free(self.polling) };
    }
}

/// Own an ordinary generator as the backing of a stream, without starting it.
/// # Safety
/// The generator transfers once; its exact immutable value descriptor outlives the stream.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_from_generator(
    generator: *mut HewCheckedGenerator,
    layout: *const HewValueLayout,
    out_status: *mut i32,
) -> *mut HewStream {
    // SAFETY: the compiler supplies writable status and a retained concrete descriptor.
    unsafe { out_status.write(0) };
    // SAFETY: the generator descriptor lives throughout this construction.
    let width = match unsafe { crate::owned_value_budget::visit::slot_bytes(&*layout) } {
        Ok(width) => width,
        Err(status) => {
            // SAFETY: status belongs to the caller throughout this call.
            unsafe { out_status.write(status) };
            return ptr::null_mut();
        }
    };
    let owner = crate::owned_value_budget::current_owner();
    let Ok(reservation) = owner
        .as_ref()
        .map(|owner| owner.reserve_new(width))
        .transpose()
    else {
        // SAFETY: refusal leaves the original generator untouched for authored cleanup.
        unsafe { out_status.write(hew_cabi::managed::HEAP_EXCEEDED) };
        return ptr::null_mut();
    };
    let (_readiness, waker) = crate::wake::blocking::Readiness::new();
    // SAFETY: this new polling invocation owns a retained readiness target and independent token.
    let polling = unsafe { coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut()) };
    let backing = Box::new(GeneratorStream {
        // SAFETY: the caller retains this exact layout during construction.
        _code: unsafe { crate::code_owner::CodeRoot::acquire((*layout).code_owner) },
        generator,
        // SAFETY: the compiler provides the exact static generator descriptor.
        layout: unsafe { *layout },
        polling,
        released: false,
        done: false,
        yield_ready: false,
        owner,
    });
    if let Some(reservation) = reservation {
        reservation.commit(&[crate::owned_value_budget::ManagedStorage {
            identity: std::ptr::from_ref(&*backing) as usize,
            bytes: width,
        }]);
    }
    super::into_boxed_stream_ptr(backing)
}
