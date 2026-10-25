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
    _code: crate::code_owner::CodeRoot,
}

// SAFETY: source admission proves Send for every captured value; polling is exclusive.
unsafe impl Send for GeneratorStream {}

impl GeneratorStream {
    pub(super) unsafe fn poll(
        &mut self,
        parent: *mut HewCoroState,
        layout: &HewValueLayout,
    ) -> (i32, Option<Item>) {
        let parent = if parent.is_null() {
            self.polling
        } else {
            parent
        };
        // SAFETY: the stream owns its generator, and the caller retains its optional parent.
        let status =
            unsafe { generator_checked::hew_checked_generator_poll(self.generator, parent, false) };
        if status == CoroStatus::Yielded as i32 {
            let allocation = std::alloc::Layout::from_size_align(layout.size.max(1), layout.align)
                .unwrap_or_else(|_| std::process::abort());
            // SAFETY: the exact checked item layout determines this temporary transfer slot.
            let slot = unsafe { std::alloc::alloc(allocation) }.cast::<c_void>();
            if slot.is_null() {
                std::alloc::handle_alloc_error(allocation);
            }
            // SAFETY: one published yield transfers into this exact slot, then into its envelope.
            let item = unsafe {
                generator_checked::hew_checked_generator_take(self.generator, slot);
                let item =
                    crate::channel_common::move_elem_envelope(slot, layout, "generator stream");
                std::alloc::dealloc(slot.cast(), allocation);
                item
            };
            return (1, Some(item));
        }
        if status == CoroStatus::Complete as i32 {
            self.done = true;
            return (2, None);
        }
        if status == CoroStatus::Pending as i32 {
            return (0, None);
        }
        (3, None)
    }
}

impl StreamBacking for GeneratorStream {
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
) -> *mut HewStream {
    let (_readiness, waker) = crate::wake::blocking::Readiness::new();
    // SAFETY: this new polling invocation owns a retained readiness target and independent token.
    let polling = unsafe { coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut()) };
    super::into_stream_ptr(GeneratorStream {
        // SAFETY: the caller retains this exact layout during construction.
        _code: unsafe { crate::code_owner::CodeRoot::acquire((*layout).code_owner) },
        generator,
        // SAFETY: the compiler provides the exact static generator descriptor.
        layout: unsafe { *layout },
        polling,
        released: false,
        done: false,
    })
}
