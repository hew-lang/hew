use core::ffi::c_void;
use core::ptr::NonNull;
use hew_cabi::value::HewValueLayout;
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};

#[derive(Debug)]
pub(crate) enum QueueItem {
    Content(Vec<u8>),
    Typed(Slot),
}

impl From<Vec<u8>> for QueueItem {
    fn from(value: Vec<u8>) -> Self {
        Self::Content(value)
    }
}

impl QueueItem {
    pub(crate) fn into_content(self) -> Vec<u8> {
        match self {
            Self::Content(bytes) => bytes,
            Self::Typed(_) => crate::channel_common::abort_elem_witness(
                "typed Pipe",
                "a raw typed slot reached a content-only queue operation",
            ),
        }
    }
}

/// Aligned storage for one ordinary typed value image. Slot ownership is
/// transferred explicitly by queue admission, read take and semantic cleanup;
/// dropping this carrier frees only its representation storage.
#[derive(Debug)]
pub(crate) struct Slot {
    pointer: NonNull<u8>,
    allocation: Layout,
    pub(crate) layout: HewValueLayout,
    _code: crate::code_owner::CodeRoot,
}

impl Slot {
    /// # Safety
    /// Input is an initialized value of this descriptor. The copied image
    /// borrows its owners until the caller commits the semantic transfer.
    pub(crate) unsafe fn copy_image(layout: HewValueLayout, input: *const c_void) -> Self {
        let allocation = Layout::from_size_align(layout.size.max(1), layout.align)
            .expect("verified Pipe element geometry");
        // SAFETY: the descriptor supplies a valid nonzero allocation geometry.
        let pointer = unsafe { alloc(allocation) };
        let pointer = NonNull::new(pointer).unwrap_or_else(|| handle_alloc_error(allocation));
        if layout.size != 0 {
            // SAFETY: input is live and the new aligned allocation is disjoint.
            unsafe {
                core::ptr::copy_nonoverlapping(input.cast::<u8>(), pointer.as_ptr(), layout.size);
            };
        }
        Self {
            pointer,
            allocation,
            layout,
            // SAFETY: the input pins its descriptor's defining code.
            _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
        }
    }

    pub(crate) fn as_ptr(&self) -> *mut c_void {
        self.pointer.as_ptr().cast()
    }
}

impl Drop for Slot {
    fn drop(&mut self) {
        // SAFETY: this carrier exclusively owns the exact aligned allocation.
        unsafe {
            dealloc(self.pointer.as_ptr(), self.allocation);
        };
    }
}

// SAFETY: a queued slot has one owner; access is serialized by its Pipe core.
unsafe impl Send for Slot {}
