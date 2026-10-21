//! Connection I/O ownership across admission, readers and deferred sends.

use std::ptr::NonNull;
use std::sync::Arc;

use crate::transport::HewTransport;

use super::SendTransport;

#[derive(Clone, Debug)]
pub(super) enum ConnectionTransport {
    Node(SendTransport),
    Owned(Arc<OwnedTransport>),
}

#[derive(Debug)]
pub(super) struct OwnedTransport {
    handle: NonNull<HewTransport>,
}

// SAFETY: the handle stays at one address and its fields are immutable during
// I/O. Its transport implementation synchronizes operations through its vtable.
unsafe impl Sync for OwnedTransport {}
// SAFETY: ownership transfers with the synchronized transport implementation.
unsafe impl Send for OwnedTransport {}

impl ConnectionTransport {
    pub(super) fn borrowed(handle: *mut HewTransport) -> Self {
        Self::Node(SendTransport(handle))
    }

    pub(super) fn owned(handle: HewTransport) -> Self {
        Self::Owned(Arc::new(OwnedTransport {
            handle: NonNull::from(Box::leak(Box::new(handle))),
        }))
    }

    pub(super) fn as_ptr(&self) -> *mut HewTransport {
        match self {
            Self::Node(handle) => handle.0,
            Self::Owned(owner) => owner.handle.as_ptr(),
        }
    }
}

impl Drop for OwnedTransport {
    fn drop(&mut self) {
        // SAFETY: this last owner recovers the allocation created above.
        let handle = unsafe { Box::from_raw(self.handle.as_ptr()) };
        // SAFETY: this is the last transport owner; all readers and claimed
        // senders have released their references before the implementation drops.
        unsafe {
            if let Some(destroy) = handle.ops.as_ref().and_then(|ops| ops.destroy) {
                destroy(handle.r#impl);
            }
        }
    }
}
