//! Immutable write content carries its account until its final producer releases it.

use std::sync::{Arc, Mutex};

use crate::owned_value_budget::{ManagedStorage, ValueOwner};
use crate::util::MutexExt;

pub(super) struct ReadOwnership {
    owner: Option<ValueOwner>,
    frozen: usize,
    publishing: bool,
    waiter: Option<crate::wake::OwnedWaker>,
}

impl ReadOwnership {
    pub(super) fn new(owner: Option<ValueOwner>) -> Self {
        Self {
            owner,
            frozen: 0,
            publishing: false,
            waiter: None,
        }
    }
}

impl super::HewAsyncIo {
    pub(super) fn prepare_content_publication(
        &self,
        result: Result<super::IoValue, super::IoFailure>,
    ) -> (Option<Result<super::IoValue, super::IoFailure>>, bool) {
        use super::{IoValue, State};
        let owner = {
            let mut ownership = self.stream_content.lock_or_recover();
            let Some(ownership) = ownership.as_mut() else {
                return (Some(result), false);
            };
            if ownership.frozen != 0 {
                let mut state = self.state.lock_or_recover();
                if matches!(*state, State::Pending(_)) {
                    let State::Pending(waker) = std::mem::replace(&mut *state, State::Taken) else {
                        unreachable!()
                    };
                    *state = State::Unpublished { result, waker };
                    return (None, false);
                }
                return (Some(result), false);
            }
            ownership.publishing = true;
            ownership.owner.clone()
        };
        // Account acquisition runs outside provider locks; the publishing flag
        // makes a concurrent transfer wait before it can inspect the result.
        let result = match result {
            Ok(IoValue::Bytes(bytes)) => {
                if bytes.is_empty() {
                    Ok(IoValue::ManagedStreamItem(None))
                } else {
                    Content::new(bytes, owner)
                        .map(|content| IoValue::ManagedStreamItem(Some(content)))
                        .map_err(super::IoFailure::managed)
                }
            }
            Ok(IoValue::StreamItem(item)) => item
                .map(|bytes| Content::new(bytes, owner))
                .transpose()
                .map(IoValue::ManagedStreamItem)
                .map_err(super::IoFailure::managed),
            other => other,
        };
        (Some(result), true)
    }

    pub(super) fn finish_content_publication(&self, managed: bool) {
        if !managed {
            return;
        }
        let waiter = {
            let mut ownership = self.stream_content.lock_or_recover();
            let ownership = ownership.as_mut().expect("managed publication");
            ownership.publishing = false;
            ownership.waiter.take()
        };
        if let Some(waiter) = waiter {
            waiter.wake();
        }
    }

    /// The enclosing invocation excludes polls while this provider gates result publication.
    pub(crate) unsafe fn visit_stream_content(
        &self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        use super::{IoValue, State};
        use hew_cabi::managed::{HewManagedWalkKind, TRANSFER_PENDING};
        let kind = unsafe { (*visitor).kind };
        let transfer = matches!(
            kind,
            HewManagedWalkKind::PrepareTransfer
                | HewManagedWalkKind::CommitTransfer
                | HewManagedWalkKind::ResumeTransfer
                | HewManagedWalkKind::AbortTransfer
        );
        let identity = if transfer {
            (unsafe { crate::owned_value_budget::transfer::transfer_identity(visitor) }) as usize
        } else {
            0
        };
        let deferred = {
            let mut ownership = self.stream_content.lock_or_recover();
            let Some(ownership) = ownership.as_mut() else {
                return 0;
            };
            if kind == HewManagedWalkKind::PrepareTransfer {
                if ownership.publishing || (ownership.frozen != 0 && ownership.frozen != identity) {
                    ownership.waiter =
                        unsafe { crate::owned_value_budget::transfer::transfer_waker(visitor) };
                    return TRANSFER_PENDING;
                }
                ownership.frozen = identity;
            }
            if kind == HewManagedWalkKind::AbortTransfer && ownership.frozen != identity {
                return 0;
            }
            let mut state = self.state.lock_or_recover();
            if let State::Ready(Ok(IoValue::ManagedStreamItem(Some(content)))) = &*state {
                let status = unsafe { content.visit(visitor) };
                if status != 0 {
                    return status;
                }
            }
            if kind == HewManagedWalkKind::CommitTransfer {
                ownership.owner =
                    unsafe { crate::owned_value_budget::transfer::destination_owner(visitor) };
            }
            if matches!(
                kind,
                HewManagedWalkKind::ResumeTransfer | HewManagedWalkKind::AbortTransfer
            ) {
                ownership.frozen = 0;
                if matches!(*state, State::Unpublished { .. }) {
                    let State::Unpublished { result, waker } =
                        std::mem::replace(&mut *state, State::Taken)
                    else {
                        unreachable!()
                    };
                    *state = State::Pending(waker);
                    Some(result)
                } else {
                    None
                }
            } else {
                None
            }
        };
        // Resume publishes a producer's deferred result against the final owner.
        if let Some(result) = deferred {
            self.complete(result);
        }
        0
    }
}

pub(crate) struct Content {
    bytes: Vec<u8>,
    owner: Mutex<Option<ValueOwner>>,
}

/// Observe producer-owned content without prolonging its ordinary lifetime.
/// A transfer temporarily pins exactly the payload whose ownership it moves.
pub(crate) struct ContentView {
    content: std::sync::Weak<Content>,
    frozen: Option<Arc<Content>>,
}

impl ContentView {
    pub(crate) fn new(content: &Arc<Content>) -> Self {
        Self {
            content: Arc::downgrade(content),
            frozen: None,
        }
    }

    pub(crate) unsafe fn visit(
        &mut self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        use hew_cabi::managed::HewManagedWalkKind;
        // SAFETY: the caller retains the canonical synchronous visitor.
        let kind = unsafe { (*visitor).kind };
        if kind == HewManagedWalkKind::PrepareTransfer && self.frozen.is_none() {
            self.frozen = self.content.upgrade();
        }
        let content = self.frozen.clone().or_else(|| self.content.upgrade());
        let status = content.as_ref().map_or(0, |content| {
            // SAFETY: this strong reference pins the immutable payload through the walk.
            unsafe { content.visit(visitor) }
        });
        if matches!(
            kind,
            HewManagedWalkKind::ResumeTransfer | HewManagedWalkKind::AbortTransfer
        ) {
            self.frozen = None;
        }
        status
    }
}

impl Content {
    pub(crate) fn owner(&self) -> Option<ValueOwner> {
        self.owner.lock_or_recover().clone()
    }
    pub(crate) fn new(bytes: Vec<u8>, owner: Option<ValueOwner>) -> Result<Arc<Self>, i32> {
        let storage = ManagedStorage {
            identity: bytes.as_ptr() as usize,
            bytes: bytes.capacity() as u64,
        };
        if storage.bytes != 0 {
            if let Some(owner) = &owner {
                owner
                    .acquire(&[storage])
                    .map_err(|_| hew_cabi::managed::HEAP_EXCEEDED)?
                    .commit();
            }
        }
        Ok(Arc::new(Self {
            bytes,
            owner: Mutex::new(owner),
        }))
    }

    pub(crate) fn unbounded(bytes: Vec<u8>) -> Arc<Self> {
        Self::new(bytes, None).expect("unbounded content")
    }

    /// The caller pins this immutable payload across the whole transfer transaction.
    pub(crate) unsafe fn visit(
        &self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        use hew_cabi::managed::{HewManagedWalkKind, DESCEND, SKIP_DESCENDANTS};
        if self.bytes.capacity() == 0 {
            return 0;
        }
        // SAFETY: the retained content allocation cannot change or be freed during this walk.
        let status = unsafe {
            crate::owned_value_budget::visit::storage(
                visitor,
                self.bytes.as_ptr().cast(),
                self.bytes.capacity() as u64,
            )
        };
        if !matches!(status, DESCEND | SKIP_DESCENDANTS) {
            return status;
        }
        // SAFETY: only a successfully acquired transfer changes the refund owner.
        if unsafe { (*visitor).kind } == HewManagedWalkKind::CommitTransfer {
            *self.owner.lock_or_recover() =
                unsafe { crate::owned_value_budget::transfer::destination_owner(visitor) };
        }
        0
    }
}

impl hew_cabi::sink::SinkContent for Content {
    fn bytes(&self) -> &[u8] {
        &self.bytes
    }
}

impl std::ops::Deref for Content {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &self.bytes
    }
}

impl Drop for Content {
    fn drop(&mut self) {
        let identity = self.bytes.as_ptr();
        let capacity = self.bytes.capacity() as u64;
        drop(std::mem::take(&mut self.bytes));
        if capacity != 0 {
            if let Some(owner) = self.owner.lock_or_recover().take() {
                owner.release_backing(identity.cast(), capacity);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    unsafe extern "C" fn visit(
        slot: *const core::ffi::c_void,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: the test keeps this Arc and its immutable payload live through transfer.
        unsafe { (&*slot.cast::<Arc<Content>>()).visit(visitor) }
    }

    unsafe extern "C" fn visit_read(
        slot: *const core::ffi::c_void,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: this test retains the operation and serializes its takes.
        unsafe { (&*slot.cast::<Arc<super::super::HewAsyncIo>>()).visit_stream_content(visitor) }
    }

    #[test]
    fn read_completed_during_transfer_publishes_against_the_receiving_limit() {
        use super::super::{AsyncIoStatus, HewAsyncIo, IoValue};
        use crate::owned_value_budget::transfer::ValueTransfer;
        let managed = hew_cabi::managed::HewManagedValueDescriptor {
            slot_bytes: 0,
            visit: Some(visit_read),
        };
        let layout = hew_cabi::value::HewValueLayout {
            size: size_of::<Arc<HewAsyncIo>>(),
            align: align_of::<Arc<HewAsyncIo>>(),
            ownership_kind: hew_cabi::value::HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: core::ptr::null(),
            managed: &managed,
        };
        for limit in [5, 6] {
            let source = ValueOwner::new(Some(0));
            let destination = ValueOwner::new(Some(limit));
            // SAFETY: the test's strong reference retains the operation and its frozen payload.
            unsafe {
                let operation =
                    HewAsyncIo::with_net(core::ptr::null(), None, Some(Some(source.clone())));
                let transfer = ValueTransfer::prepare(
                    Some(source.clone()),
                    Some(destination.clone()),
                    &layout,
                    (&raw const operation).cast(),
                )
                .unwrap();
                operation.complete(Ok(IoValue::StreamItem(Some(vec![7; 6]))));
                assert_eq!(
                    operation.state.lock_or_recover().status(),
                    AsyncIoStatus::Pending
                );
                assert_eq!(source.live_bytes(), 0);
                assert_eq!(destination.live_bytes(), 0);
                transfer.commit().unwrap();
                if limit == 5 {
                    assert_eq!(
                        operation.state.lock_or_recover().status(),
                        AsyncIoStatus::Error
                    );
                    assert_eq!(
                        super::super::failure_trap(Arc::as_ptr(&operation)),
                        Some(crate::internal::types::HEW_TRAP_HEAP_EXCEEDED)
                    );
                    assert_eq!(destination.live_bytes(), 0);
                } else {
                    let (status, content) =
                        super::super::take_managed_stream_item(Arc::as_ptr(&operation));
                    assert_eq!(status, 1);
                    assert_eq!(&content.as_ref().unwrap()[..], &[7; 6]);
                    assert_eq!(destination.live_bytes(), 6);
                    drop(content);
                    assert_eq!(destination.live_bytes(), 0);
                }
            }
        }
    }

    #[test]
    fn transferred_content_refunds_the_receiving_account() {
        let source = ValueOwner::new(Some(16));
        let destination = ValueOwner::new(Some(16));
        let content = Content::new(Vec::with_capacity(16), Some(source.clone())).unwrap();
        let managed = hew_cabi::managed::HewManagedValueDescriptor {
            slot_bytes: 0,
            visit: Some(visit),
        };
        let layout = hew_cabi::value::HewValueLayout {
            size: size_of::<Arc<Content>>(),
            align: align_of::<Arc<Content>>(),
            ownership_kind: hew_cabi::value::HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: core::ptr::null(),
            managed: &managed,
        };
        // SAFETY: this pinned immutable value does not run or change during transfer.
        unsafe {
            crate::owned_value_budget::transfer::ValueTransfer::prepare(
                Some(source.clone()),
                Some(destination.clone()),
                &layout,
                (&raw const content).cast(),
            )
            .unwrap()
            .commit()
            .unwrap();
        }
        assert_eq!(source.live_bytes(), 0);
        assert_eq!(destination.live_bytes(), 16);
        drop(content);
        assert_eq!(destination.live_bytes(), 0);
    }

    #[test]
    fn content_capacity_lives_until_the_final_producer_releases_it() {
        let owner = ValueOwner::new(Some(32));
        let content = Content::new(Vec::with_capacity(32), Some(owner.clone())).unwrap();
        let operation_view = Arc::downgrade(&content);
        let producer = Arc::clone(&content);
        assert_eq!(owner.live_bytes(), 32);
        drop(content);
        assert_eq!(owner.live_bytes(), 32);
        assert!(operation_view.upgrade().is_some());
        drop(producer);
        assert_eq!(owner.live_bytes(), 0);
        assert!(operation_view.upgrade().is_none());
    }

    #[test]
    fn partial_socket_content_keeps_its_receipt_through_finish() {
        use hew_cabi::sink::{into_nonblocking_sink_ptr, PendingBytes, TrySendResult};
        let owner = ValueOwner::new(Some(16));
        let mut bytes = Vec::with_capacity(16);
        bytes.extend_from_slice(b"abcdef");
        let content = Content::new(bytes, Some(owner.clone())).unwrap();
        let identity = content.as_ptr();
        let sink = into_nonblocking_sink_ptr((), |_, _| true, |_, _| Ok(2), |_| {}, |_| {});
        // SAFETY: the test owns this freshly created endpoint and finishes its sole loan.
        unsafe {
            assert_eq!(
                (*sink).try_write_owned(PendingBytes::retained(content)),
                TrySendResult::Accepted
            );
            assert_eq!(owner.live_bytes(), 16);
            let mut finish = (*sink).begin_finish().unwrap();
            let pending = finish.take_pending();
            assert_eq!(&*pending, b"cdef");
            assert_eq!(pending.as_ptr(), identity.add(2));
            finish.finish();
            drop(Box::from_raw(sink));
            assert_eq!(owner.live_bytes(), 16);
            drop(pending);
        }
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn empty_content_needs_no_payload_capacity_and_refusal_leaves_no_charge() {
        let owner = ValueOwner::new(Some(0));
        let empty = Content::new(Vec::new(), Some(owner.clone())).unwrap();
        assert_eq!(owner.live_bytes(), 0);
        assert!(matches!(
            Content::new(vec![1], Some(owner.clone())),
            Err(hew_cabi::managed::HEAP_EXCEEDED)
        ));
        assert_eq!(owner.live_bytes(), 0);
        drop(empty);
    }
}
