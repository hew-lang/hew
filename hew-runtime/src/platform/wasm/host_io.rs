//! `wasm32` host I/O stub backend.

use crate::actor_native::NativeActorCompletion;
use crate::internal::types::{ActorEndReason, AskError};
use crate::lifetime::local_handles::HewLocalPidId;
use crate::monitor::MonitorState;
use crate::wake::HewWaker;
use hew_cabi::vec::HewValueLayout;
use std::ffi::c_void;
use std::sync::Arc;

#[derive(Debug)]
pub(crate) struct RemoteAdmissionSlot;

impl RemoteAdmissionSlot {
    pub(crate) const NONE: Self = Self;

    #[inline]
    pub(crate) fn is_some(&self) -> bool {
        false
    }

    #[inline]
    pub(crate) fn claim(
        &self,
        claim_fn: impl FnOnce() -> Result<u8, u8>,
    ) -> Result<Result<u8, u8>, AskError> {
        Ok(claim_fn())
    }
}

#[derive(Debug)]
pub(crate) struct ImportedWaitOperation;

impl ImportedWaitOperation {
    pub(crate) const NONE: Self = Self;

    /// # Safety
    /// No-op on `wasm32`.
    #[inline]
    pub(crate) unsafe fn poll(&mut self) {}
}

#[inline]
pub(crate) fn cancelled_by_shutdown(_code: i32) -> bool {
    false
}

#[inline]
pub(crate) fn normalize_main_status(source_status: i32) -> i32 {
    source_status
}

#[inline]
pub(crate) fn retire_current_actor_names(_actor_id: u64) {}

#[inline]
pub(crate) fn record_link_probe_terminal(_actor_id: u64, _terminal: i32) {}

#[inline]
pub(crate) fn unlink_imported_actor(_current_actor_id: u64, _target: HewLocalPidId) -> bool {
    false
}

#[inline]
pub(crate) fn imported_actor_handles_identical(
    _left: HewLocalPidId,
    _right: HewLocalPidId,
) -> Option<bool> {
    None
}

#[inline]
pub(crate) unsafe fn try_start_imported_actor_wait(
    _token: HewLocalPidId,
    _waker: *const HewWaker,
) -> Option<(Arc<NativeActorCompletion>, ImportedWaitOperation)> {
    None
}

pub(crate) struct RemoteMonitorWatcherBatch;

#[inline]
pub(crate) fn take_remote_watchers_for_actor(
    _state: &MonitorState,
    _actor_id: u64,
) -> RemoteMonitorWatcherBatch {
    RemoteMonitorWatcherBatch
}

#[inline]
pub(crate) fn fan_out_remote_monitor_down(
    _watchers: RemoteMonitorWatcherBatch,
    _reason: i32,
    _crash_kind: u32,
    _end: ActorEndReason,
    _close_fault: Option<&(i32, String)>,
) {
}

#[inline]
pub(crate) fn capture_trace_actor_identity(_actor_id: u64) -> (Option<u64>, usize) {
    (None, 0)
}

#[inline]
pub(crate) fn remote_crash_origin(
    _link_target: Option<crate::monitor::HewNotificationTarget>,
) -> String {
    String::new()
}

#[inline]
pub(crate) fn observe_barrier() -> i64 {
    crate::observe::OBSERVE_BARRIER_OK
}

#[inline]
pub(crate) fn read_host_observe_u64(name: &str) -> Option<u64> {
    match name {
        "scheduler.workers" => Some(1),
        "heap.live_bytes"
        | "scheduler.runnable_actors"
        | "scheduler.runnable_coroutines"
        | "scheduler.queue_depth"
        | "scheduler.steals_total"
        | "actors.live" => Some(0),
        _ => None,
    }
}

#[inline]
pub(crate) fn observe_handler_name(_dispatch_ptr: usize, _msg_type: i32) -> Option<String> {
    None
}

#[inline]
pub(crate) fn supervisor_restarts_by_child_snapshot() -> Vec<(String, String, u64)> {
    Vec::new()
}

#[inline]
pub(crate) fn append_mailbox_loss_series(_out: &mut String, _series: &[&str; 4]) {}

#[derive(Debug)]
pub(crate) struct RemoteActorObservation;

#[inline]
pub(crate) fn new_remote_actor_observation() -> RemoteActorObservation {
    RemoteActorObservation
}

#[inline]
pub(crate) fn poll_imported_observation(
    obs: &mut crate::actor_observe::HewNativeActorObservation,
) -> Option<bool> {
    let _ = &obs.transport;
    None
}

#[inline]
pub(crate) fn retire_imported_observation(
    _obs: &mut crate::actor_observe::HewNativeActorObservation,
) -> bool {
    false
}

#[inline]
pub(crate) unsafe fn try_start_imported_observation(
    _obs: &mut crate::actor_observe::HewNativeActorObservation,
    _token: HewLocalPidId,
    _waker: *const HewWaker,
) -> bool {
    false
}

#[derive(Debug)]
pub(crate) enum ImportedActorCall {}

impl ImportedActorCall {
    #[inline]
    pub(crate) unsafe fn visit_managed(
        &self,
        _visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        match *self {}
    }

    #[inline]
    pub(crate) fn failure_snapshot(&self) -> Option<Arc<crate::failure_snapshot::FailureSnapshot>> {
        match *self {}
    }

    #[inline]
    pub(crate) unsafe fn prepare_cleanup(&mut self) -> *mut crate::mailbox::HewMsgEnvelope {
        match *self {}
    }

    #[inline]
    pub(crate) fn has_deadline(&self) -> bool {
        match *self {}
    }

    #[inline]
    pub(crate) unsafe fn poll(&self) -> i32 {
        match *self {}
    }

    #[inline]
    pub(crate) unsafe fn take_outcome(
        &self,
        _output: *mut std::ffi::c_void,
    ) -> crate::request_state::RequestOutcome {
        match *self {}
    }

    #[inline]
    pub(crate) fn detach_request(&mut self) -> *mut crate::mailbox::HewMsgEnvelope {
        match *self {}
    }
}

#[expect(
    clippy::too_many_arguments,
    reason = "matches native call initiation seam signature"
)]
#[inline]
pub(crate) unsafe fn try_start_imported_actor_call(
    _token: HewLocalPidId,
    _dispatch: *const std::ffi::c_void,
    _message: i32,
    _envelope: *mut crate::mailbox::HewMsgEnvelope,
    _reply_size: usize,
    _deadline_ns: i64,
    _waker: *const HewWaker,
    _kind: crate::request_state::RequestKind,
) -> Option<ImportedActorCall> {
    None
}

pub(crate) struct DroppedImportedActor;

#[derive(Default)]
pub(crate) struct ImportedActorRegistry;

impl ImportedActorRegistry {
    #[inline]
    pub(crate) fn retain(&mut self, _token: HewLocalPidId) -> bool {
        false
    }

    #[inline]
    pub(crate) fn release(&mut self, _token: HewLocalPidId) -> DroppedImportedActor {
        DroppedImportedActor
    }
}

#[inline]
pub(crate) fn try_submit_offload_job(
    _operation: &Arc<crate::async_io::HewAsyncIo>,
    env: crate::async_io::offload::OffloadEnv,
) -> Result<(), Option<crate::async_io::offload::OffloadEnv>> {
    Err(Some(env))
}

#[inline]
pub(crate) unsafe fn start_async_sink_finish(
    sink_ref: &crate::stream::HewSink,
    _operation: &Arc<crate::async_io::HewAsyncIo>,
    _waker: *const HewWaker,
) -> bool {
    sink_ref.finish();
    false
}

#[inline]
pub(crate) unsafe fn async_sink_cleanup_fault_message(
    _operation: *const crate::async_io::HewAsyncIo,
) -> Option<String> {
    None
}

#[inline]
pub(crate) unsafe fn start_stdin_read_line(
    waker: *const HewWaker,
) -> *const crate::async_io::HewAsyncIo {
    // SAFETY: forwarded from caller contract.
    unsafe { crate::async_io::stdin::start_blocking_read_line(waker) }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct HostRuntimeLease;

impl HostRuntimeLease {
    pub(crate) const NULL: Self = Self;

    #[inline]
    pub(crate) fn release(self) {}
}

#[inline]
pub(crate) unsafe fn unregister_actor_names(_actor_id: u64) {}

#[derive(Clone, Copy)]
pub(crate) enum StreamAsyncIo {}

impl StreamAsyncIo {
    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn visit_stream_content(
        &self,
        _visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        match *self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn free(self) {
        match self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn failure_trap(&self) -> Option<i32> {
        match *self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn failure_message(&self) -> Option<String> {
        match *self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn poll_write(&self) -> i32 {
        match *self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn timed_out_committed(&self, _write_prefix: usize) -> i64 {
        match *self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn cancel(&self) {
        match *self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn cleanup_status(&self, _waker: *const HewWaker) -> i32 {
        match *self {}
    }
}

pub(crate) enum StreamImmediate {}

impl StreamImmediate {
    #[inline]
    pub(crate) fn status(&self) -> i32 {
        match *self {}
    }

    #[inline]
    pub(crate) fn take_fault(&mut self) -> *mut crate::fault::HewFault {
        match *self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn visit_release(
        &self,
        _visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        match *self {}
    }

    /// # Safety
    /// Uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn release_begin(&mut self) -> *mut crate::release_walker::HewReleaseCursor {
        match *self {}
    }
}

pub(crate) struct StreamIoFields;

impl StreamIoFields {
    pub(crate) const EMPTY: Self = Self;

    #[inline]
    pub(crate) fn write_prefix(&self) -> usize {
        0
    }

    #[inline]
    pub(crate) fn has_write_content(&self) -> bool {
        false
    }

    /// # Safety
    /// No-op on `wasm32`.
    #[inline]
    pub(crate) unsafe fn visit(
        &mut self,
        _visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        0
    }

    /// # Safety
    /// `StreamAsyncIo` is uninhabited on `wasm32`.
    #[inline]
    pub(crate) unsafe fn poll_io(
        &mut self,
        io: StreamAsyncIo,
        _layout_size: usize,
    ) -> (i32, Option<Vec<u8>>) {
        match io {}
    }

    /// # Safety
    /// No-op on `wasm32`.
    #[inline]
    pub(crate) unsafe fn take_read_content(
        &mut self,
        _layout: &HewValueLayout,
        _out: *mut c_void,
        _fault: &mut *mut crate::fault::HewFault,
    ) -> Option<i32> {
        None
    }
}

/// A stream with no queue core is a file or a socket, and both are manifest
/// rejects on wasm32. Reaching here means a lowering defect rather than a
/// program outcome, so the module fails closed.
fn reactor_backing_unreachable(operation: &str) -> ! {
    eprintln!("hew: fail-closed: wasm32 stream {operation} with no pipe backing");
    std::process::abort();
}

/// # Safety
/// `wasm32` targets do not compile reactor stream backings and fail closed.
#[inline]
pub(crate) unsafe fn try_start_stream_reactor_read(
    _stream: *mut crate::stream::HewStream,
    _layout: HewValueLayout,
    _parent: *mut crate::coro_state::HewCoroState,
    _waker: *const HewWaker,
) -> Option<(StreamAsyncIo, StreamIoFields)> {
    reactor_backing_unreachable("read")
}

#[inline]
pub(crate) unsafe fn try_nonblocking_stream_write(
    _sink: *mut crate::stream::HewSink,
    _data: *mut c_void,
    _layout: HewValueLayout,
    _parent: *const crate::coro_state::HewCoroState,
) -> Result<(StreamImmediate, StreamIoFields), i32> {
    Err(hew_cabi::managed::UNDESCRIBED)
}

#[inline]
pub(crate) unsafe fn try_start_sink_reactor_write(
    _sink: *mut crate::stream::HewSink,
    _waker: *const HewWaker,
    _data: *mut c_void,
    _layout: HewValueLayout,
    _parent: *const crate::coro_state::HewCoroState,
) -> Option<Result<(StreamAsyncIo, StreamIoFields), i32>> {
    None
}

#[inline]
pub(crate) fn missing_stream_pipe_backing(operation: &str) -> ! {
    reactor_backing_unreachable(operation)
}
