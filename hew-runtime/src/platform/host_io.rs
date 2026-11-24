//! Host I/O, observe/profiler telemetry, and remote transport hook seam.

use super::imp;
use crate::actor_native::NativeActorCompletion;
use crate::internal::types::ActorEndReason;
use crate::lifetime::local_handles::HewLocalPidId;
use crate::monitor::MonitorState;
use crate::wake::HewWaker;
use hew_cabi::vec::HewValueLayout;
use std::ffi::c_void;
use std::sync::Arc;

/// Optional remote-export admission handle stored on queued requests.
///
/// On native targets this retains an authenticated remote admission; on `wasm32`
/// it is zero-sized.
pub(crate) type RemoteAdmissionSlot = imp::host_io::RemoteAdmissionSlot;

/// Optional imported-actor wait operation tracked on `HewNativeActorWait`.
///
/// On native targets this wraps an owned [`crate::actor_observe::HewNativeActorObservation`];
/// on `wasm32` it is zero-sized.
pub(crate) type ImportedWaitOperation = imp::host_io::ImportedWaitOperation;

/// Return whether `error_code` represents a cancellation caused by host shutdown.
#[inline]
pub(crate) fn cancelled_by_shutdown(code: i32) -> bool {
    imp::host_io::cancelled_by_shutdown(code)
}

/// Normalize a `main` return status code for process exit semantics.
#[inline]
pub(crate) fn normalize_main_status(source_status: i32) -> i32 {
    imp::host_io::normalize_main_status(source_status)
}

/// Retire registered node-level actor names when an actor terminates.
#[inline]
pub(crate) fn retire_current_actor_names(actor_id: u64) {
    imp::host_io::retire_current_actor_names(actor_id);
}

/// Record a terminal actor crash in the cross-node link probe ledger when active.
#[inline]
pub(crate) fn record_link_probe_terminal(actor_id: u64, terminal: i32) {
    imp::host_io::record_link_probe_terminal(actor_id, terminal);
}

/// Unlink an imported remote actor handle when `target` names an imported peer.
///
/// Returns `true` if `target` was an imported handle and was processed.
#[inline]
pub(crate) fn unlink_imported_actor(current_actor_id: u64, target: HewLocalPidId) -> bool {
    imp::host_io::unlink_imported_actor(current_actor_id, target)
}

/// Compare two actor handles via imported remote identity rules when applicable.
///
/// Returns `Some(true/false)` when role/import identity resolved the pair, or
/// `None` when the handles require ordinary local resolution.
#[inline]
pub(crate) fn imported_actor_handles_identical(
    left: HewLocalPidId,
    right: HewLocalPidId,
) -> Option<bool> {
    imp::host_io::imported_actor_handles_identical(left, right)
}

/// Attempt to start a native wait on an imported actor handle.
///
/// # Safety
/// `waker` must obey the retained native wake descriptor contract.
#[inline]
pub(crate) unsafe fn try_start_imported_actor_wait(
    token: HewLocalPidId,
    waker: *const HewWaker,
) -> Option<(Arc<NativeActorCompletion>, ImportedWaitOperation)> {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::try_start_imported_actor_wait(token, waker) }
}

/// Snapshot and remove any remote watchers registered against `actor_id`.
pub(crate) type RemoteMonitorWatcherBatch = imp::host_io::RemoteMonitorWatcherBatch;

/// Claim remote watchers recorded for `actor_id` inside `state` on native targets.
#[inline]
pub(crate) fn take_remote_watchers_for_actor(
    state: &MonitorState,
    actor_id: u64,
) -> RemoteMonitorWatcherBatch {
    imp::host_io::take_remote_watchers_for_actor(state, actor_id)
}

/// Fan out remote `DOWN` notices to previously claimed remote watchers.
#[inline]
pub(crate) fn fan_out_remote_monitor_down(
    watchers: RemoteMonitorWatcherBatch,
    reason: i32,
    crash_kind: u32,
    end: ActorEndReason,
    close_fault: Option<&(i32, String)>,
) {
    imp::host_io::fan_out_remote_monitor_down(watchers, reason, crash_kind, end, close_fault);
}

/// Capture a profiler actor identity `(spawn_serial, dispatch)` for tracing.
#[inline]
pub(crate) fn capture_trace_actor_identity(actor_id: u64) -> (Option<u64>, usize) {
    imp::host_io::capture_trace_actor_identity(actor_id)
}

/// Resolve a remote notification target description for crash reporting.
#[inline]
pub(crate) fn remote_crash_origin(
    link_target: Option<crate::monitor::HewNotificationTarget>,
) -> String {
    imp::host_io::remote_crash_origin(link_target)
}

/// Wait on the observe attribution barrier, returning 0 on success or negative
/// error code on failure/timeout.
#[inline]
pub(crate) fn observe_barrier() -> i64 {
    imp::host_io::observe_barrier()
}

/// Read a target-dependent host or scheduler observability gauge/counter.
#[inline]
pub(crate) fn read_host_observe_u64(name: &str) -> Option<u64> {
    imp::host_io::read_host_observe_u64(name)
}

/// Look up a registered handler name for attributed-turn Prometheus series.
#[inline]
pub(crate) fn observe_handler_name(dispatch_ptr: usize, msg_type: i32) -> Option<String> {
    imp::host_io::observe_handler_name(dispatch_ptr, msg_type)
}

/// Snapshot `(supervisor_label, child_label, restarts)` rows when supported.
#[inline]
pub(crate) fn supervisor_restarts_by_child_snapshot() -> Vec<(String, String, u64)> {
    imp::host_io::supervisor_restarts_by_child_snapshot()
}

/// Append per-actor mailbox loss Prometheus series when host profiling is active.
#[inline]
pub(crate) fn append_mailbox_loss_series(out: &mut String, series: &[&str; 4]) {
    imp::host_io::append_mailbox_loss_series(out, series);
}

/// Target-specific remote state attached to an actor observation handle.
///
/// Zero-sized on `wasm32`.
pub(crate) type RemoteActorObservation = imp::host_io::RemoteActorObservation;

/// Capture the initial target-specific state for a new actor observation.
#[inline]
pub(crate) fn new_remote_actor_observation() -> RemoteActorObservation {
    imp::host_io::new_remote_actor_observation()
}

/// Poll a native imported observation, returning `None` when handled locally.
#[inline]
pub(crate) fn poll_imported_observation(
    obs: &mut crate::actor_observe::HewNativeActorObservation,
) -> Option<bool> {
    imp::host_io::poll_imported_observation(obs)
}

/// Retire imported observation setup state on drop; returns `true` when the
/// observation owned remote setup state and handled cleanup.
#[inline]
pub(crate) fn retire_imported_observation(
    obs: &mut crate::actor_observe::HewNativeActorObservation,
) -> bool {
    imp::host_io::retire_imported_observation(obs)
}

/// Start monitoring/linking an imported remote actor if `token` resolves to one.
///
/// Returns `true` when `token` named an imported remote actor and started setup.
///
/// # Safety
/// `waker` must remain a live borrowed wake descriptor throughout registration.
#[inline]
pub(crate) unsafe fn try_start_imported_observation(
    obs: &mut crate::actor_observe::HewNativeActorObservation,
    token: HewLocalPidId,
    waker: *const HewWaker,
) -> bool {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::try_start_imported_observation(obs, token, waker) }
}

/// Target-specific call state for imported remote actor invocations.
pub(crate) type ImportedActorCall = imp::host_io::ImportedActorCall;

/// Target-specific registry of imported actor descriptors stored inside `LocalHandles`.
pub(crate) type ImportedActorRegistry = imp::host_io::ImportedActorRegistry;

/// Unregister any node-level actor names during actor teardown.
///
/// # Safety
/// Must be called only while tearing down `actor_id` after dispatch quiescence.
#[inline]
pub(crate) unsafe fn unregister_actor_names(actor_id: u64) {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::unregister_actor_names(actor_id) };
}

/// Submit an `#[offload]` closure onto the native blocking pool if available.
#[inline]
pub(crate) fn try_submit_offload_job(
    operation: &Arc<crate::async_io::HewAsyncIo>,
    env: crate::async_io::offload::OffloadEnv,
) -> Result<(), Option<crate::async_io::offload::OffloadEnv>> {
    imp::host_io::try_submit_offload_job(operation, env)
}

/// Start an asynchronous native sink finish operation; returns `true` when
/// completed or deferred asynchronously via native reactor/pool hooks.
///
/// # Safety
/// `waker` must remain live for the duration of native sink finish setup.
#[inline]
pub(crate) unsafe fn start_async_sink_finish(
    sink_ref: &crate::stream::HewSink,
    operation: &Arc<crate::async_io::HewAsyncIo>,
    waker: *const HewWaker,
) -> bool {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::start_async_sink_finish(sink_ref, operation, waker) }
}

/// Extract diagnostic failure detail from a failed async sink finish operation.
///
/// # Safety
/// `operation` must remain a live quiescent [`crate::async_io::HewAsyncIo`].
#[inline]
pub(crate) unsafe fn async_sink_cleanup_fault_message(
    operation: *const crate::async_io::HewAsyncIo,
) -> Option<String> {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::async_sink_cleanup_fault_message(operation) }
}

/// Start reading one line from standard input via reactor or blocking WASI stdin.
///
/// # Safety
/// `waker` must be null or a valid borrowed [`HewWaker`] descriptor.
#[inline]
pub(crate) unsafe fn start_stdin_read_line(
    waker: *const HewWaker,
) -> *const crate::async_io::HewAsyncIo {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::start_stdin_read_line(waker) }
}

/// Target-specific retained host runtime lease held by [`crate::host_call::HewHostCall`].
pub(crate) type HostRuntimeLease = imp::host_io::HostRuntimeLease;

/// Attempt to initiate a checked actor call against an imported remote service.
///
/// # Safety
/// `envelope` must be either null or a valid owned [`crate::mailbox::HewMsgEnvelope`]
/// whose transfer ownership passes to the returned [`ImportedActorCall`]; `waker`
/// must remain a live borrowed wake descriptor.
#[expect(
    clippy::too_many_arguments,
    reason = "forwarding checked call ABI parameters without allocating an intermediate descriptor"
)]
#[inline]
pub(crate) unsafe fn try_start_imported_actor_call(
    token: HewLocalPidId,
    dispatch: *const std::ffi::c_void,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
    reply_size: usize,
    deadline_ns: i64,
    waker: *const HewWaker,
    kind: crate::request_state::RequestKind,
) -> Option<ImportedActorCall> {
    // SAFETY: forwarded from caller contract.
    unsafe {
        imp::host_io::try_start_imported_actor_call(
            token,
            dispatch,
            message,
            envelope,
            reply_size,
            deadline_ns,
            waker,
            kind,
        )
    }
}

/// Target-specific handle to a reactor-backed stream I/O operation.
pub(crate) type StreamAsyncIo = imp::host_io::StreamAsyncIo;

/// Target-specific result of a non-blocking stream write attempt.
pub(crate) type StreamImmediate = imp::host_io::StreamImmediate;

/// Target-specific reactor and managed-content fields held by `HewNativeStream`.
pub(crate) type StreamIoFields = imp::host_io::StreamIoFields;

/// Begin a reactor-backed stream read on targets that support reactor I/O.
///
/// Aborts fail-closed on `wasm32` where reactor stream backings cannot exist.
///
/// # Safety
/// `stream` must be an exclusively borrowed [`crate::stream::HewStream`],
/// `parent` null or live for construction, and `waker` a live borrowed descriptor.
#[inline]
pub(crate) unsafe fn try_start_stream_reactor_read(
    stream: *mut crate::stream::HewStream,
    layout: HewValueLayout,
    parent: *mut crate::coro_state::HewCoroState,
    waker: *const HewWaker,
) -> Option<(StreamAsyncIo, StreamIoFields)> {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::try_start_stream_reactor_read(stream, layout, parent, waker) }
}

/// Perform one non-parking capacity attempt for a legacy content sink.
///
/// # Safety
/// The caller must hold the underlying sender gate and pass initialized `data`
/// matching `layout`.
#[inline]
pub(crate) unsafe fn try_nonblocking_stream_write(
    sink: *mut crate::stream::HewSink,
    data: *mut c_void,
    layout: HewValueLayout,
    parent: *const crate::coro_state::HewCoroState,
) -> Result<(StreamImmediate, StreamIoFields), i32> {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::try_nonblocking_stream_write(sink, data, layout, parent) }
}

/// Attempt to start a reactor-backed write on an open legacy content sink.
///
/// Returns `None` when the sink is closed, pipe-backed, or running on `wasm32`.
///
/// # Safety
/// The caller must retain the exclusive sink loan, live `waker`, initialized `data`
/// matching `layout`, and optional `parent`.
#[inline]
pub(crate) unsafe fn try_start_sink_reactor_write(
    sink: *mut crate::stream::HewSink,
    waker: *const HewWaker,
    data: *mut c_void,
    layout: HewValueLayout,
    parent: *const crate::coro_state::HewCoroState,
) -> Option<Result<(StreamAsyncIo, StreamIoFields), i32>> {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::host_io::try_start_sink_reactor_write(sink, waker, data, layout, parent) }
}

/// Fail-closed abort when a stream operation unexpectedly lacks a pipe backing.
#[inline]
pub(crate) fn missing_stream_pipe_backing(operation: &str) -> ! {
    imp::host_io::missing_stream_pipe_backing(operation)
}
