//! Native host I/O, observe/profiler, and remote transport hooks.

use crate::actor_native::NativeActorCompletion;
use crate::internal::types::{ActorEndReason, AskError};
use crate::lifetime::local_handles::HewLocalPidId;
use crate::monitor::{MonitorState, RemoteWatcherTarget};
use crate::wake::HewWaker;
use hew_cabi::vec::{HewTypeOwnershipKind, HewValueLayout};
use std::ffi::c_void;
use std::sync::Arc;

#[derive(Debug)]
pub(crate) struct RemoteAdmissionSlot(Option<crate::hew_node::service::RemoteAdmission>);

impl RemoteAdmissionSlot {
    pub(crate) const NONE: Self = Self(None);

    #[inline]
    pub(crate) fn from_native(
        admission: Option<crate::hew_node::service::RemoteAdmission>,
    ) -> Self {
        Self(admission)
    }

    #[inline]
    pub(crate) fn is_some(&self) -> bool {
        self.0.is_some()
    }

    #[inline]
    pub(crate) fn claim(
        &self,
        claim_fn: impl FnOnce() -> Result<u8, u8>,
    ) -> Result<Result<u8, u8>, AskError> {
        if let Some(admission) = &self.0 {
            admission.claim(claim_fn)
        } else {
            Ok(claim_fn())
        }
    }
}

#[derive(Debug)]
pub(crate) struct ImportedWaitOperation(
    Option<Box<crate::actor_observe::HewNativeActorObservation>>,
);

impl ImportedWaitOperation {
    pub(crate) const NONE: Self = Self(None);

    /// # Safety
    /// The unique wait owner must serialize polls of its setup operation.
    #[inline]
    pub(crate) unsafe fn poll(&mut self) {
        if let Some(operation) = &mut self.0 {
            // SAFETY: forwarded from caller contract.
            unsafe { crate::actor_observe::hew_native_actor_observe_poll(&raw mut **operation) };
        }
    }
}

#[inline]
pub(crate) fn cancelled_by_shutdown(code: i32) -> bool {
    code == crate::fault::HEW_FAULT_CANCELLED && crate::shutdown::hew_is_shutting_down() != 0
}

#[inline]
pub(crate) fn normalize_main_status(source_status: i32) -> i32 {
    crate::exit_status::to_process_exit_byte(i64::from(source_status))
}

#[inline]
pub(crate) fn retire_current_actor_names(actor_id: u64) {
    crate::hew_node::retire_current_actor_names(actor_id);
}

#[inline]
pub(crate) fn record_link_probe_terminal(actor_id: u64, terminal: i32) {
    crate::link::record_link_probe_terminal(actor_id, terminal);
}

pub(crate) fn unlink_imported_actor(current_actor_id: u64, target: HewLocalPidId) -> bool {
    let Some(imported) = crate::lifetime::local_handles::current_imported_actor(target) else {
        return false;
    };
    for (id, authority) in crate::runtime::rt_current()
        .monitors
        .remove_imported_links(current_actor_id, imported.identity())
    {
        crate::hew_node::monitor_link::send_admitted_cleanup(
            imported.identity(),
            authority,
            id,
            true,
        );
    }
    true
}

pub(crate) fn imported_actor_handles_identical(
    left: HewLocalPidId,
    right: HewLocalPidId,
) -> Option<bool> {
    if crate::lifetime::local_handles::current_actor_role_owner_slot(left).is_some()
        || crate::lifetime::local_handles::current_actor_role_owner_slot(right).is_some()
    {
        return Some(false);
    }
    let left_import = crate::lifetime::local_handles::current_imported_actor(left);
    let right_import = crate::lifetime::local_handles::current_imported_actor(right);
    match (left_import, right_import) {
        (Some(left), Some(right)) => Some(left.identity() == right.identity()),
        (Some(imported), None) => Some(
            crate::hew_node::service_api::local_actor_identity(right) == Some(imported.identity()),
        ),
        (None, Some(imported)) => Some(
            crate::hew_node::service_api::local_actor_identity(left) == Some(imported.identity()),
        ),
        (None, None) => None,
    }
}

pub(crate) unsafe fn try_start_imported_actor_wait(
    token: HewLocalPidId,
    waker: *const HewWaker,
) -> Option<(Arc<NativeActorCompletion>, ImportedWaitOperation)> {
    let imported = crate::lifetime::local_handles::current_imported_actor(token)?;
    // SAFETY: forwarded live wake descriptor contract.
    let (completion, operation) =
        unsafe { crate::hew_node::monitor_link::imported_actor_wait(&imported, waker) };
    Some((completion, ImportedWaitOperation(Some(operation))))
}

pub(crate) type RemoteMonitorWatcherBatch = Vec<RemoteWatcherTarget>;

pub(crate) fn take_remote_watchers_for_actor(
    state: &MonitorState,
    actor_id: u64,
) -> RemoteMonitorWatcherBatch {
    state.take_remote_watchers(crate::pid::hew_pid_serial(actor_id))
}

#[inline]
pub(crate) fn fan_out_remote_monitor_down(
    watchers: RemoteMonitorWatcherBatch,
    reason: i32,
    crash_kind: u32,
    end: ActorEndReason,
    close_fault: Option<&(i32, String)>,
) {
    crate::hew_node::fan_out_remote_monitor_down(watchers, reason, crash_kind, end, close_fault);
}

#[cfg(feature = "profiler")]
#[inline]
pub(crate) fn capture_trace_actor_identity(actor_id: u64) -> (Option<u64>, usize) {
    crate::profiler::actor_registry::capture_actor_identity(actor_id)
        .map_or((None, 0), |(identity, dispatch)| {
            (identity.spawn_serial, dispatch)
        })
}

#[cfg(not(feature = "profiler"))]
#[inline]
pub(crate) fn capture_trace_actor_identity(_actor_id: u64) -> (Option<u64>, usize) {
    (None, 0)
}

pub(crate) fn remote_crash_origin(
    link_target: Option<crate::monitor::HewNotificationTarget>,
) -> String {
    link_target
        .and_then(crate::monitor::HewNotificationTarget::remote_location)
        .map_or_else(String::new, |location| {
            format!("hew:   while handling link exit from remote {location}\n")
        })
}

#[inline]
pub(crate) fn observe_barrier() -> i64 {
    crate::observe::native_observe_barrier()
}

#[cfg(feature = "profiler")]
#[inline]
fn heap_live_bytes() -> u64 {
    crate::profiler::allocator::hew_memory_bytes_live()
}

#[cfg(not(feature = "profiler"))]
#[inline]
fn heap_live_bytes() -> u64 {
    0
}

pub(crate) fn read_host_observe_u64(name: &str) -> Option<u64> {
    match name {
        "heap.live_bytes" => Some(heap_live_bytes()),
        "scheduler.workers" => Some(crate::scheduler::hew_sched_metrics_worker_count()),
        "scheduler.runnable_actors" => Some(crate::scheduler::hew_sched_metrics_runnable_actors()),
        "scheduler.runnable_coroutines" => {
            Some(crate::scheduler::hew_sched_metrics_runnable_coroutines())
        }
        "scheduler.queue_depth" => Some(crate::scheduler::hew_sched_metrics_queue_depth()),
        "scheduler.steals_total" => Some(crate::scheduler::hew_sched_metrics_steals()),
        "actors.live" => Some(crate::profiler::actor_registry::snapshot_all().len() as u64),
        #[cfg(feature = "profiler")]
        "mailbox.depth" => Some(crate::profiler::actor_registry::mailbox_aggregate().sum_depth),
        #[cfg(feature = "profiler")]
        "mailbox.depth_max" => Some(crate::profiler::actor_registry::mailbox_aggregate().max_depth),
        #[cfg(feature = "profiler")]
        "mailbox.depth_p99" => Some(crate::profiler::actor_registry::mailbox_aggregate().p99_depth),
        "registry.conflicts_total" => Some(crate::registry::metrics_snapshot().1),
        _ => crate::hew_node::observe_metric(name),
    }
}

#[cfg(feature = "profiler")]
#[inline]
pub(crate) fn observe_handler_name(dispatch_ptr: usize, msg_type: i32) -> Option<String> {
    crate::profiler::actor_registry::handler_name_by_ptr(dispatch_ptr, msg_type)
}

#[cfg(not(feature = "profiler"))]
#[inline]
pub(crate) fn observe_handler_name(_dispatch_ptr: usize, _msg_type: i32) -> Option<String> {
    None
}

#[cfg(feature = "profiler")]
#[inline]
pub(crate) fn supervisor_restarts_by_child_snapshot() -> Vec<(String, String, u64)> {
    crate::supervisor::restarts_by_child_snapshot()
}

#[cfg(not(feature = "profiler"))]
#[inline]
pub(crate) fn supervisor_restarts_by_child_snapshot() -> Vec<(String, String, u64)> {
    Vec::new()
}

#[cfg(feature = "profiler")]
pub(crate) fn append_mailbox_loss_series(out: &mut String, series: &[&str; 4]) {
    use std::fmt::Write as _;

    let actors = crate::profiler::actor_registry::snapshot_all();
    for (index, name) in series.iter().enumerate() {
        let name = crate::observe::render_prometheus_name(name);
        let _ = writeln!(out, "# TYPE {name} counter");
        for actor in &actors {
            let (Some(identity), Some(mailbox)) = (&actor.identity, &actor.mailbox) else {
                continue;
            };
            let Some(serial) = identity.spawn_serial else {
                continue;
            };
            let values = [
                mailbox.dropped_new,
                mailbox.dropped_old,
                mailbox.coalesced,
                mailbox.dropped_on_stop,
            ];
            let _ = writeln!(
                out,
                "{name}{{actor=\"{}\",spawn_serial=\"{serial}\"}} {}",
                identity.actor_id, values[index]
            );
        }
    }
}

#[cfg(not(feature = "profiler"))]
#[inline]
pub(crate) fn append_mailbox_loss_series(_out: &mut String, _series: &[&str; 4]) {}

pub(crate) struct RemoteActorObservation {
    pub(crate) pending: Option<(u64, Arc<crate::hew_node::PendingReply>)>,
    pub(crate) timer: *mut crate::coro_sleep::HewCoroSleep,
    pub(crate) remote: Option<(
        crate::node_identity::Location,
        crate::monitor::ObservationAuthority,
    )>,
    pub(crate) policy: u8,
    pub(crate) completion: Option<Arc<crate::actor_native::NativeActorCompletion>>,
}

impl RemoteActorObservation {
    pub(crate) const EMPTY: Self = Self {
        pending: None,
        timer: std::ptr::null_mut(),
        remote: None,
        policy: 0,
        completion: None,
    };
}

#[inline]
pub(crate) fn new_remote_actor_observation() -> RemoteActorObservation {
    RemoteActorObservation {
        policy: crate::execution_context::current_partition_policy() as u8,
        ..RemoteActorObservation::EMPTY
    }
}

#[expect(
    clippy::unnecessary_wraps,
    reason = "cross-platform seam returns Option so wasm32 can report unsupported"
)]
#[inline]
pub(crate) fn poll_imported_observation(
    obs: &mut crate::actor_observe::HewNativeActorObservation,
) -> Option<bool> {
    Some(crate::hew_node::monitor_link::poll_imported_observation(
        obs,
    ))
}

#[inline]
pub(crate) fn retire_imported_observation(
    obs: &mut crate::actor_observe::HewNativeActorObservation,
) -> bool {
    crate::hew_node::monitor_link::retire_imported_observation(obs)
}

#[inline]
pub(crate) unsafe fn try_start_imported_observation(
    obs: &mut crate::actor_observe::HewNativeActorObservation,
    token: HewLocalPidId,
    waker: *const HewWaker,
) -> bool {
    let Some(imported) = crate::lifetime::local_handles::current_imported_actor(token) else {
        return false;
    };
    // SAFETY: forwarded live wake descriptor contract.
    unsafe {
        crate::hew_node::monitor_link::start_imported_observation(obs, &imported, waker);
    }
    true
}

#[derive(Debug)]
pub(crate) struct ImportedActorCall {
    operation: *mut crate::hew_node::HewRemoteCall,
    request: *mut crate::mailbox::HewMsgEnvelope,
}

impl ImportedActorCall {
    /// # Safety
    /// Caller guarantees serialized storage visitation outside concurrent polls.
    #[inline]
    pub(crate) unsafe fn visit_managed(
        &self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        if self.request.is_null() {
            0
        } else {
            // SAFETY: request is a live CoW envelope retained until cleanup.
            unsafe { crate::cow_envelope::visit_managed(self.request.cast(), visitor) }
        }
    }

    #[inline]
    pub(crate) fn failure_snapshot(&self) -> Option<Arc<crate::failure_snapshot::FailureSnapshot>> {
        // SAFETY: this call owns its live remote operation.
        unsafe { self.operation.as_ref() }.and_then(|call| call.failure.clone())
    }

    /// # Safety
    /// Caller owns exclusive cleanup transition for this call.
    #[inline]
    pub(crate) unsafe fn prepare_cleanup(&mut self) -> *mut crate::mailbox::HewMsgEnvelope {
        // SAFETY: `self.operation` is owned by this route and freed at most once.
        unsafe {
            crate::hew_node::hew_remote_call_free(std::mem::replace(
                &mut self.operation,
                std::ptr::null_mut(),
            ));
        }
        std::mem::replace(&mut self.request, std::ptr::null_mut())
    }

    #[inline]
    pub(crate) fn has_deadline(&self) -> bool {
        // SAFETY: this call owns its optional live remote operation.
        unsafe { self.operation.as_ref() }.is_some_and(crate::hew_node::HewRemoteCall::has_deadline)
    }

    /// # Safety
    /// This call exclusively drives the imported operation.
    #[inline]
    pub(crate) unsafe fn poll(&self) -> i32 {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::hew_node::hew_remote_call_poll(self.operation) }
    }

    /// # Safety
    /// The imported operation is ready and `output` holds caller reply storage.
    #[inline]
    pub(crate) unsafe fn take_outcome(
        &self,
        output: *mut std::ffi::c_void,
    ) -> crate::request_state::RequestOutcome {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::hew_node::take_imported_request(self.operation, output) }
    }

    #[inline]
    pub(crate) fn detach_request(&mut self) -> *mut crate::mailbox::HewMsgEnvelope {
        std::mem::replace(&mut self.request, std::ptr::null_mut())
    }
}

#[expect(
    clippy::too_many_arguments,
    reason = "forwarding checked call ABI parameters without intermediate allocation"
)]
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
    let service = crate::lifetime::local_handles::current_imported_actor(token)?;
    // SAFETY: caller transfers `envelope` and borrows `waker` across initiation.
    let operation = unsafe {
        let (payload, size) = envelope
            .as_ref()
            .map_or((std::ptr::null_mut(), 0), |envelope| {
                (envelope.payload, envelope.payload_size)
            });
        crate::hew_node::service_api::start_imported_actor_call(
            &service,
            dispatch,
            message,
            payload,
            size,
            reply_size,
            deadline_ns,
            waker,
            kind,
        )
    };
    Some(ImportedActorCall {
        operation,
        request: envelope,
    })
}

pub(crate) struct ImportedActorBinding {
    references: usize,
    service: Arc<crate::hew_node::service_api::ImportedService>,
}

pub(crate) struct DroppedImportedActor(
    #[expect(dead_code, reason = "dropped outside the handle lock")] Option<ImportedActorBinding>,
);

#[derive(Default)]
pub(crate) struct ImportedActorRegistry {
    bindings: std::collections::HashMap<HewLocalPidId, ImportedActorBinding>,
}

impl ImportedActorRegistry {
    #[inline]
    pub(crate) fn insert(
        &mut self,
        token: HewLocalPidId,
        service: Arc<crate::hew_node::service_api::ImportedService>,
    ) {
        self.bindings.insert(
            token,
            ImportedActorBinding {
                references: 1,
                service,
            },
        );
    }

    #[inline]
    pub(crate) fn clone_service(
        &self,
        token: HewLocalPidId,
    ) -> Option<Arc<crate::hew_node::service_api::ImportedService>> {
        self.bindings
            .get(&token)
            .map(|binding| Arc::clone(&binding.service))
    }

    #[inline]
    pub(crate) fn retain(&mut self, token: HewLocalPidId) -> bool {
        let Some(binding) = self.bindings.get_mut(&token) else {
            return false;
        };
        binding.references = binding
            .references
            .checked_add(1)
            .expect("actor descriptor reference count overflow");
        true
    }

    #[inline]
    pub(crate) fn release(&mut self, token: HewLocalPidId) -> DroppedImportedActor {
        let remove = self.bindings.get_mut(&token).and_then(|binding| {
            binding.references -= 1;
            (binding.references == 0).then_some(())
        });
        DroppedImportedActor(remove.and_then(|()| self.bindings.remove(&token)))
    }
}

unsafe extern "C" fn run_offload_job(context: *mut std::ffi::c_void) {
    use crate::async_io::offload::OffloadEnv;
    use crate::async_io::{IoProducer, IoValue};

    // SAFETY: a successful submit transfers this unique Box to one callback.
    let (operation, mut env) =
        *unsafe { Box::from_raw(context.cast::<(IoProducer, OffloadEnv)>()) };
    if !operation.is_pending() {
        return;
    }
    if let Some(detach) = crate::blocking_pool::Detach::current() {
        operation.produced_by(detach);
    }
    env.run();
    crate::blocking_pool::job_finishing();
    operation.complete(Ok(IoValue::Offload(env)));
}

#[inline]
pub(crate) fn try_submit_offload_job(
    operation: &Arc<crate::async_io::HewAsyncIo>,
    env: crate::async_io::offload::OffloadEnv,
) -> Result<(), Option<crate::async_io::offload::OffloadEnv>> {
    let Some(pool) = crate::blocking_pool::shared_blocking_pool_opt() else {
        return Err(Some(env));
    };
    let job = Box::new((crate::async_io::IoProducer::new(Arc::clone(operation)), env));
    // SAFETY: the pool is installed; the job is owned until its callback.
    match unsafe { crate::blocking_pool::submit_abandonable_owned(pool, run_offload_job, job) } {
        Ok(()) => Ok(()),
        Err(job) => {
            drop(job);
            operation.complete(Err(crate::async_io::IoFailure::invalid(
                "offload blocking pool could not accept work",
            )));
            Err(None)
        }
    }
}

#[inline]
pub(crate) unsafe fn start_async_sink_finish(
    sink_ref: &crate::stream::HewSink,
    operation: &Arc<crate::async_io::HewAsyncIo>,
    waker: *const HewWaker,
) -> bool {
    use crate::util::MutexExt as _;
    // SAFETY: caller's loan survives mandatory finish drain.
    let Some(pending) = (unsafe { sink_ref.begin_finish() }) else {
        return false;
    };
    if !pending.needs_close() {
        pending.finish();
        operation.complete(Ok(crate::async_io::IoValue::Count(0)));
        return true;
    }
    // SAFETY: the pending backing pins its exact adapter allocation.
    let owner = (!pending.adapter().is_null())
        .then(|| unsafe { crate::stream::sender::retain_adapter(pending.adapter()) });
    // SAFETY: the borrowed descriptor is retained until native finish cleanup completes.
    let waker = Arc::new(unsafe { crate::wake::OwnedWaker::retain(&*waker) });
    if let Some(owner) = &owner {
        owner.observe(&waker);
    }
    *operation.native.sink_finish.lock_or_recover() =
        Some(crate::async_io::sink::Finish::new(pending, owner, waker));
    true
}

#[inline]
pub(crate) unsafe fn async_sink_cleanup_fault_message(
    operation: *const crate::async_io::HewAsyncIo,
) -> Option<String> {
    // SAFETY: forwarded from caller contract.
    unsafe { crate::async_io::failure_message(operation) }
}

#[inline]
pub(crate) unsafe fn start_stdin_read_line(
    waker: *const HewWaker,
) -> *const crate::async_io::HewAsyncIo {
    // SAFETY: forwarded from caller contract.
    unsafe { crate::async_io::stdin::start_native_read_line(waker) }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct HostRuntimeLease(*mut crate::runtime_handle::HewRuntime);

impl HostRuntimeLease {
    pub(crate) const NULL: Self = Self(std::ptr::null_mut());

    #[inline]
    pub(crate) fn from_raw(runtime: *mut crate::runtime_handle::HewRuntime) -> Self {
        Self(runtime)
    }

    /// # Safety
    /// Caller owns the final retained reference transferred into this lease.
    #[inline]
    pub(crate) unsafe fn release(self) {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::runtime_handle::hew_runtime_release(self.0) };
    }
}

// SAFETY: only released on teardown after exclusive call ownership finishes.
unsafe impl Send for HostRuntimeLease {}

#[inline]
pub(crate) unsafe fn unregister_actor_names(actor_id: u64) {
    // SAFETY: forwarded from caller contract.
    unsafe { crate::hew_node::unregister_actor_names(actor_id) };
}

#[derive(Clone, Copy)]
enum ReadYield {
    Content,
    Connection,
    Readiness,
}

#[derive(Clone, Copy)]
pub(crate) struct StreamAsyncIo(*const crate::async_io::HewAsyncIo);

// SAFETY: only accessed under `HewNativeStream`'s exclusive poll/cleanup/drop contract.
unsafe impl Send for StreamAsyncIo {}
// SAFETY: concurrent observers only access `HewNativeStream`'s immutable wake header.
unsafe impl Sync for StreamAsyncIo {}

impl StreamAsyncIo {
    /// # Safety
    /// Operation retains a live creator-owned reference through this walk.
    #[inline]
    pub(crate) unsafe fn visit_stream_content(
        &self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: forwarded from caller contract.
        unsafe { (*self.0).visit_stream_content(visitor) }
    }

    /// # Safety
    /// Caller owns the final creator reference returned by start.
    #[inline]
    pub(crate) unsafe fn free(self) {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::async_io::hew_async_io_free(self.0) };
    }

    /// # Safety
    /// Caller owns a live exclusive operation reference.
    #[inline]
    pub(crate) unsafe fn failure_trap(&self) -> Option<i32> {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::async_io::failure_trap(self.0) }
    }

    /// # Safety
    /// Caller owns a live exclusive operation reference.
    #[inline]
    pub(crate) unsafe fn failure_message(&self) -> Option<String> {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::async_io::failure_message(self.0) }
    }

    /// # Safety
    /// Caller owns a live exclusive write operation reference.
    #[inline]
    pub(crate) unsafe fn poll_write(&self) -> i32 {
        // SAFETY: forwarded from caller contract.
        match unsafe { crate::async_io::hew_async_io_status(self.0) } {
            0 => 0,
            1 => 1,
            // SAFETY: status answered Error for this live request.
            _ => match unsafe { crate::async_io::write_failure(self.0) } {
                crate::async_io::WriteFailure::Closed => 2,
                crate::async_io::WriteFailure::TimedOut(_) => 4,
                crate::async_io::WriteFailure::Other => 3,
            },
        }
    }

    /// # Safety
    /// Caller owns a live exclusive write operation whose last poll reported timeout.
    #[inline]
    pub(crate) unsafe fn timed_out_committed(&self, write_prefix: usize) -> i64 {
        // SAFETY: forwarded from caller contract.
        match unsafe { crate::async_io::write_failure(self.0) } {
            crate::async_io::WriteFailure::TimedOut(committed) => committed
                .saturating_sub(i64::try_from(write_prefix).unwrap_or(i64::MAX))
                .max(0),
            _ => 0,
        }
    }

    /// # Safety
    /// Caller owns a live exclusive operation reference.
    #[inline]
    pub(crate) unsafe fn cancel(&self) {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::async_io::hew_async_io_cancel(self.0) };
    }

    /// # Safety
    /// Caller owns a live exclusive operation reference and `waker` remains live.
    #[inline]
    pub(crate) unsafe fn cleanup_status(&self, waker: *const HewWaker) -> i32 {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::async_io::hew_async_io_cleanup_status(self.0, waker) }
    }
}

pub(crate) struct StreamImmediate {
    status: i32,
    fault: *mut crate::fault::HewFault,
    release: *mut crate::release_walker::HewReleaseCursor,
}

// SAFETY: only accessed under `HewNativeStream`'s exclusive poll/cleanup/drop contract.
unsafe impl Send for StreamImmediate {}
// SAFETY: concurrent observers only access `HewNativeStream`'s immutable wake header.
unsafe impl Sync for StreamImmediate {}

impl StreamImmediate {
    #[inline]
    pub(crate) fn status(&self) -> i32 {
        self.status
    }

    #[inline]
    pub(crate) fn take_fault(&mut self) -> *mut crate::fault::HewFault {
        std::mem::replace(&mut self.fault, std::ptr::null_mut())
    }

    /// # Safety
    /// `visitor` must be valid per [`crate::release_walker::visit_cursor`].
    #[inline]
    pub(crate) unsafe fn visit_release(
        &self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: forwarded from caller contract.
        unsafe { crate::release_walker::visit_cursor(self.release, visitor, true) }
    }

    /// # Safety
    /// Operation must already be quiescent before releasing immediate fault and cursor.
    #[inline]
    pub(crate) unsafe fn release_begin(&mut self) -> *mut crate::release_walker::HewReleaseCursor {
        // SAFETY: an unobserved terminal fault remains owned by this operation.
        unsafe {
            crate::fault::hew_fault_drop(std::mem::replace(&mut self.fault, std::ptr::null_mut()));
        }
        std::mem::replace(&mut self.release, std::ptr::null_mut())
    }
}

pub(crate) struct StreamIoFields {
    yields: ReadYield,
    write_prefix: usize,
    content: Option<crate::async_io::content::ContentView>,
    read_content: Option<Arc<crate::async_io::content::Content>>,
}

// SAFETY: only accessed under `HewNativeStream`'s exclusive poll/cleanup/drop contract.
unsafe impl Send for StreamIoFields {}
// SAFETY: concurrent observers only access `HewNativeStream`'s immutable wake header.
unsafe impl Sync for StreamIoFields {}

impl StreamIoFields {
    pub(crate) const EMPTY: Self = Self {
        yields: ReadYield::Content,
        write_prefix: 0,
        content: None,
        read_content: None,
    };

    #[inline]
    pub(crate) fn write_prefix(&self) -> usize {
        self.write_prefix
    }

    #[inline]
    pub(crate) fn has_write_content(&self) -> bool {
        self.content.is_some()
    }

    /// # Safety
    /// `visitor` must be a valid managed storage visitor and state access must be exclusive.
    #[inline]
    pub(crate) unsafe fn visit(
        &mut self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        if let Some(content) = &self.read_content {
            // SAFETY: forwarded from caller contract.
            let status = unsafe { content.visit(visitor) };
            if status != 0 {
                return status;
            }
        }
        if let Some(content) = &mut self.content {
            // SAFETY: forwarded from caller contract.
            let status = unsafe { content.visit(visitor) };
            if status != 0 {
                return status;
            }
        }
        0
    }

    /// # Safety
    /// Operation retains this live `StreamAsyncIo` request and exclusive state access.
    #[inline]
    pub(crate) unsafe fn poll_io(
        &mut self,
        io: StreamAsyncIo,
        layout_size: usize,
    ) -> (i32, Option<Vec<u8>>) {
        // SAFETY: the operation owns this live async request.
        unsafe {
            match self.yields {
                ReadYield::Content => {
                    let (status, content) = crate::async_io::take_managed_stream_item(io.0);
                    self.read_content = content;
                    (status, None)
                }
                ReadYield::Connection => crate::async_io::take_accepted_item(io.0, layout_size),
                ReadYield::Readiness => crate::async_io::take_readiness_item(io.0),
            }
        }
    }

    /// # Safety
    /// Caller holds exclusive operation access; `out` is a writable slot for `layout`.
    #[inline]
    pub(crate) unsafe fn take_read_content(
        &mut self,
        layout: &HewValueLayout,
        out: *mut c_void,
        fault: &mut *mut crate::fault::HewFault,
    ) -> Option<i32> {
        let content = self.read_content.as_ref()?;
        let owner = content.owner();
        let Ok(reservation) = owner
            .as_ref()
            .map(|owner| owner.reserve_new(content.len() as u64))
            .transpose()
        else {
            *fault = crate::fault::hew_fault_new(crate::internal::types::HEW_TRAP_HEAP_EXCEEDED);
            return Some(3);
        };
        // SAFETY: the temporary content image is decoded into the caller's unpublished slot.
        let status = unsafe {
            crate::channel_common::decode_elem_envelope(
                Some(content.to_vec()),
                out,
                layout,
                "native stream read",
            )
        };
        if status == 1 {
            if let Some(reservation) = reservation {
                // SAFETY: decoding initialized `out` with the descriptor used for this reservation.
                if let Err(failure) = unsafe { reservation.commit_value(layout, out) } {
                    // SAFETY: typed source is initialized; its charge was released before destruction.
                    crate::owned_value_budget::with_value_owner(&None, || unsafe {
                        layout.drop_fn.expect("content destructor")(out);
                    });
                    *fault = crate::fault::hew_fault_new(
                        if failure == hew_cabi::managed::HEAP_EXCEEDED {
                            crate::internal::types::HEW_TRAP_HEAP_EXCEEDED
                        } else {
                            crate::internal::types::HEW_TRAP_USER_PANIC
                        },
                    );
                    return Some(3);
                }
            }
            self.read_content = None;
        }
        Some(status)
    }
}

/// A connection item is its handle's slot image.
fn connection_layout(layout: &HewValueLayout) {
    if !matches!(layout.size, 4 | 8) {
        crate::channel_common::abort_elem_witness(
            "native incoming stream",
            "an accepted connection needs a handle-sized witness",
        );
    }
}

/// A readiness item is `()`.
fn unit_layout(layout: &HewValueLayout) {
    if layout.size != 0 || layout.ownership_kind != HewTypeOwnershipKind::Plain {
        crate::channel_common::abort_elem_witness(
            "native readiness stream",
            "a readiness item is `()`",
        );
    }
}

fn content_layout(layout: &HewValueLayout) {
    if !matches!(
        layout.ownership_kind,
        HewTypeOwnershipKind::String | HewTypeOwnershipKind::Bytes
    ) {
        crate::channel_common::abort_elem_witness(
            "native content stream",
            "content backing requires a string or bytes witness",
        );
    }
}

/// # Safety
/// `stream` must be an exclusively borrowed [`crate::stream::HewStream`],
/// `parent` null or live for construction, and `waker` a live borrowed descriptor.
#[expect(
    clippy::unnecessary_wraps,
    reason = "cross-platform seam keeps Option so wasm32 can diverge without triggering unreachable_code"
)]
pub(crate) unsafe fn try_start_stream_reactor_read(
    stream: *mut crate::stream::HewStream,
    layout: HewValueLayout,
    parent: *mut crate::coro_state::HewCoroState,
    waker: *const HewWaker,
) -> Option<(StreamAsyncIo, StreamIoFields)> {
    let mut yields = ReadYield::Content;
    // SAFETY: the exclusive stream loan remains live during inspection;
    // generated cleanup retains it through producer quiescence.
    let io = unsafe {
        match (*stream).native_read() {
            Some(crate::stream::NativeRead::Content(handle)) => {
                content_layout(&layout);
                crate::async_io::start_tcp_stream_read(
                    handle,
                    waker,
                    crate::owned_value_budget::materialize::invocation_owner(parent),
                )
            }
            Some(crate::stream::NativeRead::Accept(listener)) => {
                connection_layout(&layout);
                yields = ReadYield::Connection;
                crate::async_io::hew_async_tcp_accept(listener, waker)
            }
            Some(crate::stream::NativeRead::Readiness(handle)) => {
                unit_layout(&layout);
                yields = ReadYield::Readiness;
                crate::async_io::start_readiness_watch(handle, waker)
            }
            None => {
                content_layout(&layout);
                crate::async_io::start_stream_read(
                    stream,
                    waker,
                    crate::owned_value_budget::materialize::invocation_owner(parent),
                )
            }
        }
    };
    Some((
        StreamAsyncIo(io),
        StreamIoFields {
            yields,
            ..StreamIoFields::EMPTY
        },
    ))
}

pub(crate) unsafe fn try_nonblocking_stream_write(
    sink: *mut crate::stream::HewSink,
    data: *mut c_void,
    layout: HewValueLayout,
    parent: *const crate::coro_state::HewCoroState,
) -> Result<(StreamImmediate, StreamIoFields), i32> {
    let release = std::ptr::null_mut();
    let mut fault = std::ptr::null_mut();
    content_layout(&layout);
    // SAFETY: data still holds an initialized value matching the checked content descriptor.
    let bytes = unsafe {
        crate::channel_common::encode_elem_envelope(data, &layout, "native content try write")
    };
    // SAFETY: the optional parent invocation stays live until this constructor returns.
    let owner = unsafe { crate::owned_value_budget::materialize::invocation_owner(parent) };
    let content = crate::async_io::content::Content::new(bytes, owner.clone())?;
    let view = crate::async_io::content::ContentView::new(&content);
    if let Some(owner) = &owner {
        // SAFETY: the source still owns its typed backing; the replacement content is already reserved.
        unsafe { owner.release_value(&layout, data) }?;
    }
    // SAFETY: the typed source is initialized; its charge was released before this physical destruction.
    crate::owned_value_budget::with_value_owner(&None, || unsafe {
        layout.drop_fn.expect("validated content destructor")(data);
    });
    let status = if sink.is_null() {
        2
    } else {
        // SAFETY: the sender gate retains this live sink through the synchronous capacity attempt.
        unsafe { (*sink).try_write_owned(hew_cabi::sink::PendingBytes::retained(content)) }
            .into_abi_code()
    };
    if status == 3 {
        fault = crate::stream::hew_stream_take_error_fault();
    }
    let status = match status {
        0 => 1,
        1 => 2,
        2 => 4,
        3 => 3,
        _ => std::process::abort(),
    };
    Ok((
        StreamImmediate {
            status,
            fault,
            release,
        },
        StreamIoFields {
            content: Some(view),
            ..StreamIoFields::EMPTY
        },
    ))
}

pub(crate) unsafe fn try_start_sink_reactor_write(
    sink: *mut crate::stream::HewSink,
    waker: *const HewWaker,
    data: *mut c_void,
    layout: HewValueLayout,
    parent: *const crate::coro_state::HewCoroState,
) -> Option<Result<(StreamAsyncIo, StreamIoFields), i32>> {
    // SAFETY: the sender gate retains the exclusive endpoint loan.
    if unsafe { (*sink).is_closed() || !(*sink).channel_core_ptr().is_null() } {
        return None;
    }
    Some((|| {
        content_layout(&layout);
        // Inspect both sources before changing either; quota refusal preserves them.
        // SAFETY: the sender gate retains the live sink and serializes pending-content access.
        let mut bytes = unsafe { (*sink).pending_content() };
        let write_prefix = bytes.len();
        // SAFETY: data still holds an initialized value matching the checked content descriptor.
        let envelope = unsafe {
            crate::channel_common::encode_elem_envelope(data, &layout, "native content write")
        };
        bytes.extend_from_slice(&envelope);
        let value_owner =
            // SAFETY: the optional parent invocation stays live until this constructor returns.
            unsafe { crate::owned_value_budget::materialize::invocation_owner(parent) };
        let content = crate::async_io::content::Content::new(bytes, value_owner.clone())?;
        let content_view = crate::async_io::content::ContentView::new(&content);
        if let Some(owner) = &value_owner {
            // SAFETY: content owns the replacement allocation; the source still retains its typed graph.
            unsafe { owner.release_value(&layout, data) }?;
        }
        // SAFETY: the admitted buffer now owns all content and the source is consumed once.
        crate::owned_value_budget::with_value_owner(&None, || unsafe {
            layout.drop_fn.expect("validated content destructor")(data);
        });
        // SAFETY: the sender gate retains the live sink and serializes pending-content access.
        drop(unsafe { (*sink).take_pending() });
        // SAFETY: the sender gate retains the live sink and serializes pending-content access.
        let io = if let Some(connection) = unsafe { (*sink).native_connection() } {
            // SAFETY: the operation retains the endpoint loan and borrowed waker until producer quiescence.
            unsafe { crate::async_io::start_tcp_stream_write(connection, content, waker) }
        } else {
            // SAFETY: the operation retains the endpoint loan and borrowed waker until producer quiescence.
            unsafe { crate::async_io::start_sink_write(sink, content, waker) }
        };
        Ok((
            StreamAsyncIo(io),
            StreamIoFields {
                yields: ReadYield::Content,
                write_prefix,
                content: Some(content_view),
                read_content: None,
            },
        ))
    })())
}

#[inline]
pub(crate) fn missing_stream_pipe_backing(_operation: &str) -> ! {
    std::process::abort()
}
