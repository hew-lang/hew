//! Executor, wake, runtime-state resolution, and shutdown-ingress seam.

use super::imp;
use crate::activation::SchedulerQueueEntry;
use crate::lifetime::local_handles::{LocalHandles, SupervisorTeardownLease};
use crate::metrics::MetricsState;
use crate::monitor::MonitorState;
use crate::runtime_id::RuntimeId;
use crate::supervisor::roots::SupervisorPtr;
use std::sync::MutexGuard;

/// Opaque per-actor/per-supervisor handle to the owning native runtime.
///
/// On native targets this wraps `*const RuntimeInner` (`#[repr(transparent)]`);
/// on `wasm32` it is zero-sized because only a single cooperative runtime exists.
pub(crate) type RuntimeHandle = imp::executor::RuntimeHandle;

/// Guard keeping a borrowed runtime entered on the current thread on native targets.
pub(crate) type RuntimeContextGuard = imp::executor::RuntimeContextGuard;

/// Guard keeping external work from racing teardown publication fences.
pub(crate) type ExternalIngressPermit = imp::executor::ExternalIngressPermit;

/// Return the owning runtime's identifier, or `RuntimeId::DEFAULT` on `wasm32`.
#[inline]
pub(crate) fn current_runtime_id_or_default() -> RuntimeId {
    imp::executor::current_runtime_id_or_default()
}

/// Return the current runtime identifier when a runtime is installed.
#[inline]
pub(crate) fn current_runtime_id() -> Option<RuntimeId> {
    imp::executor::current_runtime_id()
}

/// Resolve the installed runtime's local-handle authority, if present.
#[inline]
pub(crate) fn current_handles() -> Option<&'static LocalHandles> {
    imp::executor::current_handles()
}

/// Run `f` against the local-handle authority owned by `runtime` (or the active
/// single-runtime authority on `wasm32`).
///
/// # Safety
/// On native targets, `runtime` must be valid for the duration of `f`.
#[inline]
pub(crate) unsafe fn with_actor_runtime_handles<R>(
    runtime: RuntimeHandle,
    f: impl FnOnce(Option<&LocalHandles>) -> R,
) -> R {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::executor::with_actor_runtime_handles(runtime, f) }
}

/// Retire a tracked live actor's local route and clear unreferenced terminal
/// monitor metadata under its owning runtime authority.
///
/// # Safety
/// `runtime`, on native targets, must point to the live runtime that spawned
/// this tracked actor.
#[inline]
pub(crate) unsafe fn retire_tracked_actor_route(
    runtime: RuntimeHandle,
    token: crate::lifetime::local_handles::HewLocalPidId,
    actor_id: u64,
) {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::executor::retire_tracked_actor_route(runtime, token, actor_id) }
}

/// Resolve the active runtime's monitor table, panicking on native targets if
/// no runtime is installed.
#[inline]
pub(crate) fn current_monitors() -> &'static MonitorState {
    imp::executor::current_monitors()
}

/// Resolve the active runtime's monitor table, returning `None` when no native
/// runtime is installed.
#[inline]
pub(crate) fn current_monitors_opt() -> Option<&'static MonitorState> {
    imp::executor::current_monitors_opt()
}

/// Resolve the active runtime's metric state, panicking on native targets if no
/// runtime is installed.
#[inline]
pub(crate) fn current_metrics() -> &'static MetricsState {
    imp::executor::current_metrics()
}

/// Resolve the active runtime's metric state, returning `None` when no native
/// runtime is installed.
#[inline]
pub(crate) fn current_metrics_opt() -> Option<&'static MetricsState> {
    imp::executor::current_metrics_opt()
}

/// Operate on the installed runtime's registered top-level supervisors.
#[inline]
pub(crate) fn with_supervisor_roots<R>(f: impl FnOnce(&mut Vec<SupervisorPtr>) -> R) -> R {
    imp::executor::with_supervisor_roots(f)
}

/// Acquire an external-work ingress permit unless runtime teardown is closed.
#[inline]
pub(crate) fn admit_external_work() -> Result<ExternalIngressPermit, ()> {
    imp::executor::admit_external_work()
}

/// Publish a resumed queue entry to either the active cooperative driver or the
/// native scheduler.
#[inline]
pub(crate) fn publish_queue_entry(entry: SchedulerQueueEntry) {
    imp::executor::publish_queue_entry(entry);
}

/// Publish a running actor's own requeue at the FIFO tail of the active
/// cooperative driver or the native scheduler.
#[inline]
pub(crate) fn publish_requeue_entry(entry: SchedulerQueueEntry) {
    imp::executor::publish_requeue_entry(entry);
}

/// Return whether the active scheduler or cooperative driver has queued work.
#[inline]
pub(crate) fn has_runnable_work() -> bool {
    imp::executor::has_runnable_work()
}

/// Return whether synchronous waits should abandon due to shutdown drain.
#[inline]
pub(crate) fn send_abandoned_by_shutdown() -> bool {
    imp::executor::send_abandoned_by_shutdown()
}

/// Return whether supervisor teardown observed an external shutdown request.
#[inline]
pub(crate) fn shutdown_requested() -> bool {
    imp::executor::shutdown_requested()
}

/// Return whether actor progress remains possible during teardown quiescence.
#[inline]
pub(crate) fn actor_progress_available() -> bool {
    imp::executor::actor_progress_available()
}

/// Perform a bounded adaptive spin wait when supported by the native executor.
#[inline]
pub(crate) fn spin_waiter(ready: impl FnMut() -> bool) -> bool {
    imp::executor::spin_waiter(ready)
}

/// Publish crash-cleanup drain activity to the native hardware signal handler.
#[inline]
pub(crate) fn publish_crash_cleanup_drain_to_signal_handler(active: bool) {
    imp::executor::publish_crash_cleanup_drain_to_signal_handler(active);
}

/// Resolve the local PID routing slot for the current runtime.
#[inline]
pub(crate) fn local_route_slot() -> u16 {
    imp::executor::local_route_slot()
}

/// Free a batch of retired actors via a tracked background teardown thread on
/// native targets or the cooperative driver on `wasm32`.
#[inline]
pub(crate) fn defer_actor_free(actors: Vec<*mut crate::actor::HewActor>) {
    imp::executor::defer_actor_free(actors);
}

/// Defer freeing a single self-stopping actor to a background teardown thread on
/// native targets or the cooperative driver on `wasm32`.
#[inline]
pub(crate) fn defer_single_actor_free(actor: *mut crate::actor::HewActor) -> std::ffi::c_int {
    imp::executor::defer_single_actor_free(actor)
}

/// Enter a handle's runtime on the current thread on native targets.
///
/// # Safety
/// On native targets, `runtime` must name a live runtime for the lifetime of the
/// returned guard.
#[inline]
pub(crate) unsafe fn enter_runtime(runtime: RuntimeHandle) -> RuntimeContextGuard {
    // SAFETY: forwarded from caller contract.
    unsafe { runtime.enter() }
}

/// Forward a supervisor stop request either onto the cooperative driver or a
/// tracked teardown thread.
#[inline]
pub(crate) fn schedule_deferred_supervisor_stop(
    sup: *mut crate::supervisor::HewSupervisor,
    teardown: SupervisorTeardownLease,
    work: impl FnOnce(*mut crate::supervisor::HewSupervisor, SupervisorTeardownLease) + Send + 'static,
) -> bool {
    imp::executor::schedule_deferred_supervisor_stop(sup, teardown, work)
}

/// Ensure the background output drainer thread is started when under a native runtime.
#[inline]
pub(crate) fn output_drainer_running(drain_entry: fn()) -> bool {
    imp::executor::output_drainer_running(drain_entry)
}

/// Wake the native background output drainer thread after queuing a chunk.
#[inline]
pub(crate) fn notify_output_drainer() {
    imp::executor::notify_output_drainer();
}

/// Wait on the native output drainer work condition variable.
#[inline]
pub(crate) fn wait_output_work<T>(guard: MutexGuard<'_, T>) -> MutexGuard<'_, T> {
    imp::executor::wait_output_work(guard)
}

/// Abort or trap on a bounds/precondition failure through the target trap bridge.
///
/// # Safety
/// Must be called only from a fail-closed bounds/precondition trap path after
/// any diagnostic has been prepared.
#[cfg_attr(
    not(any(test, target_arch = "wasm32")),
    allow(
        dead_code,
        reason = "call sites are wired in subsequent runtime stages"
    )
)]
#[inline]
pub(crate) unsafe fn bounds_trap(code: std::ffi::c_int) -> ! {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::executor::bounds_trap(code) }
}

/// Bridge an unrecovered runtime fault through the target supervisor or WASI
/// exit handler.
///
/// # Safety
/// May be called in or out of actor dispatch when the current execution context,
/// if any, is valid.
#[inline]
pub(crate) unsafe fn fault_trap_bridge(code: std::ffi::c_int, reported: bool) {
    // SAFETY: forwarded from caller contract.
    unsafe { imp::executor::fault_trap_bridge(code, reported) }
}

/// Run every actor to a stop and reclaim the runtime on main return.
#[inline]
pub(crate) fn drain_to_quiescence() -> i32 {
    imp::executor::drain_to_quiescence()
}

/// Reserve kept before the soft stack watermark on the current target.
pub(crate) const STACK_SOFT_RESERVE: usize = imp::executor::STACK_SOFT_RESERVE;

/// Reserve kept before the hard stack watermark on the current target.
pub(crate) const STACK_HARD_RESERVE: usize = imp::executor::STACK_HARD_RESERVE;

/// Return the stack growth floor for the current thread or shadow stack.
#[inline]
pub(crate) fn current_thread_stack_floor() -> Option<usize> {
    imp::executor::current_thread_stack_floor()
}

/// Install root-thread crash signal handling on native targets.
#[inline]
pub(crate) fn init_root_crash_handling() {
    imp::executor::init_root_crash_handling();
}

/// Resolve the active runtime's live-actor registry, panicking if none is installed.
#[inline]
pub(crate) fn current_live_actors() -> &'static crate::lifetime::live_actors::LiveActors {
    imp::executor::current_live_actors()
}

/// Resolve the active runtime's live-actor registry when installed.
#[inline]
pub(crate) fn current_live_actors_opt() -> Option<&'static crate::lifetime::live_actors::LiveActors>
{
    imp::executor::current_live_actors_opt()
}

/// Return whether an actor belonging to `runtime_id` may be addressed on this thread.
#[inline]
pub(crate) fn actor_runtime_matches(actor_id: u64, runtime_id: RuntimeId) -> bool {
    imp::executor::actor_runtime_matches(actor_id, runtime_id)
}
