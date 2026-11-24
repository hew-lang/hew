//! Cooperative single-threaded `wasm32` executor backend.

use std::sync::{MutexGuard, OnceLock};

use crate::activation::SchedulerQueueEntry;
use crate::lifetime::local_handles::{LocalHandles, SupervisorTeardownLease};
use crate::lifetime::PoisonSafe;
use crate::metrics::MetricsState;
use crate::monitor::MonitorState;
use crate::runtime_id::RuntimeId;
use crate::supervisor::roots::SupervisorPtr;

static WASM_LOCAL_HANDLES: OnceLock<LocalHandles> = OnceLock::new();
static WASM_METRICS: MetricsState = MetricsState::new();
static WASM_MONITORS: OnceLock<MonitorState> = OnceLock::new();
static WASM_SUPERVISOR_ROOTS: PoisonSafe<Vec<SupervisorPtr>> = PoisonSafe::new(Vec::new());
static WASM_LIVE_ACTORS: crate::lifetime::live_actors::LiveActors =
    crate::lifetime::live_actors::LiveActors::new();

/// Zero-sized handle representing the single cooperative `wasm32` runtime.
#[derive(Clone, Copy, Debug)]
pub(crate) struct RuntimeHandle;

impl RuntimeHandle {
    #[cfg(any(test, feature = "composition-test"))]
    #[inline]
    pub(crate) const fn null() -> Self {
        Self
    }

    #[inline]
    pub(crate) fn current() -> Self {
        Self
    }

    /// No-op context entry on the single cooperative `wasm32` runtime.
    ///
    /// # Safety
    /// Satisfied trivially on single-runtime targets.
    #[inline]
    pub(crate) unsafe fn enter(self) -> RuntimeContextGuard {
        RuntimeContextGuard
    }
}

pub(crate) struct RuntimeContextGuard;

pub(crate) struct ExternalIngressPermit;

#[inline]
pub(crate) fn current_runtime_id_or_default() -> RuntimeId {
    RuntimeId::DEFAULT
}

#[inline]
pub(crate) fn current_runtime_id() -> Option<RuntimeId> {
    Some(RuntimeId::DEFAULT)
}

#[inline]
pub(crate) fn current_handles() -> Option<&'static LocalHandles> {
    Some(WASM_LOCAL_HANDLES.get_or_init(LocalHandles::new))
}

#[inline]
pub(crate) unsafe fn with_actor_runtime_handles<R>(
    _runtime: RuntimeHandle,
    f: impl FnOnce(Option<&LocalHandles>) -> R,
) -> R {
    f(current_handles())
}

pub(crate) unsafe fn retire_tracked_actor_route(
    _runtime: RuntimeHandle,
    token: crate::lifetime::local_handles::HewLocalPidId,
    actor_id: u64,
) {
    let retired = current_handles().map_or(
        crate::lifetime::local_handles::RetireActorResult::AlreadyRetired,
        |handles| handles.retire_actor(token, actor_id),
    );
    assert_ne!(
        retired,
        crate::lifetime::local_handles::RetireActorResult::Mismatch,
        "live actor route retirement lost exact authority"
    );
    crate::monitor::forget_unreferenced_actor_terminal(actor_id);
}

#[inline]
pub(crate) fn current_monitors() -> &'static MonitorState {
    WASM_MONITORS.get_or_init(MonitorState::new)
}

#[inline]
pub(crate) fn current_monitors_opt() -> Option<&'static MonitorState> {
    Some(current_monitors())
}

#[inline]
pub(crate) fn current_metrics() -> &'static MetricsState {
    &WASM_METRICS
}

#[inline]
pub(crate) fn current_metrics_opt() -> Option<&'static MetricsState> {
    Some(&WASM_METRICS)
}

#[inline]
pub(crate) fn with_supervisor_roots<R>(f: impl FnOnce(&mut Vec<SupervisorPtr>) -> R) -> R {
    WASM_SUPERVISOR_ROOTS.access(f)
}

#[inline]
pub(crate) fn admit_external_work() -> Result<ExternalIngressPermit, ()> {
    Ok(ExternalIngressPermit)
}

#[inline]
pub(crate) fn publish_queue_entry(entry: SchedulerQueueEntry) {
    if crate::driver::active() {
        crate::driver::publish_queue_entry(entry);
    }
}

#[inline]
pub(crate) fn has_runnable_work() -> bool {
    crate::driver::has_queued_work()
}

#[inline]
pub(crate) fn send_abandoned_by_shutdown() -> bool {
    false
}

#[inline]
pub(crate) fn shutdown_requested() -> bool {
    false
}

#[inline]
pub(crate) fn actor_progress_available() -> bool {
    true
}

#[inline]
pub(crate) fn spin_waiter(_ready: impl FnMut() -> bool) -> bool {
    false
}

#[inline]
pub(crate) fn publish_crash_cleanup_drain_to_signal_handler(_active: bool) {
    // wasm32 has no native signal module or hardware-fault boundary;
    // the thread-local drain-depth guard suffices for cooperative trap checks.
}

#[inline]
pub(crate) fn local_route_slot() -> u16 {
    0
}

#[inline]
pub(crate) fn defer_actor_free(actors: Vec<*mut crate::actor::HewActor>) {
    if !actors.is_empty() {
        crate::driver::publish_actor_free(actors);
    }
}

#[inline]
pub(crate) fn defer_single_actor_free(actor: *mut crate::actor::HewActor) -> std::ffi::c_int {
    crate::driver::publish_actor_free(vec![actor]);
    0
}

#[inline]
pub(crate) fn schedule_deferred_supervisor_stop(
    sup: *mut crate::supervisor::HewSupervisor,
    teardown: SupervisorTeardownLease,
    _work: impl FnOnce(*mut crate::supervisor::HewSupervisor, SupervisorTeardownLease) + Send + 'static,
) -> bool {
    crate::driver::publish_supervisor_stop(crate::supervisor::QueuedSupervisorStop {
        supervisor: sup,
        root_unregistered: false,
        teardown,
    });
    true
}

#[inline]
pub(crate) fn output_drainer_running(_drain_entry: fn()) -> bool {
    false
}

#[inline]
pub(crate) fn notify_output_drainer() {}

#[inline]
pub(crate) fn wait_output_work<T>(guard: MutexGuard<'_, T>) -> MutexGuard<'_, T> {
    guard
}

#[inline]
pub(crate) unsafe fn bounds_trap(code: std::ffi::c_int) -> ! {
    // SAFETY: the shared bridge reports and ends the run; it never returns
    // for a non-actor trap.
    unsafe { fault_trap_bridge(code, false) };
    std::process::abort();
}

#[inline]
pub(crate) unsafe fn fault_trap_bridge(code: std::ffi::c_int, reported: bool) {
    if !reported {
        crate::fault::report_trap_code(code);
    }
    crate::output::flush();
    crate::test_report::finish(1);
    // An unrecovered fault ends the run with status 1 (HEW-SPEC-2026 5.8);
    // the trap code in the reported line is the runtime's internal tag and
    // is never the process exit status.
    // JUSTIFIED: this raw trap ABI has no checked failure continuation.
    // Checked actor invocations return their fault before this boundary;
    // an unowned fallback ends the module and the host reclaims it.
    std::process::exit(1);
}

pub(crate) fn drain_to_quiescence() -> i32 {
    crate::driver::run_to_quiescence();
    if !crate::supervisor::roots::stop_registered_supervisors() {
        return 1;
    }
    // Root teardown can enqueue detached incarnation frees. Drain them
    // before selecting the remaining process-owned actors.
    crate::driver::run_to_quiescence();
    let actors = crate::lifetime::live_actors::snapshot_live_incarnations();
    for &target in &actors {
        crate::lifetime::live_actors::with_live_incarnation(target, |actor| {
            // SAFETY: the exact incarnation pin protects this stop request.
            unsafe { crate::actor::hew_actor_request_stop(actor.as_ptr()) };
        });
    }
    for target in actors {
        let actor = crate::lifetime::live_actors::with_live_incarnation(
            target,
            crate::lifetime::live_actors::ActorPin::as_ptr,
        );
        let Some(actor) = actor else { continue };
        // SAFETY: the single driver cannot retire this freshly resolved
        // actor before free takes over. Release the lookup pin first so
        // free can drain admitted pins; never retain another actor address
        // across the scheduler progress made by this call.
        if unsafe { crate::actor::hew_actor_free(actor) } != 0 {
            return 1;
        }
    }
    crate::driver::run_to_quiescence();
    crate::lifetime::local_handles::begin_current_shutdown();
    crate::lifetime::local_handles::close_current_supervisor_teardown_admission();
    if !crate::lifetime::local_handles::wait_for_current_supervisor_teardowns(
        std::time::Duration::from_secs(5),
    ) {
        return 1;
    }
    crate::lifetime::local_handles::assert_current_actor_routes_empty();
    crate::lifetime::local_handles::assert_current_supervisors_empty();
    0
}

pub(crate) const STACK_SOFT_RESERVE: usize = 64 * 1024;
pub(crate) const STACK_HARD_RESERVE: usize = 16 * 1024;

#[inline]
pub(crate) fn current_thread_stack_floor() -> Option<usize> {
    extern "C" {
        static __stack_low: u8;
    }
    // Only the symbol's address is taken; wasm-ld defines it.
    Some(std::ptr::addr_of!(__stack_low) as usize)
}

#[inline]
pub(crate) fn init_root_crash_handling() {}

#[inline]
pub(crate) fn current_live_actors() -> &'static crate::lifetime::live_actors::LiveActors {
    &WASM_LIVE_ACTORS
}

#[inline]
pub(crate) fn current_live_actors_opt() -> Option<&'static crate::lifetime::live_actors::LiveActors>
{
    Some(&WASM_LIVE_ACTORS)
}

#[inline]
pub(crate) fn actor_runtime_matches(_actor_id: u64, _runtime_id: RuntimeId) -> bool {
    true
}
