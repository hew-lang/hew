//! Native multithreaded executor, wake, and runtime-state backend.

use std::cell::Cell;
use std::sync::{Condvar, MutexGuard, OnceLock};

use crate::activation::SchedulerQueueEntry;
use crate::lifetime::local_handles::{LocalHandles, SupervisorTeardownLease};
use crate::metrics::MetricsState;
use crate::monitor::MonitorState;
use crate::runtime_id::RuntimeId;
use crate::scheduler::spin::{SpinBudget, DEFAULT_SPIN_LIMIT};
use crate::supervisor::roots::SupervisorPtr;
use crate::util::CondvarExt as _;

/// Signalled when the output queue gains a chunk for the drainer thread.
static OUTPUT_WORK: Condvar = Condvar::new();

thread_local! {
    /// Adaptive spin budget of the synchronous waiter on this thread.
    static WAITER_SPIN: Cell<SpinBudget> =
        const { Cell::new(SpinBudget::new(DEFAULT_SPIN_LIMIT)) };
}

/// Native handle pointing to an actor's or supervisor's owning `RuntimeInner`.
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub(crate) struct RuntimeHandle(*const crate::runtime::RuntimeInner);

impl RuntimeHandle {
    #[cfg(any(test, feature = "composition-test"))]
    #[inline]
    pub(crate) const fn null() -> Self {
        Self(std::ptr::null())
    }

    #[cfg(test)]
    #[inline]
    pub(crate) const fn from_raw(ptr: *const crate::runtime::RuntimeInner) -> Self {
        Self(ptr)
    }

    #[inline]
    pub(crate) fn current() -> Self {
        Self(std::ptr::from_ref(crate::runtime::rt_current()))
    }

    #[inline]
    pub(crate) fn as_raw(self) -> *const crate::runtime::RuntimeInner {
        self.0
    }

    /// Enter this handle's runtime on the current thread.
    ///
    /// # Safety
    /// `self` must point to a live [`crate::runtime::RuntimeInner`] for the
    /// lifetime of the returned guard.
    #[inline]
    pub(crate) unsafe fn enter(self) -> RuntimeContextGuard {
        // SAFETY: forwarded from caller contract.
        RuntimeContextGuard(unsafe { crate::runtime::enter(&*self.0) })
    }
}

pub(crate) struct RuntimeContextGuard(
    #[expect(
        dead_code,
        reason = "guard restores thread-local runtime selection on drop"
    )]
    crate::runtime::EnterGuard,
);

pub(crate) struct ExternalIngressPermit(
    #[expect(
        dead_code,
        reason = "ingress permit releases on drop after publication completes"
    )]
    crate::shutdown::IngressPermit<'static>,
);

#[inline]
pub(crate) fn current_runtime_id_or_default() -> RuntimeId {
    crate::runtime::rt_current().runtime_id()
}

#[inline]
pub(crate) fn current_runtime_id() -> Option<RuntimeId> {
    crate::runtime::rt_current_opt().map(crate::runtime::RuntimeInner::runtime_id)
}

#[inline]
pub(crate) fn current_handles() -> Option<&'static LocalHandles> {
    crate::runtime::rt_current_opt().map(|rt| &rt.local_handles)
}

#[inline]
pub(crate) unsafe fn with_actor_runtime_handles<R>(
    runtime: RuntimeHandle,
    f: impl FnOnce(Option<&LocalHandles>) -> R,
) -> R {
    // SAFETY: caller guarantees `runtime` is live when non-null.
    let handles = unsafe { runtime.as_raw().as_ref() }.map(|rt| &rt.local_handles);
    f(handles)
}

pub(crate) unsafe fn retire_tracked_actor_route(
    runtime: RuntimeHandle,
    token: crate::lifetime::local_handles::HewLocalPidId,
    actor_id: u64,
) {
    let owner = runtime.as_raw();
    assert!(
        !owner.is_null(),
        "tracked actor with a local handle has no owning runtime"
    );
    // SAFETY: `RuntimeInner` owns and outlives every tracked actor.
    let handles = unsafe { &(*owner).local_handles };
    let retired = handles.retire_actor(token, actor_id);
    assert_ne!(
        retired,
        crate::lifetime::local_handles::RetireActorResult::Mismatch,
        "live actor route retirement lost exact authority"
    );
    // The handle lock is released before taking a monitor shard. Terminal
    // publication takes the shard first and checks identity liveness there.
    // SAFETY: the owning runtime outlives this actor's retirement.
    unsafe { &(*owner).monitors }.forget_unreferenced_actor_terminal(actor_id, handles);
}

#[inline]
pub(crate) fn current_monitors() -> &'static MonitorState {
    &crate::runtime::rt_current().monitors
}

#[inline]
pub(crate) fn current_monitors_opt() -> Option<&'static MonitorState> {
    crate::runtime::rt_current_opt().map(|rt| &rt.monitors)
}

#[inline]
pub(crate) fn current_metrics() -> &'static MetricsState {
    &crate::runtime::rt_current().metrics
}

#[inline]
pub(crate) fn current_metrics_opt() -> Option<&'static MetricsState> {
    crate::runtime::rt_current_opt().map(|rt| &rt.metrics)
}

#[inline]
pub(crate) fn with_supervisor_roots<R>(f: impl FnOnce(&mut Vec<SupervisorPtr>) -> R) -> R {
    crate::runtime::rt_current().supervisor_roots.access(f)
}

#[inline]
pub(crate) fn admit_external_work() -> Result<ExternalIngressPermit, ()> {
    crate::shutdown::admit_external_work().map(ExternalIngressPermit)
}

#[inline]
pub(crate) fn publish_queue_entry(entry: SchedulerQueueEntry) {
    if crate::driver::active() {
        crate::driver::publish_queue_entry(entry);
        return;
    }
    crate::scheduler::publish_queue_entry(entry);
}

#[inline]
pub(crate) fn has_runnable_work() -> bool {
    crate::scheduler::has_runnable_work()
}

#[inline]
pub(crate) fn send_abandoned_by_shutdown() -> bool {
    crate::shutdown::hew_shutdown_phase() >= crate::shutdown::PHASE_DRAIN
}

#[inline]
pub(crate) fn shutdown_requested() -> bool {
    crate::scheduler::shutdown_requested()
}

#[inline]
pub(crate) fn actor_progress_available() -> bool {
    crate::scheduler::actor_progress_available()
}

#[inline]
pub(crate) fn spin_waiter(ready: impl FnMut() -> bool) -> bool {
    WAITER_SPIN.with(|budget| {
        let mut current = budget.get();
        let spun = crate::scheduler::spin_waiter(&mut current, ready);
        budget.set(current);
        spun
    })
}

#[inline]
pub(crate) fn publish_crash_cleanup_drain_to_signal_handler(active: bool) {
    crate::signal::set_crash_cleanup_drain_active(active);
}

#[inline]
pub(crate) fn local_route_slot() -> u16 {
    crate::runtime::rt_current_opt().map_or(0, |rt| rt.node.local_route_slot())
}

pub(crate) fn spawn_deferred_thread(
    name: &'static str,
    work: impl FnOnce() + Send + 'static,
) -> bool {
    if let Ok(handle) = std::thread::Builder::new().name(name.into()).spawn(work) {
        crate::lifetime::live_actors::push_deferred_teardown_thread(handle);
        true
    } else {
        false
    }
}

struct DeferredActorBatch(Vec<usize>);

// SAFETY: pointers in `DeferredActorBatch` point to live detached actor allocations
// whose exclusive cleanup ownership transferred to the background teardown thread.
unsafe impl Send for DeferredActorBatch {}

#[derive(Clone, Copy)]
struct DeferredActorFree(*mut crate::actor::HewActor);

// SAFETY: the deferred free thread observes the actor pointer only after the
// owning dispatch thread requests teardown and returns out of dispatch.
unsafe impl Send for DeferredActorFree {}

pub(crate) fn defer_actor_free(actors: Vec<*mut crate::actor::HewActor>) {
    if actors.is_empty() {
        return;
    }
    let batch = DeferredActorBatch(actors.into_iter().map(|ptr| ptr as usize).collect());
    if !spawn_deferred_thread("deferred-free", move || {
        for addr in batch.0 {
            let actor = addr as *mut crate::actor::HewActor;
            // SAFETY: caller transferred exclusive stopped-actor ownership after
            // dropping any supervisor reference.
            unsafe { crate::actor::hew_actor_free(actor) };
        }
    }) {
        eprintln!("hew: warning: failed to spawn deferred-free thread");
    }
}

pub(crate) fn defer_single_actor_free(actor: *mut crate::actor::HewActor) -> std::ffi::c_int {
    let deferred = DeferredActorFree(actor);
    let spawned = spawn_deferred_thread("deferred-actor-free", move || {
        let deferred = deferred;
        // SAFETY: the runtime still owns `actor`; the background thread retries
        // the same free once the current dispatch unwinds.
        let rc = unsafe { crate::actor::hew_actor_free(deferred.0) };
        if rc != 0 {
            eprintln!("hew: warning: deferred actor free failed with rc={rc}");
        }
    });
    if spawned {
        0
    } else {
        crate::set_last_error("hew_actor_free: failed to spawn deferred free thread");
        -1
    }
}

pub(crate) fn schedule_deferred_supervisor_stop(
    sup: *mut crate::supervisor::HewSupervisor,
    teardown: SupervisorTeardownLease,
    work: impl FnOnce(*mut crate::supervisor::HewSupervisor, SupervisorTeardownLease) + Send + 'static,
) -> bool {
    if crate::driver::active() {
        crate::driver::publish_supervisor_stop(crate::supervisor::QueuedSupervisorStop {
            supervisor: sup,
            root_unregistered: false,
            teardown,
        });
        return true;
    }
    let sup_addr = sup as usize;
    let spawned = spawn_deferred_thread("deferred-sup-stop", move || {
        work(sup_addr as *mut crate::supervisor::HewSupervisor, teardown);
    });
    if !spawned {
        eprintln!("hew: warning: failed to spawn deferred supervisor-stop thread");
    }
    spawned
}

pub(crate) fn output_drainer_running(drain_entry: fn()) -> bool {
    static DRAINER: OnceLock<bool> = OnceLock::new();
    if crate::runtime::rt_current_opt().is_none() {
        return false;
    }
    *DRAINER.get_or_init(|| {
        std::thread::Builder::new()
            .name("hew-output".into())
            .spawn(drain_entry)
            .is_ok()
    })
}

#[inline]
pub(crate) fn notify_output_drainer() {
    OUTPUT_WORK.notify_one();
}

#[inline]
pub(crate) fn wait_output_work<T>(guard: MutexGuard<'_, T>) -> MutexGuard<'_, T> {
    OUTPUT_WORK.wait_or_recover(guard)
}

#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "call sites are wired in subsequent runtime stages"
    )
)]
#[inline]
pub(crate) unsafe fn bounds_trap(code: std::ffi::c_int) -> ! {
    // SAFETY: the native trap bridge checks whether an actor recovery
    // context is active and returns only when no recovery context exists.
    unsafe { crate::supervisor::hew_trap_with_code(code) };
    std::process::abort();
}

#[inline]
pub(crate) unsafe fn fault_trap_bridge(code: std::ffi::c_int, reported: bool) {
    // SAFETY: forwarded caller contract.
    unsafe {
        crate::supervisor::trap_with_code(code, reported);
    }
}

pub(crate) fn drain_to_quiescence() -> i32 {
    if crate::driver::active() {
        crate::driver::run_to_quiescence();
    }
    crate::shutdown::hew_shutdown_initiate_implicit(0);
    let status = crate::shutdown::hew_shutdown_wait();
    crate::scheduler::hew_runtime_cleanup_after_main();
    status
}

pub(crate) const STACK_SOFT_RESERVE: usize = 256 * 1024;
pub(crate) const STACK_HARD_RESERVE: usize = 64 * 1024;

#[inline]
pub(crate) fn current_thread_stack_floor() -> Option<usize> {
    use crate::signal::stack;
    if !stack::is_registered() {
        stack::register_current_thread(stack::ThreadLabel::Other);
    }
    let bounds = stack::current();
    bounds.is_known().then_some(bounds.low)
}

#[inline]
pub(crate) fn init_root_crash_handling() {
    crate::signal::init_crash_handling();
}

#[inline]
pub(crate) fn current_live_actors() -> &'static crate::lifetime::live_actors::LiveActors {
    &crate::runtime::rt_current().live_actors
}

#[inline]
pub(crate) fn current_live_actors_opt() -> Option<&'static crate::lifetime::live_actors::LiveActors>
{
    crate::runtime::rt_current_opt().map(|rt| &rt.live_actors)
}

#[inline]
pub(crate) fn actor_runtime_matches(actor_id: u64, runtime_id: RuntimeId) -> bool {
    let Some(current) = crate::runtime::rt_current_id() else {
        return true;
    };
    if current == runtime_id {
        return true;
    }
    eprintln!(
        "hew-runtime: refused a send to actor {actor_id:#x} owned by runtime {} from runtime {} \
         (cross-runtime boundary; pointer not routed)",
        runtime_id.as_u64(),
        current.as_u64(),
    );
    false
}
