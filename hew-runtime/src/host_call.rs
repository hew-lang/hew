//! Wake-driven execution for generated, same-build asynchronous host exports.
//! Checked parameter carriers and value cleanup remain generated code's concern.

use std::ffi::c_void;
use std::sync::Arc;

use hew_cabi::host_error::{HewError, HostStatus};

use crate::coro_root::{poll_root, HewRootStart, RootExecution};
use crate::fault::HewFault;
use crate::runtime_handle::HewRuntime;
use crate::wake::scheduled::ReadyState;
use crate::wake::{HewWaker, OwnedWaker};

/// Compiler-generated completion over the exact retained input carriers.
pub type HewHostCallFinish = unsafe extern "C" fn(*mut c_void, i32);
/// Compiler-generated storage disposal after result transfer and completion.
pub type HewHostCallFree = unsafe extern "C" fn(*mut c_void);

#[derive(Debug)]
struct HostReadiness {
    state: ReadyState,
    host: OwnedWaker,
}

impl HostReadiness {
    fn notify(&self) {
        if self.state.notify() {
            self.host.wake();
        }
    }

    fn finish(&self, complete: bool) {
        if self.state.finish(complete) {
            self.host.wake();
        }
    }

    fn descriptor(owner: &Arc<Self>) -> HewWaker {
        unsafe extern "C" fn wake(context: *mut c_void) {
            // SAFETY: every descriptor holder retains this independent target.
            unsafe { &*context.cast::<HostReadiness>() }.notify();
        }
        unsafe extern "C" fn retain(context: *mut c_void) {
            // SAFETY: an existing descriptor reference keeps the allocation live.
            unsafe { Arc::increment_strong_count(context.cast::<HostReadiness>()) };
        }
        unsafe extern "C" fn release(context: *mut c_void) {
            // SAFETY: each retained descriptor transfers one reference here.
            unsafe { Arc::decrement_strong_count(context.cast::<HostReadiness>()) };
        }
        HewWaker {
            context: Arc::as_ptr(owner).cast_mut().cast(),
            wake,
            retain,
            release,
        }
    }
}

/// One host-thread invocation. Its readiness target, never its frame address,
/// may be retained by operations and notified from other runtime threads.
#[derive(Debug)]
pub struct HewHostCall {
    execution: RootExecution,
    readiness: Arc<HostReadiness>,
    runtime: *mut HewRuntime,
    frame: *mut c_void,
    arguments: *mut c_void,
    finish: Option<HewHostCallFinish>,
    free: Option<HewHostCallFree>,
    status: Option<i32>,
    taken: bool,
}

unsafe fn refuse(error: *mut *mut HewError, message: &str) -> i32 {
    // SAFETY: the generated boundary supplies one empty writable error slot.
    unsafe { *error = HewError::new(HostStatus::InvalidState, message).into_raw() };
    HostStatus::InvalidState as i32
}

/// Prepare an invocation before handing off any consuming input owners.
/// # Safety
/// A non-null runtime is an owned process handle. `waker` obeys its retained
/// callback contract; callbacks schedule a future poll and never re-enter it.
/// The caller supplies an empty writable error slot and must eventually start,
/// drain, take the result and release the returned invocation on this thread.
#[no_mangle]
pub unsafe extern "C" fn hew_host_call_new(
    runtime: *mut HewRuntime,
    waker: *const HewWaker,
    error: *mut *mut HewError,
) -> *mut HewHostCall {
    // SAFETY: the caller supplies an empty writable slot.
    unsafe { *error = std::ptr::null_mut() };
    if waker.is_null() || !crate::runtime_handle::retain_active_host_runtime(runtime) {
        // SAFETY: the writable error slot has not received another owner.
        unsafe {
            refuse(
                error,
                "asynchronous host calls require a live runtime and wake target",
            )
        };
        return std::ptr::null_mut();
    }
    let readiness = Arc::new(HostReadiness {
        state: ReadyState::idle(),
        // SAFETY: the caller retains the borrowed descriptor through preparation.
        host: unsafe { OwnedWaker::retain(&*waker) },
    });
    let descriptor = HostReadiness::descriptor(&readiness);
    // SAFETY: readiness owns the descriptor target; the call drains before drop.
    let execution = unsafe { RootExecution::new(&descriptor) };
    Box::into_raw(Box::new(HewHostCall {
        execution,
        readiness,
        runtime,
        frame: std::ptr::null_mut(),
        arguments: std::ptr::null_mut(),
        finish: None,
        free: None,
        status: None,
        taken: false,
    }))
}

/// Start the exact checked callable using persistent generated input storage.
/// # Safety
/// `call` is freshly prepared on this thread. The callbacks, argument carriers
/// and output slots have their exact generated layouts and remain live until
/// release. Start never unwinds; finish/free discharge only their stated owners.
#[no_mangle]
pub unsafe extern "C" fn hew_host_call_start(
    call: *mut HewHostCall,
    start: HewRootStart,
    arguments: *mut c_void,
    result: *mut c_void,
    fault: *mut *mut HewFault,
    finish: HewHostCallFinish,
    free: HewHostCallFree,
) {
    // SAFETY: the generated caller transfers exclusive invocation access.
    let call = unsafe { &mut *call };
    call.arguments = arguments;
    call.finish = Some(finish);
    call.free = Some(free);
    call.frame = call.execution.enter(|state| {
        // SAFETY: all carriers and slots are held in the generated allocation.
        unsafe { start(arguments, result, fault, state) }
    });
    if !call.execution.pending() {
        call.readiness.notify();
    }
}

/// Drive one consumed readiness notification; return zero pending or one ready.
/// # Safety
/// `call` is live, started and exclusively accessed on its owning host thread.
#[no_mangle]
pub unsafe extern "C" fn hew_host_call_poll(call: *mut HewHostCall) -> i32 {
    // SAFETY: the host serializes access to its invocation owner.
    let call = unsafe { &mut *call };
    if call.status.is_some() {
        return 1;
    }
    let ready = call.readiness.state.try_begin();
    let frame = &mut call.frame;
    let status = call.execution.enter(|state| {
        // SAFETY: the invocation owns this frame/state and all typed storage.
        unsafe { poll_root(frame, state, ready) }
    });
    let Some(status) = status else {
        if ready {
            call.readiness.finish(false);
        }
        return 0;
    };
    // SAFETY: terminal frame destruction has completed all language cleanup;
    // the generated callback now finishes the retained input carriers once.
    unsafe {
        call.finish.unwrap_or_else(|| std::process::abort())(call.arguments, status);
    }
    call.status = Some(status);
    call.readiness.finish(true);
    1
}

/// Request cancellation without blocking; late cancellation preserves success.
/// # Safety
/// `call` is live and exclusively accessed on its owning host thread.
#[no_mangle]
pub unsafe extern "C" fn hew_host_call_cancel(call: *mut HewHostCall) {
    // SAFETY: the host serializes access to its live invocation.
    let call = unsafe { &mut *call };
    call.execution.cancel();
}

/// Reserve the one terminal result transfer; refusal preserves every owner.
/// # Safety
/// `call` is live on its host thread; `error` is an empty writable owner slot.
#[no_mangle]
pub unsafe extern "C" fn hew_host_call_result_begin(
    call: *mut HewHostCall,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the generated wrapper supplies a writable slot and exclusive call.
    unsafe { *error = std::ptr::null_mut() };
    // SAFETY: the host serializes access to the live invocation.
    let call = unsafe { &mut *call };
    if call.status.is_none() {
        // SAFETY: no result or error has been transferred on this path.
        return unsafe { refuse(error, "host call result is not ready") };
    }
    if call.taken {
        // SAFETY: no new result or error has been transferred on this path.
        return unsafe { refuse(error, "host call result was already taken") };
    }
    call.taken = true;
    0
}

/// Borrow generated storage after a successful result reservation.
/// # Safety
/// `call` is the live exact typed call whose result has just been reserved.
#[no_mangle]
pub unsafe extern "C" fn hew_host_call_arguments(call: *const HewHostCall) -> *mut c_void {
    // SAFETY: the generated result wrapper holds the live invocation owner.
    unsafe { (*call).arguments }
}

/// Read the private checked outcome after a successful result reservation.
/// # Safety
/// `call` is live and has completed the root frame's terminal cleanup.
#[no_mangle]
pub unsafe extern "C" fn hew_host_call_status(call: *const HewHostCall) -> i32 {
    // SAFETY: the generated result wrapper has established terminal completion.
    unsafe { (*call).status.unwrap_or_else(|| std::process::abort()) }
}

/// Release only a completed call with its result taken; null is idempotent.
/// # Safety
/// `owner` and `error` are disjoint writable slots; a non-null call is owned on
/// this thread. Refusal leaves the call owner untouched and returns one error.
#[no_mangle]
pub unsafe extern "C" fn hew_host_call_release(
    owner: *mut *mut HewHostCall,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: both slots are live, writable and disjoint.
    unsafe { *error = std::ptr::null_mut() };
    // SAFETY: the host supplied a live, writable call owner slot.
    let pointer = unsafe { *owner };
    if pointer.is_null() {
        return 0;
    }
    // SAFETY: the host transfers exclusive access to this call for the check.
    let call = unsafe { &*pointer };
    if call.status.is_none() || !call.taken {
        // SAFETY: no owner slot has changed, so refusal preserves the call.
        return unsafe { refuse(error, "take the completed host call result before release") };
    }
    // SAFETY: terminal cleanup and result transfer have discharged all values.
    unsafe {
        *owner = std::ptr::null_mut();
        let call = Box::from_raw(pointer);
        call.free.unwrap_or_else(|| std::process::abort())(call.arguments);
        let runtime = call.runtime;
        drop(call);
        crate::runtime_handle::hew_runtime_release(runtime);
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::coro_state::{hew_coro_state_finish, hew_coro_state_waker, HewCoroState};
    use crate::wake::blocking::Readiness;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Mutex;

    struct Probe {
        late: Arc<Mutex<Option<OwnedWaker>>>,
        finished: Arc<AtomicUsize>,
        freed: Arc<AtomicUsize>,
    }

    unsafe extern "C" fn start(
        arguments: *mut c_void,
        _result: *mut c_void,
        _fault: *mut *mut HewFault,
        state: *mut HewCoroState,
    ) -> *mut c_void {
        // SAFETY: the test retains this exact argument allocation until free.
        let probe = unsafe { &*arguments.cast::<Probe>() };
        // SAFETY: this root invocation retains the descriptor while cloning it.
        let late = unsafe { OwnedWaker::retain(&*hew_coro_state_waker(state)) };
        *probe.late.lock().unwrap() = Some(late);
        // SAFETY: this synchronous unit body initializes no result or fault.
        unsafe { hew_coro_state_finish(state, 0) };
        std::ptr::null_mut()
    }

    unsafe extern "C" fn finish(arguments: *mut c_void, status: i32) {
        assert_eq!(status, 0);
        // SAFETY: terminal polling retains the argument allocation until release.
        let probe = unsafe { &*arguments.cast::<Probe>() };
        probe.finished.fetch_add(1, Ordering::Relaxed);
    }

    unsafe extern "C" fn free(arguments: *mut c_void) {
        // SAFETY: release transfers the sole, fully drained argument allocation.
        let probe = unsafe { Box::from_raw(arguments.cast::<Probe>()) };
        probe.freed.fetch_add(1, Ordering::Relaxed);
    }

    #[test]
    fn wake_during_poll_is_delivered_after_poll_and_terminal_wakes_are_suppressed() {
        let (host, waker) = Readiness::new();
        let readiness = Arc::new(HostReadiness {
            state: ReadyState::idle(),
            host: waker,
        });
        readiness.notify();
        assert!(host.take_ready());
        assert!(readiness.state.try_begin());
        let operation = readiness.clone();
        std::thread::spawn(move || {
            operation.notify();
            operation.notify();
        })
        .join()
        .unwrap();
        assert!(!host.take_ready());
        readiness.finish(false);
        assert!(host.take_ready());
        assert!(readiness.state.try_begin());
        let operation = readiness.clone();
        std::thread::spawn(move || operation.notify())
            .join()
            .unwrap();
        readiness.finish(true);
        readiness.notify();
        assert!(!host.take_ready());
    }

    #[test]
    fn late_operation_wake_retains_only_the_readiness_target_after_call_release() {
        let _runtime = crate::runtime_test_guard();
        let runtime = crate::runtime_handle::hew_runtime_new();
        let (ready, waker) = Readiness::new();
        let weak = Arc::downgrade(&ready);
        let late = Arc::new(Mutex::new(None));
        let finished = Arc::new(AtomicUsize::new(0));
        let freed = Arc::new(AtomicUsize::new(0));
        let probe = Box::into_raw(Box::new(Probe {
            late: late.clone(),
            finished: finished.clone(),
            freed: freed.clone(),
        }));
        let mut error = std::ptr::null_mut();
        // SAFETY: every owner is fresh, callbacks obey the exact root contract,
        // no result storage is needed, and completion precedes release.
        unsafe {
            let mut call = hew_host_call_new(runtime, waker.descriptor(), &raw mut error);
            assert!(!call.is_null() && error.is_null());
            hew_host_call_start(
                call,
                start,
                probe.cast(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                finish,
                free,
            );
            crate::runtime_handle::hew_runtime_release(runtime);
            assert!(crate::runtime_handle::retain_active_host_runtime(runtime));
            crate::runtime_handle::hew_runtime_release(runtime);
            assert!(ready.take_ready());
            assert_eq!(hew_host_call_poll(call), 1);
            assert_eq!(hew_host_call_poll(call), 1);
            assert_eq!(finished.load(Ordering::Relaxed), 1);
            assert_eq!(hew_host_call_result_begin(call, &raw mut error), 0);
            assert_eq!(hew_host_call_status(call), 0);
            hew_host_call_cancel(call);
            assert_eq!(hew_host_call_status(call), 0);
            assert_eq!(hew_host_call_release(&raw mut call, &raw mut error), 0);
            assert!(call.is_null() && error.is_null());
        }
        assert!(!crate::runtime_handle::retain_active_host_runtime(runtime));
        assert_eq!(freed.load(Ordering::Relaxed), 1);
        drop(ready);
        drop(waker);
        assert!(weak.upgrade().is_some());
        let late = late.lock().unwrap().take().unwrap();
        std::thread::spawn(move || {
            late.wake();
            drop(late);
        })
        .join()
        .unwrap();
        assert!(weak.upgrade().is_none());
    }
}
